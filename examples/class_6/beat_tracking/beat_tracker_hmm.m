function results = beat_tracker_hmm(fpath, beat_var_dir)

% Tom Collins 11/8/2015.

% This is a simplified reencoding of Florian Krebs' beat-tracking
% algorithm. I found it helpful to fix some of the parameters and reduce
% the number of calls to external functions/methods. Further reductions
% could/should be made.

if nargin < 2
  beat_var_dir = fullfile('~', 'Dropbox', 'musicPredictionAppData',...
    'beatTrackingVariables');
end

% Get the file name and file type.
[~, fname, ftype] = fileparts(fpath);
if ~strcmp(ftype, '.wav')
  error('Input file is not of the type wav.');
else
  fprintf('Running the beat tracker on %s.\n', fname);
end

% General function parameters.
save_intermediary_results = 0;

% Detection function parameters.
params.nfft = 2048; % Window size (length of the FFT).
params.win = hann(params.nfft);
% Use a Hann window.
params.hopsize = 441; % 10 ms -> frame rate = 100.
% Import filterbank.
load(fullfile(beat_var_dir, 'filterbank82_sb.csv'));
params.filterbank = filterbank82_sb;
params.freq_bands = [0 230; 250 44100];
params.max_f = 230;
params.lambda = 10221; % Logarithmic amplitude.
params.mvavg_win = 100;
params.frame_length = 0.02;
% Call detection function.
[observations, fr] = detection_function(fpath, params.nfft,...
  params.hopsize, params.win, params.filterbank, params.freq_bands,...
  params.lambda, params.mvavg_win, params.frame_length);
if save_intermediary_results
  % Save observations to file.
  csvwrite(fullfile(results_dir, [fname ' observations.csv']),...
    observations);
end

% HMM parameters.
params.R = 2;
params.barGrid = 64;
params.M = 768;
params.N = 16;
params.rhythm2meter = [3 4; 4 4];
params.Meff = [576; 768];

% Import an example detection function (for now).
% observations2 = csvread(fullfile(beat_var_dir, 'det_func_example.csv'));
nFrames = size(observations, 1);

% Import HMM variables.
fprintf('Importing the HMM variables.\n');
obs_model_pts_r1 = csvread(fullfile(beat_var_dir, 'gmm_pts_r1.csv'));
obs_model_pdf_r1 = csvread(fullfile(beat_var_dir, 'gmm_pdf_r1.csv'));
obs_model_pts_r2 = csvread(fullfile(beat_var_dir, 'gmm_pts_r2.csv'));
obs_model_pdf_r2 = csvread(fullfile(beat_var_dir, 'gmm_pdf_r2.csv'));
initial_prob = csvread(fullfile(beat_var_dir, 'initial_prob.csv'));
trans_table = csvread(fullfile(beat_var_dir, 'trans_table.csv'));
trans_mtx = spconvert(trans_table);
state2obs_idx = csvread(fullfile(beat_var_dir, 'state2obs_idx.csv'));

% Calculate obs_lik.
fprintf('Calculating the observed likelihood.\n');
obs_lik = ones(params.R, params.barGrid, nFrames) * -1;
for imod = 1:params.R
  switch imod
    case 1
      obs_model_pts = obs_model_pts_r1;
      obs_model_pdf = obs_model_pdf_r1;
    case 2
      obs_model_pts = obs_model_pts_r2;
      obs_model_pdf = obs_model_pdf_r2;
  end
  % Number of points at which the distribution was sampled.
  npts = size(obs_model_pts, 1);
  % Number of bar positions for this value of r.
  npos = round(size(obs_model_pdf, 1)/npts^2);
  for ipos = 1:npos % Iterate over bar position.
    if mod(ipos, 20) == 0
      fprintf(['Calculating the observed likelihood for r = %d of %d,\n'...
        ' bar position %d of %d.\n'], imod, params.R, ipos, npos);
    end
    for iFrame = 1:nFrames % Iterate over frames.
      det_func_pair = observations(iFrame, :);
      % Find the indices of the values in the saved grid closest to this
      % detection function pair.
      [~, i_ind] = min(abs(obs_model_pts - det_func_pair(1)));
      [~, j_ind] = min(abs(obs_model_pts - det_func_pair(2)));
      % Get the probability at this grid location. Recall that
      % obs_model_pdf is a vectorised array of dimension
      % params.barGrid * npts^2. So the values for the ipos_th matrix will
      % be found in rows npts^2*(ipos - 1) + 1 to npts^2*ipos. Within these
      % rows, matrix data has been entered by column and then by row, so
      % the values for the j_ind_th column will be found in rows
      % npts*(j_ind - 1) + 1 to npts*j_ind, and the values for the i_ind_th
      % row will be found in rows 1 to npts.
      p = obs_model_pdf(npts^2*(ipos - 1) + npts*(j_ind - 1) + i_ind);
      obs_lik(imod, ipos, iFrame) = p;
    end
  end
end
if save_intermediary_results
  % Save obs_lik to file, converting it to a matrix first.
  obs_lik_matrix = zeros(params.R, npos*nFrames);
  for iFrame = 1:nFrames
    obs_lik_matrix(:, npos*(iFrame - 1) + 1:npos*iFrame) =...
      obs_lik(:, :, iFrame);
  end
  csvwrite(fullfile(results_dir, [fname ' obs_lik_matrix.csv']),...
    obs_lik_matrix);
end

% Do the decoding.
% Do not compute states that cannot be reached.
[row, col] = find(trans_mtx);
maxState = max([row; col]);
minState = min([row; col]);
nStates = maxState + 1 - minState;

delta = initial_prob;
valid_states = false(maxState, 1);
valid_states(unique(col)) = true;
delta(~valid_states) = 0;
delta = delta(minState:maxState);
A = trans_mtx(minState:maxState, minState:maxState);
% fprintf('Size of Psi = %.1f MB\n', nStates * nFrames * 2 / 10^6);
psi_mat = zeros(nStates, nFrames, 'uint16'); % 16 bit unsigned integer.
perc = round(0.1*nFrames);
i_row = 1:nStates;
j_col = 1:nStates;
ind = sub2ind([params.R, params.barGrid, nFrames],...
  state2obs_idx(minState:maxState, 1),...
  state2obs_idx(minState:maxState, 2), ones(nStates, 1));
ind_stepsize = params.barGrid * params.R;
O = zeros(nStates, 1);
validInds = ~isnan(ind); %
for iFrame = 1:nFrames
  % Progress string.
  if mod(iFrame, 1000) == 0
    fprintf('Working on frame %d of %d.\n', iFrame, nFrames);
  end
  % The variable delta is equal to the probability of the best sequence
  % ending in state j at time t, when observing y(1:t).
  % D is the matrix of probabilities of best sequences with state i at time
  % t-1 and state j at time t, when observing y(1:t).
  % Create a matrix that has the same value of delta for all entries with
  % the same state i (row). This is the same as repmat(delta, 1, col).
  D = sparse(i_row, j_col, delta(:), nStates, nStates);
  [delta_max, psi_mat(:, iFrame)] = max(D*A);
  % Compute likelihood p(yt|x1:t).
  % ind is shifted at each time frame until all frames are used.
  O(validInds) = obs_lik(ind(validInds));
  % Increase index to new time frame.
  ind = ind + ind_stepsize;
  delta_max = O .* delta_max';
  % normalize
  norm_const = sum(delta_max);
  delta = delta_max/norm_const;
  % if rem(iFrame, perc) == 0
  %   fprintf('.');
  % end
end
% Backtracing.
bestpath = zeros(nFrames, 1);
[m, bestpath(nFrames)] = max(delta);
maxIndex = find(delta == m);
bestpath(nFrames) = round(median(maxIndex));
for iFrame=nFrames-1:-1:1
  bestpath(iFrame) = psi_mat(bestpath(iFrame + 1), iFrame + 1);
end
if save_intermediary_results
  % Save hidden_state_sequence to file.
  csvwrite(fullfile(results_dir, [fname ' best_path.csv']), bestpath);
end
% Add state offset.
bestpath = bestpath + minState - 1;
hidden_state_sequence = bestpath;
if save_intermediary_results
  % Save hidden_state_sequence to file.
  csvwrite(fullfile(results_dir, [fname ' hidden_state_sequence.csv']),...
    hidden_state_sequence);
end
% Exporting a best path for testing purposes.
% csvwrite(fullfile(results_dir, '20150804',...
%   '01 Let Me Go bestpath_tom.csv'), bestpath);

% Decode state index into sub indices.
[m_path, n_path, r_path] =...
  ind2sub([params.M, params.N, params.R], hidden_state_sequence(:)');
if save_intermediary_results
  % Save m_path, n_path, and r_path to file.
  csvwrite(fullfile(results_dir, [fname ' m_path.csv']), m_path);
  csvwrite(fullfile(results_dir, [fname ' n_path.csv']), n_path);
  csvwrite(fullfile(results_dir, [fname ' r_path.csv']), r_path);
end
% Strip of silence state.
idx = true(length(r_path), 1);
% Compute beat times and bar positions of beats.
meter = zeros(2, length(r_path));
meter(:, idx) = params.rhythm2meter(r_path(idx), :)';
beats = find_beat_times_simple(params, m_path, r_path);
if ~isempty(n_path)
  tempo = meter(1, idx)' .* 60 .* n_path(idx)' ./ ...
    (params.Meff(r_path(idx)') * params.frame_length);
else
  tempo = 60 .* n_path(idx) / (params.M * params.frame_length);
end
results.beats = beats;
results.tempo = tempo;
results.meter = meter;
results.r_path = r_path;
results.hidden_state_sequence = hidden_state_sequence;

% Save the estimated beat locations to a CSV file.
% csvwrite(fullfile(results_dir, [fname '.txt']), beats);
% Save the results to file.
% save(fullfile(results_dir, [fname '.mat']), 'results');
if save_intermediary_results
  % Save tempo and meter too.
  csvwrite(fullfile(results_dir, [fname ' tempo.csv']), tempo);
  csvwrite(fullfile(results_dir, [fname ' meter.csv']), meter);
end

end
