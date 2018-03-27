% Copyright Tom Collins 3/24/2018

% Create db for a bunch of audio files.

% Add a fast 2D peak finder.
addpath('./FastPeakFind');

% Paths to audio files.
audPaths = {...
  ...%'./../class_7/Labyrinth.wav'
  ...%'./../class_6/beat_tracking/Kendrick Lamar - HUMBLE.wav'
  './fragment_up.wav'
  './fragment_down.wav'
  ...
  };
naud = size(audPaths, 1);

% Spectrogram parameters.
nfft = 2048;
win = hann(nfft);
overlap = nfft/4; % 25% overlap between adjacent spectra.
nrows = 50; % Most of the spectrum (representing higher frequencies) is empty.

% Fingerprint parameters.
timeThresMin = 0.1;
timeThresMax = 1;
pidxThresMin = 0;
pidxThresMax = 5;

% Variable to hold all the fingerprints.
dbase = struct;
cumuSamp = 0;
for i=1:naud
  fprintf('Processing file %d of %d.\n', i, naud);
  % Import wav file.
  [sig, Fs] = audioread(audPaths{i});
  % Spectrogram and peak pick.
  [s, w, t] = spectrogram(sig(:, 1), win, overlap, nfft); % Just left channel.
  s = abs(s(1:50, :));
  % Show spectrogram.
  % close all; imagesc(-s); colormap 'gray';
  % Pick peaks.
  thres = quantile(s(:), 0.9);
  IJ = FastPeakFind(s, thres);
  I = IJ(2:2:end);
  J = IJ(1:2:end);
  % hold on; plot(J, I, 'r+'); hold off;
  
  % Create fingerprints.
  fp = []; % Record current fps just for plot purposes.
  npeak = size(I, 1);
  nfp = 0;
  for ii = 1:npeak
    ind1 = [I(ii) J(ii)];
    jj = ii + 1;
    while jj <= npeak
      ind2 = [I(jj) J(jj)];
      time_diff = (ind2(2) - ind1(2))*overlap/Fs;
      pitch_diff = abs(ind2(1) - ind1(1));
      % Decide whether to make a fingerprint.
      if time_diff > timeThresMin && time_diff < timeThresMax &&...
          pitch_diff > pidxThresMin && pitch_diff < pidxThresMax
        % Make a fingerprint. Start with the hash.
        time_hash = num2str(time_diff);
        time_hash = time_hash(3:4);
        pitch_hash = num2str(pitch_diff);
        hash = ['fd' time_hash pitch_hash];
        if ~isfield(dbase, hash)
          % Make a new entry in dbase.
          dbase.(hash) = struct('timestamp', cumuSamp + ind1(2)*overlap,...
            'idpiece', audPaths{i});
        else
          % Add to an existing entry in dbase.
          dbase.(hash) = [dbase.(hash);...
            struct('timestamp', cumuSamp + ind1(2)*overlap,...
            'idpiece', audPaths{i})];
        end
        nfp=nfp+1;
        fp = [fp; [ind1 ind2]];
        % fprintf(...
        %   'Made a fp for ind1 = [%d, %d] and ind2 = [%d, %d].\n',...
        %   ind1(1), ind1(2), ind2(1), ind2(2));
      end
      % Move on to the next value of ii if we are too far away in time.
      if time_diff >= timeThresMax
        jj = npeak;
      end
      jj=jj+1;
    end
  end
  % Plot some of the fingerprints.
  hold on;
  for ifp = 1:nfp
    pause(0.05);
    line([fp(ifp, 2) fp(ifp, 4)], [fp(ifp, 1) fp(ifp, 3)]);
  end
  hold off;
  
  % Update cumuSamp.
  cumuSamp = cumuSamp + size(sig, 1);
end

% Save dbase.
save('dbase', 'dbase');