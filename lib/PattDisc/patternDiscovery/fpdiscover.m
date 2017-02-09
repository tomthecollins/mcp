function A = fpdiscover(D, dbpath, win, hop, tfPlot, similarFunc,...
  similarParam)

% Copyright Tom Collins 7/3/2014

% Given a point set consisting of (onset, pitch) pairs D, this function
% determines sets of points belonging to the time windows
% [0 win), [hop, win + hop), [2*hop, win + 2*hop),...
% and defines a fingerprint Xi for each time window.

% INPUT
%  D is an n x 2 matrix representing a set of (onset, pitch) pairs.
%  dppath is a string specifying the path and name of where the database of
%   fingerprints for the whole piece should be stored.
%  win is a window length in seconds.
%  hop is a hop size in seconds.
%  tfPlot takes the value one if an image of the output matrix should be
%   plotted, and zero otherwise.
%  similarFunc is a string indicating which function should be used for
%   calculating the similarity of each fingerprint to the database, either
%   'cardinality score' or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent', 'tempoindependent', or
%   'tempoandpitchindependent', see fpgethistogram2 for details).

% Assign default parameters in the event none are provided.
if nargin < 7
  similarFunc = 'normalised matching score';
end
if nargin < 8
  if strcmp(similarFunc, 'cardinality score')
    similarParam = 0;
  else
    % similarParam = 'normal';
    similarParam = 'tempoandpitchindependent';
  end
end
% If using normalised matching score, check which implementation has been
% defined.
if strcmp(similarFunc, 'normalised matching score')
  if exist('fpgethistogram') == 3
    h_nsm = @fpgethistogram;
  else
    h_nsm = @fpgethistogram2;
  end
end
if nargin < 6
  tfPlot = 1;
end
if nargin < 5
  hop = 1;
end
if nargin < 4
  win = 4;
end

% Create time windows and sets of points belonging to each time window.
% Also get the fingerprint histogram for each time window.
T = max(D(:, 1));
nwin = floor(T/hop + 1);
win_fp = cell(nwin, 2);
A = zeros(nwin, ceil(T));
progress = round((.1:.1:1)*nwin);
for iwin = 1:nwin
  
  % Print a progress message.
  [~, progIdx] = ismember(iwin, progress);
  if progIdx
    if progIdx == 10
      fprintf(['Stop looking at kitten photos, it''s time to get back'...
        ' to work!\n'])
    else
      fprintf('Calculation of fingerprints %d%% done.\n', 10*progIdx)
    end
  end
  
  % Put the time window and relevant members of D in a row of win_fp.
  t0 = hop*(iwin - 1);
  t1 = t0 + win;
  win_fp{iwin, 1} = [t0 t1];
  rel_idx = (D(:, 1) >= t0) + (D(:, 1) < t1) == 2;
  win_fp{iwin, 2} = D(rel_idx, :);
  
  % Now get the fingerprint histogram.
  P = win_fp{iwin, 2};
  if ~isempty(P)
    if strcmp(similarFunc, 'normalised matching score')
      sim_hist = fpmatch(dbpath, similarParam, P);
      sim_hist2 = h_nsm(P, P, similarParam);
      maxscore = max(sim_hist2(:, 2));
      if ~isempty(sim_hist)
        sim_hist = [sim_hist(:, 1) sim_hist(:, 2)/maxscore];
      end
      % Histogram resolution is set by default to 1 sec, and only nonzero
      % match scores are returned. Remove any negative times from the
      % histogram.
      pos_idx = sim_hist(:, 1) >= 0;
      sim_hist = sim_hist(pos_idx, :);
      % Enter the match scores in the self-similarity matrix.
      A(iwin, sim_hist(:, 1) + 1) = sim_hist(:, 2);
    elseif strcmp(similarFunc, 'cardinality score')
      % sim_hist = P2gethistogram(P, D);
    end
  end
  
end

if tfPlot
  imagesc(A);
  colormap('bone')
  axis square
end

end
