function lS = levelSet(P, Dc, lvl, similarFunc, similarParam,...
  excludeIntra, excludeInter)

% Copyright Tom Collins 10/11/2014

% Given a point set consisting of (onset, pitch) pairs P, which represents
% a pattern occurrence, and a cell of pieces Dc this function determines
% how often the pattern occurrence or something similar occurs in each of
% the pieces in Dc. The frequency of occurrence is expressed as the number
% of times that the fingerprint histogram exceeds a specifiable similarity
% level, lvl. This is returned as the first value in a pair. The second
% value is the maximum possible number of times that the similarity level
% could have been exceeded.

% INPUT
%  P is an n x 2 matrix representing a set of (ontime, pitch) pairs.
%  Dc is either: a struct consisting of a string specifying the path and
%   name of an already-calculated fingerprint database to use as the look
%   up, and the final ontime of the final piece in that concatenated point
%   set; a cell consisting of n x 2 matrices, each of which represents a
%   piece as set of (onset, pitch) pairs.
%  lvl is a value between 0 and 1. It specifies the similarity level that
%   the fingerprint histogram must exceed in order to be considered an
%   occurrence.
%  similarFunc is a string indicating which function should be used for
%   calculating the similarity of each fingerprint to the database, either
%   'cardinality score' or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent', 'tempoindependent', or
%   'tempoandpitchindependent', see fpgethistogram2 for details).
%  excludeIntra is a nonnegative integer, k, indicating an index for one of
%   the pieces contained in the database. When the histogram comes back
%   from the database, each row contains a reference to the index of the
%   piece where matches were found. For k > 0, matches from the kth piece
%   will be excluded. This enables control over whether matches from within
%   the same piece are counted. For k = 0 here and for the next paramter,
%   all matches are included.
%  excludeInter is a nonnegative integer, k, indicating an index for one of
%   the pieces contained in the database. When the histogram comes back
%   from the database, each row contains a reference to the index of the
%   piece where matches were found. For k > 0, only matches from the kth
%   piece will be included.

% Assign default parameters in the event none are provided.
if nargin < 4
  similarFunc = 'normalised matching score';
end
if nargin < 5
  if strcmp(similarFunc, 'cardinality score')
    similarParam = 0;
  else
    % similarParam = 'normal';
    similarParam = 'tempoandpitchindependent';
  end
end
if nargin < 6
  excludeIntra = 0;
end
if nargin < 7
  excludeInter = 0;
end
% If using normalised matching score, check which implementation has been
% defined.
if strcmp(similarFunc, 'normalised matching score')
  if exist('fpgethistogram', 'file') == 3
    h_nsm = @fpgethistogram;
  else
    h_nsm = @fpgethistogram2;
  end
end
% If using normalised matching score, check whether fpmatch is defined. If
% not, we will have to use the slower version of this function.
if strcmp(similarFunc, 'normalised matching score')
  if exist('fpmatch', 'file') == 3
    useSlow = 0;
  else
    useSlow = 1;
  end
end
if nargin < 3
  lvl = .5;
end

if useSlow
  szLevel = 0;
  szSet = 0;
  npiece = size(Dc, 2);
  for ipiece = 1:npiece
    % Print progress string.
    % fprintf('Running on piece %d of %d.\n', ipiece, npiece);
    if ~isempty(P)
      if strcmp(similarFunc, 'normalised matching score')
        sim_hist = h_nsm(Dc{ipiece}, P, similarParam);
        sim_hist2 = h_nsm(P, P, similarParam);
        maxscore = max(sim_hist2(:, 2));
        if ~isempty(sim_hist)
          sim_hist = [sim_hist(:, 1) sqrt(sim_hist(:, 2)/maxscore)];
        end
        % plot(sim_hist(:, 1), sim_hist(:, 2));
        n = size(Dc{ipiece}, 1);
        A = zeros(ceil(Dc{ipiece}(n, 1)), 1);
        pos_idx = sim_hist(:, 1) >= 0;
        sim_hist = sim_hist(pos_idx, :);
        % Enter the match scores in the self-similarity matrix.
        A(sim_hist(:, 1) + 1) = sim_hist(:, 2);
        % plot(A);
        currSzLevel = sum((A >= lvl) ~= 0);
        currSzSet = size(A, 1);
        szLevel = szLevel + currSzLevel;
        szSet = szSet + currSzSet;
      end
    end
  end
  
  lS = [szLevel szSet];
  
else
  % See if it is retrieved.
  H_P = sortrows(fpmatch(Dc.dbName, similarParam, P));
  temp_hist = h_nsm(P, P, similarParam);
  maxscore = max(temp_hist(:, 2));
  if ~isempty(H_P)
    H_P = [H_P(:, 1:2) sqrt(H_P(:, 3)/maxscore)];
  end
  % B_P = zeros(ceil(Dc.finalOntime), 1);
  % Exclude intra-opus matches and get statistics. Then exclude inter-opus
  % matches and get statistics too.
  if excludeIntra > 0 && excludeInter > 0
    relIdx1 = H_P(:, 1) ~= excludeIntra;
    relIdx2 = H_P(:, 1) == excludeInter;
    H_Q = H_P(relIdx1, :);
    H_R = H_P(relIdx2, :);
    % Prevent empty matrices causing shifts in the indices of lS.
    if isempty(H_Q)
      H_Q = zeros(1, 3);
    end
    if isempty(H_R)
      H_R = zeros(1, 3);
    end
    lS = [sum(H_Q(:, 3) >= lvl) ceil(Dc.finalOntime) max(H_Q(:, 3))...
      sum(H_R(:, 3) >= lvl) ceil(Dc.pieceFinalOntime) max(H_R(:, 3))];
  % Exclude intra-opus matches if excludeIntra > 0.
  elseif excludeIntra > 0
    relIdx = H_P(:, 1) ~= excludeIntra;
    H_Q = H_P(relIdx, :);
    % Prevent empty matrices causing shifts in the indices of lS.
    if isempty(H_Q)
      H_Q = zeros(1, 3);
    end
    lS = [sum(H_Q(:, 3) >= lvl) ceil(Dc.finalOntime) max(H_Q(:, 3))];
  % Exclude inter-opus matches if excludeInter > 0.
  elseif excludeInter > 0
    relIdx = H_P(:, 1) == excludeInter;
    H_R = H_P(relIdx, :);
    % Prevent empty matrices causing shifts in the indices of lS.
    if isempty(H_R)
      H_R = zeros(1, 3);
    end
    lS = [sum(H_R(:, 3) >= lvl) ceil(Dc.pieceFinalOntime) max(H_R(:, 3))];
  else
    % Prevent empty matrices causing shifts in the indices of lS.
    if isempty(H_P)
      H_P = zeros(1, 3);
    end
    lS = [sum(H_P(:, 3) >= lvl) ceil(Dc.finalOntime) max(H_P(:, 3))];
  end
  
end

end
