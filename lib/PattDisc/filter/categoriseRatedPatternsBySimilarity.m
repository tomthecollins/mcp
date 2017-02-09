function [S_out, categories, s, S_in] =...
  categoriseRatedPatternsBySimilarity(S_in, rating_field, simMtx,...
  similarThresh, similarFunc, similarParam)

% Copyright Tom Collins 28/4/2013

% This function categorises patterns contained in S_in, according to
% similarity. It is assumed that an order can be imposed on S_in, given by
% values in the argument rating_field. A similarity matrix for S_in can be
% provided, or left empty, in which case parts of the matrix are calculated
% dynamically. 

% INPUT
%  S_in is a vector of structs containing repeated patterns.
%  rating_field is a string indicating which field of each struct in S
%   should be used to order the repeated patterns.
%  simMtx is an optional m x m matrix containing the similarity of S_in(i)
%   and S_in(j) in element (i, j).
%  similarThresh is a value in [0, 1). If the similarity of the current
%   highest-rated pattern S_in(i) and some other pattern S_in(j) is greater
%   than this threshold, then S_in(j) will be categorised as an instance of
%   the exemplar S_in(i). Otherwise S_in(j) may become an exemplar in a
%   subsequent step.
%  similarFunc is a string indicating which function should be used for
%   calculating the symbolic music similarity, either 'cardinality score'
%   or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent',
%   'tempoindependent', or 'tempoandpitchindependent', see fpgethistogram2
%   for details).

if nargin < 6
  similarParam = [];
end

if ~isempty(S_in) && ~isempty(rating_field)
  % Sort by rating field, in case not sorted already.
  a = [S_in.(rating_field)];
  [~, m] = sort(a, 'descend');
  S_in = S_in(m);
end

if ~isempty(simMtx) % Has the similarity matrix been calculated already?
  s = simMtx;
else
  % If not, we will calculate parts of it dynamically, rather than
  % calculating it exhaustively, as some elements will never be used for
  % certain categorisation tracks.
  s = [];
  % s = scoreMatrix(S_in, [], [], [], similarFunc, similarParam);
end

nPatt = size(S_in, 2);
categories = repmat(struct([]), 1, nPatt);
rel_idx = ones(1, nPatt); % Tracks remaining uncategorized patterns.
icat = 1; % Increment to create categories.
while icat <= nPatt && sum(rel_idx) > 0
  n_idx = sum(rel_idx);
  if n_idx == 1
    % Only one pattern left. Put it in its own category and break loop.
    rel_idx1 = find(rel_idx, 1, 'first');
    categories(icat).index = rel_idx1;
    categories(icat).members = rel_idx1;
    categories(icat).similarityRatings = 1;
    rel_idx = []; % Update rel_idx.
  else
    % More than one pattern left. Find any similar patterns.
    rel_idx1 = find(rel_idx, 1, 'first');
    curr_row = nan(1, nPatt);
    if ~isempty(s)
      curr_row(find(rel_idx)) = s(rel_idx1, find(rel_idx));
    else
      % Here we calculate parts of the score matrix dynamically.
      S_mini = []; % Cancel to avoid error from previous assignments.
      S_mini.pattern = S_in(rel_idx1).pattern;
      S_mini.cardinality = S_in(rel_idx1).cardinality;
      f_rel_idx = find(rel_idx); % Turn logical into integer indices.
      curr_row(f_rel_idx(1)) = 1; % Pattern must be the same as itself.
      uf_curr_row = ones(n_idx, 1);
      for i_idx = 2:n_idx
          S_mini(i_idx).pattern = S_in(f_rel_idx(i_idx)).pattern;
          S_mini(i_idx).cardinality = S_in(f_rel_idx(i_idx)).cardinality;
      end
      % S_mini(2:n_idx) = S_in(f_rel_idx(2:n_idx));
      printProgressString = 0;
      % Define a progress string.
      fprintf('Category %d, pattern 1 of %d.\n', icat, n_idx)
      parfor i_idx = 2:n_idx
        s2by2 = scoreMatrix(S_mini([1 i_idx]), [], [], [], similarFunc,...
          similarParam, printProgressString);
        uf_curr_row(i_idx) = s2by2(1, 2);
      end
      curr_row(f_rel_idx) = uf_curr_row;
      % Old, unparallelised 
      % for i_idx = 2:n_idx
      %   % Create a mini vector of pattern structs to pass to scoreMatrix.
      %   S_mini(2).pattern = S_in(f_rel_idx(i_idx)).pattern;
      %   S_mini(2).cardinality = S_in(f_rel_idx(i_idx)).cardinality;
      %   printProgressString = 0;
      %   s2by2 = scoreMatrix(S_mini, [], [], [], similarFunc,...
      %     similarParam, printProgressString);
      %   curr_row(f_rel_idx(i_idx)) = s2by2(1, 2);
      % end
    end
    categories(icat).index = rel_idx1;
    curr_idx = curr_row >= similarThresh;
    categories(icat).members = find(curr_idx);
    categories(icat).similarityRatings = curr_row(curr_idx);
    rel_idx = rel_idx - curr_idx; % Update rel_idx.
  end
  icat = icat + 1;
end
ncat = icat - 1;
categories = categories(1:ncat);

% Define S_out.
S_out = repmat(struct([]), 1, ncat);
for icat = 1:ncat
  S = S_in(categories(icat).index);
  S.categoryIndex = categories(icat).index;
  S.categoryMembers = S_in(categories(icat).members);
  S.categoryMembersIndex = categories(icat).members;
  S.categorySimilarityRatings = categories(icat).similarityRatings;
  if icat == 1
    S_out = S;
  else
    S_out(icat) = S;
  end
end

end
