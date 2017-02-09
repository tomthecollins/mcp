function h = matchScoreHistogram(X_T, X_C, Y_T, Y_C, res, timeTolerance,...
  use_ratio, plotData)

% Copyright Tom Collins 9/1/2013

% Given two collections of symbolic fingerprints {X_T, X_C} and {Y_T, Y_C},
% where T stands for tokens and C is the associated cell array containing
% IDs, time stamps, and time differences, this function finds the matching
% tokens. If plotData = 1, it will scatter plot the time stamps of the
% matching tokens, and time regions of high similarity appear approximately
% as diagonal lines (Arzt, Bock, & Widmer, 2012). An affine transformation
% is applied to the matching tokens (converting diagonal lines into
% straight lines), and they are binned in time windows according to res to
% produce a histogram. Again, if plotData = 1, the histogram will be
% plotted. The maximum value in the histogram corresponds to the time point
% in the music data for X where the music data for Y is most similar.

% The affine transformation allows for timescale-invariant repetition, and
% the symbolic fingerprint may be transposition invariant.

% INPUT
%  X_T is an m x 4 matrix of tokens.
%  X_C is a corresponding m x 3 cell, containing query or piece ID, time
%   stamp, and a time difference.
%  Y_T is an n x 4 matrix of tokens.
%  Y_C is a corresponding n x 3 cell, containing query or piece ID, time
%   stamp, and a time difference.
%  res is the resolution of binning in seconds.
%  timeTolerance is a constant in (0, 1]. Part of a token is the time
%   difference ratio r = (t3 - t2)/(t2 - t1). When lookup is performed,
%   some inexactness in this ratio is permitted, as set by timeTolerance.
%   A value of .75 permits 25% either way, for example.
%  use_ratio is a logical, indicating whether the ratio of the difference
%   between (notes 2 and 3) and (notes 1 and 2) should be used, or just the
%   difference between notes 1 and 2.
%  plotData takes the value one if the time stamps and resulting histogram
%   should be plotted, and zero otherwise.

% EXAMPLE INPUT
% params = patterns2012Globals;
% query_path = fullfile(params.musicDatasetsRoot,...
%   'mutantBeethovenOp2No2Mvt3', 'Repeated patterns', 'query',...
%   'CSV datasets', 'query.csv');
% X_all = csvread(query_path);
% X = unique(X_all(:, 1:2), 'rows');
% piece_path = fullfile(params.musicDatasetsRoot,...
%   'mutantBeethovenOp2No2Mvt3', 'CSV datasets',...
%   'mutantBeethovenOp2No2Mvt3.csv');
% Y_all = csvread(piece_path);
% Y = unique(Y_all(:, 1:2), 'rows');
% ID_X = 'queryBeethovenOp2No2Mvt3';
% n1 = 5; % Controls number of points to look ahead of point 1.
% n2 = 5; % Controls number of points to look ahead of point 2.
% d = .05; % Points must be at least d seconds in the future.
% trans_invar = 0;
% [X_T, X_C] = symbolicFingerprint(X, ID_X, n1, n2, d, trans_invar);
% ID_Y = 'mutantBeethovenOp2No2Mvt3';
% tic
% [Y_T, Y_C] = symbolicFingerprint(Y, ID_Y, n1, n2, d, trans_invar);
% toc
% res = 1; % Resolution of binning in seconds.
% plotData = 1;

if nargin < 8
  plotData = 0;
end

% Determine which of X_T and Y_T has fewer rows: this will be the query.
% Relabel it as X.
nx = size(X_T, 1);
ny = size(Y_T, 1);
if ny < nx
  % Relabel X as Y and Y as X.
  Z_T = X_T;
  Z_C = X_C;
  nz = nx;
  X_T = Y_T;
  X_C = Y_C;
  nx = ny;
  Y_T = Z_T;
  Y_C = Z_C;
  ny = nz;
end

% Find matching tokens. Store i in X_idx and j in Y_idx, where there is a
% match between the ith row of X_T and the jth row of Y_T.

% tic
X_idx = zeros(ny, 1);
Y_idx = zeros(ny, 1);
jx = 1; % Increment to populate X_idx and Y_idx.
for ix = 1:nx
  if timeTolerance ~= 1
    if use_ratio
      loc = findrowsLastRatioWithinC(X_T(ix, :), Y_T, timeTolerance);
    else
      loc = findrowsLastDiffnWithinC(X_T(ix, :), Y_T, timeTolerance);
    end
  else
    loc = findrows(X_T(ix, :), Y_T);
  end
  nloc = size(loc, 1);
  kx = jx + nloc - 1;
  X_idx(jx:kx, :) = ix;
  Y_idx(jx:kx, :) = loc;
  jx = kx + 1;
end
X_idx = X_idx(1:jx - 1);
Y_idx = Y_idx(1:jx - 1);
% toc

% Time stamps of matching tokens.
x_ts = cell2mat(X_C(X_idx, 2));
y_ts = cell2mat(Y_C(Y_idx, 2));
if plotData
  figure
  plot(y_ts, x_ts, '.')
end
% xlim([-5 200])

if use_ratio
  % Apply affine transformation, beginning with calculation of time
  % differences.
  x_td = cell2mat(X_C(X_idx, 3));
  y_td = cell2mat(Y_C(Y_idx, 3));
  r = y_td./x_td;
  s = y_ts - x_ts.*r;
else
  s = y_ts - x_ts;
end
edges = floor(min(s)):res:ceil(max(s));

% Return edges as well as the frequency count.
h = histc(s, edges);
if ~isempty(h)
  sizh = size(h);
  if sizh(1) > sizh(2)
    h = [edges' h];
  else
    h = [edges' h'];
  end
  % Return non-zero entries of the histogram.
  h = h(h(:, 2) ~= 0, :);
end
if plotData
  figure
  hist(s, edges);
end

end