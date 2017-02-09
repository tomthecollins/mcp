function [A, onOffs, tt] = appendPointSets(pointSets, varargin)

% Copyright Tom Collins 8/7/2014

% Given a cell of point sets and an optional vector containing the number
% of crotchet beats per bar in each point set, this function returns one
% point set in which all of the input point sets are appended. It also
% returns the minimal ontimes and maximal offtimes of each point set, and
% the time translations that have been applied to make sure the point sets
% remain distinguishable.

% INPUT
%  pointSets is a cell containing n x k matrices (possibly of different
%   dimensions) that represent a k-dimensional sets of n points.
%  beatsInBar is an optional vector argument, containing the number of
%   crotchet beats per bar in each point set.
%  jumpBars is an optional positive real, containing the number of bars to
%   put between adjacent point sets.

% EXAMPLE
% pointSets = {[-1 60 60 2; 4 61 60 1] [-1 48 52 .5; 6 59 60 8]...
%   [0 60 60 1; 2 61 60 1; 3 48 52 1; 6 59 60 4]};
% beatsInBars = [3 4 3];
% [A, onOffs, tt] = appendPointSets(pointSets, beatsInBars);
% plot(A(:, 1), A(:, 2), '.b')

onIdx = 1;
durIdx = 4;
% jumpBars = 5; % How many bars to put between point sets.
nps = size(pointSets, 2);
if nargin < 2
  beatsInBars = repmat(4, 1, nps);
else
  beatsInBars = varargin{1};
end
if nargin < 3
  jumpBars = 5;
else
  jumpBars = varargin{2};
end
% Collect the size of each input point set in order to preallocate A.
npts = zeros(nps, 1);
kdim = zeros(nps, 1);
for ips = 1:nps
  npts(ips) = size(pointSets{ips}, 1);
  % Record the dimension of the point set as well.
  kdim(ips) = size(pointSets{ips}, 2);
end

k = unique(kdim);
if size(k, 1) ~= 1
  warning('d:ream', ['You are appending point sets of dissimilar'...
    ' dimension. Beware of zero padding.']);
end
k = k(end);

A = zeros(sum(npts), k); % Big point set that will contain all point sets.
onOffs = zeros(nps, 2); % Record of minimal ontimes and maximal offtimes.
tt = zeros(nps, 1); % Record of the time shift applied to each point set.
% Assign values for the first point set.
row1 = 1;
nrow = npts(1);
A(row1:nrow, 1:kdim(1)) = pointSets{1};
onOffs(1, :) = [min(pointSets{1}(:, onIdx))...
  max(pointSets{1}(:, onIdx) + pointSets{1}(:, durIdx))];
row1 = nrow(1) + 1;
% Iterate for the second point set onwards.
for ips = 2:nps
  % Get the last offtime in previous point set.
  nrow = nrow + npts(ips);
  prevEnd = onOffs(ips - 1, 2); % Maximal offtime of previous point set.
  % Get to the next ontime m such that m modulo n = 0, where n is the
  % number of beats in a bar for the next point set, then jump by jumpBars.
  nextBgn = beatsInBars(ips)*...
    (floor(prevEnd/beatsInBars(ips)) + 1 + jumpBars);
  tt(ips) = nextBgn; % The time shift to apply.
  currT = [nextBgn zeros(1, kdim(ips) - 1)];
  currPs = pointSets{ips} + repmat(currT, npts(ips), 1); % Apply shift.
  A(row1:nrow, 1:kdim(ips)) = currPs; % Add it to A.
  % Record minimal ontime and maximal offtime.
  onOffs(ips, :) = [min(currPs(:, onIdx))...
    max(currPs(:, onIdx) + currPs(:, durIdx))];
  row1 = nrow + 1; % Get the first row index in A for the next point set.
end

end
