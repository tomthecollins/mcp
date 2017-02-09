function loc = findrowsLastRatioWithinC(query_row, mtx, c)

% Copyright Tom Collins 19/2/2013

% This utility function returns the locations of all rows in the variable
% mtx (a matrix) that are similar to the variable query_row (a vector with
% the same number of columns n as mtx). The first n - 1 entries of
% query_row are tested for equality, and the fourth entry must be within
% 100*(1 - c)% of the last column.

% INPUT
%  query_row is an m-element vector.
%  mtx is an m x n matrix.
%  c is a constant in (0, 1]. Part of a token is the time difference ratio
%   r = (t3 - t2)/(t2 - t1). When lookup is performed, some inexactness in
%   this ratio is permitted, as set by timeTolerance. A value of .75
%   permits 25% either way, for example.

% EXAMPLE INPUT
% query_row = [1 2 4 7];
% mtx = [-2 0 0 6; 1 2 5 2; 1 2 4 7.1; -3 -1 0 1; 1 2 4 7; 3 2 3 2];
% c = .75;

mn = size(mtx);
loc = find(sum([sum(repmat(query_row(1:mn(2)-1), mn(1), 1) ==...
  mtx(:, 1:mn(2)-1), 2) == mn(2) - 1 ...
  abs(mtx(:, mn(2)) - repmat(query_row(mn(2)), mn(1), 1))./mtx(:, mn(2))...
  <= 1 - c], 2) == 2);

end
