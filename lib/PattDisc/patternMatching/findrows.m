function loc = findrows(query_row, mtx)

% Copyright Tom Collins 19/2/2013

% This utility function returns the locations of all rows in the variable
% mtx (a matrix) that are equal to the variable query_row (a vector with
% the same number of columns as mtx).

% INPUT
%  query_row is an m-element vector.
%  mtx is an m x n matrix.

% EXAMPLE INPUT
% query_row = [1 2 4];
% mtx = [-2 0 0; 1 2 5; 1 2 4; -3 -1 0; 1 2 4; 3 2 3];

mn = size(mtx);
loc = find(sum(repmat(query_row, mn(1), 1) == mtx, 2) == mn(2));

end
