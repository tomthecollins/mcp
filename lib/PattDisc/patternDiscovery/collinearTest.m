function tf = collinearTest(X)

% 17/5/2011 Copyright Tom Collins

% This function takes row vectors in a matrix X as its first argument, and
% it determines whether the points expressed by the rows are collinear to
% within the standard tolerance, returning 1 if this is the case, and 0
% otherwise.

% INPUT
%  X is an m x n matrix.

% EXAMPLE INPUT
% X = [1 8; 4 17; 5 20+1e-20];

% Calculate the difference between consecutive rows of X.
n = size(X,1);
Y = zeros(n-1, size(X,2));
i = 1; % Increment over X.
while i < n
    Y(i,:) = X(i+1,:) - X(i,:);
    i=i+1;
end

% Calculate the rank of Y. If it is only rank 1 then the points in X are
% collinear, as rank is the number of linearly independent rows (or
% columns).
tf = rank(Y) <= 1;

end