function freq = countWeighted(X)

% Returns a frequency count of the elements in X(:, 1:end-1), weighted by
% the elements in X(:, end).
%
% TC 2012.4.11

% INPUT
%  X is an m x n matrix, where the first n - 1 columns of each row specify
%   a vector to be counted.

% EXAMPLE
% X = [.75 60 2; .5 61 4; 1 60 3; 2 60 7; 1.5 60 2; 1.33 60 4; 1.5 60 6;...
%   1.5 61 4; .5 62 8; 2 60 7];

m = size(X, 1);
n = size(X, 2);
Y = sortrows(X);
% Record the indices of Y where changes occur between row i and row i + 1
% in columns 1 through n - 1.
change_idx = [1; find(sum(diff(Y(:, 1:n - 1)), 2) ~= 0) + 1; m + 1];
nch = size(change_idx, 1) - 1;

% Use this to define freq.
freq = zeros(nch, n + 1);
for ich = 1:nch
  freq(ich, 1:n - 1) = Y(change_idx(ich), 1:n - 1);
  freq(ich, n) = sum(Y(change_idx(ich):change_idx(ich + 1) - 1, n));
end

% Include relative frequencies.
N = sum(freq(:, n));
if N > 0
  freq(:, n + 1) = freq(:, n)/N;
end

end
