function [vectors, vecCount] = translatorsOfPartialPatternInDataset(P, D)

% 29/3/2013 Copyright Tom Collins

% This function takes a pattern P and a dataset D as its arguments.
% Generally it will be the case that P is a subset of D, and the function
% translatorsOfPartialPatternInDataset is being used to find other
% occurrences (full or partial translations) of P in D. This is an
% implemented solution to P2 as described by Ukkonen, Lemstrom, and
% Makinen (2003).

% It is assumed that P and D are in lexicographic order. Otherwise
% D = sortrows(D);
% can be used to achieve this. Alternatively,
% D = unique(D, 'rows');
% will also remove duplicate datapoints.

% INPUT
%  P is a k-dimensional set of l points.
%  D is a k-dimensional set of n points.

% EXAMPLE INPUT
% P = [0 0; 0 3; 1 2];
% D = [2 2; 2 4; 3 4; 3 5; 6 4; 6 5; 6 7; 7 6; 8 4; 9 2; 9 5; 10 3; 10 4];
% D = [2 2; 2 4; 3 4; 3 5; 6 4; 6 5; 6 7; 7 6; 8 4; 9 2; 9 5; 10 3;...
%   10 4; 11 3; 11 4; 12 4; 13 2];
% % Or:
% P = [13 6; 15 7; 15 8; 16 6; 16 7];
% D = [1 3; 1 5; 3 4; 4 4; 5 5; 6 4; 6 5; 9 4; 9 5];
% plot(D(:, 1), D(:, 2), '.k')
% hold on
% plot(P(:, 1), P(:, 2), '.b')
% hold off
% xlim([-1 17])
% ylim([-1 10])

m = size(P, 1);
n = size(D, 1);
N = m*n;
k = size(D, 2);
vectors = zeros(N, k);
vecCount = zeros(N, 1);
sortVec = [1:k -(k + 1)]; % How to sort F.
J = ones(1, m); % Incrementing m traversals of D.
s = 1; % Increment to populate vectors and vectorCount.
c = 1; % Increment over corresponding entry for vectorCount.
% Define checking difference vector.
f = D(1, :) - P(1, :);
% Define check-end difference vector.
checkendVec = D(n, :) - P(1, :);
checkend = lexLessOrEqual(f, checkendVec, k);
% Define priority queue F.
F = [repmat(D(1, :), m, 1) - P (1:m)'];
while s <= N && (ischar(checkend) || checkend == 1)
  % If the increment over last traversal equals n, there cannot possibly be
  % more partial translations.
  [~, min_idx] = sortrows(F, sortVec);
  min_idx = min_idx(1);
  fprime = F(min_idx, 1:k);
  % Update simultaneous traversals of D. If one or more of the traversals
  % has reached the end of D, then we have to go to the next lowest element
  % of Fs.
  J(min_idx) = J(min_idx) + 1;
  if J(min_idx) <= n
    F(min_idx, 1:k) = D(J(min_idx), :) - P(min_idx, :);
  else
    F(min_idx, 1:k) = Inf;
  end
  if max(abs(f - fprime)) < 1e-5 % Test equality with tolerance for error.
    c = c + 1;
  else
    vectors(s, :) = f;
    vecCount(s) = c;
    f = fprime;
    % Help check when the last possible translation vector has is reached.
    checkend = lexLessOrEqual(f, checkendVec, k);
    c = 1;
    s = s + 1;
  end
end
vectors = vectors(1:s - 1, :);
vecCount = vecCount(1:s - 1);
  
end
