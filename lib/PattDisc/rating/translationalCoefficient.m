function tc = translationalCoefficient(D)

% Copyright Tom Collins 3/6/2015

% The TC can be used to measure the amount of randomness in a
% multidimensional point set in the range [0, 1], with values near one
% indicating more random than values near zero.

% This function calculates the number of MTPs M in a point set D of size n.
% (As SIA returns all MTPs, M could also be referred to as the size of SIA
% output). The maximum value M can take is n(n - 1)/2, corresponding to an
% upper triangle of all unique difference vectors. The minimum value M can
% take is n - 1, corresponding to equally spaced collinear points (musical
% manifestations include an isochronous tone, scales, and various types of
% chord). The translational coefficient (TC) of a point set D converts M to
% the range [0, 1] (for purposes of standardised comparison between point
% sets) thus: TC(D) = [M - (n - 1)]/[.5*n(n - 1) - (n - 1)], which
% simplifies to 2[M - n + 1]/[(n - 1)(n - 2)], where n > 2.

% INPUT
%  D is a multidimension point set.

% EXAMPLE
% D = [0 60; 1 59; 3 61; 6 58; 10 62; 15 57; 21 63; 28 56; 36 64; 45 56];
% plot(D(:, 1), D(:, 2), '.');
% TC_D = translationalCoefficient(D);
% E = [0 60; 1 59; 2 61; 3 58; 4 62; 5 57; 6 63; 7 56; 8 64; 9 56];
% hold on
% plot(E(:, 1), E(:, 2), 'or');
% hold off
% TC_E = translationalCoefficient(E);
% TC_D should be larger than TC_E, because the x-values in D do not conform
% to a structure that enables D to be expressed in terms of translational
% patterns, whereas the x-values in E do conform to a translational
% structure. Therefore, D is 'more random' than E.

n = size(D, 1);
if n > 2
  k = size(D, 2);
  N = n*(n - 1)/2;
  % Calculate the similarity matrix, but leave it as a vector.
  V = zeros(N, k+1);
  L = 1;
  for i = 1:n-1
    for j = i+1:n
      V(L,:) = [rounddec(D(j, :) - D(i, :), 5) i];
      L=L+1;
    end
  end
  % Get the unique entries.
  U = unique(V(:, 1:k), 'rows');
  M = size(U, 1);
  tc = 2*(M - n + 1)/((n - 1)*(n - 2));
  % Check that my algebra was good!
  % tc(2) = (M - (n - 1))/(n*(n - 1)/2 - (n - 1));
else
  % Not enough points in the set to calculate TC.
  error('Not enough points in the set to calculate TC.');
end

end
