function ratedOutput = rateOutput(S, D, tfPitch, projIdx2)

% 11/5/2011 Copyright Tom Collins

% This function takes a struct S as input, a dataset D, and a logical
% variable tfPitch, indicating if the struct contains information about a
% pattern that consists of pitch material. The pattern is rated (from about
% 1, meaning of low musical importance, to 10, meaning of high musical
% importance) using a perceptually validated model. More details about this
% model can be found in Tom Collins, Robin Laney, Alistair Willis, and Paul
% H. Garthwaite. (2011). Modeling pattern importance in Chopin's mazurkas.
% In Music Perception, 28(4), 387-414.

% INPUT
%  S is a vector of structs, containing fields for pattern and cardinality.
%  D is an n x k matrix representing a k-dimensional set of n points.
%  tfPitch is an optional argument indicating whether the projection of the
%   point set includes a dimension for pitch (thus making allowances for
%   transpositions in calculations).
%  projIdx2 is an optional argument indicating which columns of D to use to
%   calculate an empirical mass function. For example, the full point set
%   may have dimensions for ontime, pitch, duration, and staff number, but
%   one wants to exclude ontime and staff number from calculation of the
%   empirical mass function, in which case projIdx2 = [2 3].

dimension = size(D,2);
if nargin < 4
  projIdx2 = 2:dimension;
  dim2 = dimension;
  proj4exp = 1:dimension;
else
  dim2 = size(projIdx2, 2) + 1;
  proj4exp = unique([1 projIdx2]);
end

% The parameters are as follows:
alpha = 4.277867; % Constant term.
beta1 = 3.422478734; % Coefficient for compactness.
beta2 = -0.038536808; % Coefficient for normExpectedOccurrences.
beta3 = 0.651073171; % Coefficient for compressionRatio.
a = 73.5383283152; % Coefficient for normalising expectedOccurrences.
b = 0.02114878519; % Exponent for normalising expectedOccurrences.


cardD = size(D, 1);
empiricalMass = count(D(:, projIdx2), 'rows');
% Preallocation.
[S(:).translators] = deal([]);
[S(:).occurrences] = deal([]);
[S(:).coverage] = deal([]);
[S(:).compressionRatio] = deal([]);
[S(:).expectedOccurrences] = deal([]);
[S(:).normExpectedOccurrences] = deal([]);
[S(:).rating] = deal([]);
n = size(S,2);
parfor i = 1:n
  if mod(i, 100) == 0
    fprintf('Filtering pattern %d of %d.\n', i, n)
  end
  S(i).translators = translatorsOfPatternInDataset(S(i).pattern, D);
  S(i).occurrences = size(S(i).translators, 1);
  L = S(i).cardinality;
  unionOfPoints = zeros(L*S(i).occurrences, dimension);
  j = 1; % Increment over the unionOfPoints.
  while j <= S(i).occurrences
    unionOfPoints((L*(j - 1) + 1):(L*j), :) = S(i).pattern +...
      repmat(S(i).translators(j, :), L, 1);
    j=j+1;
  end
  unionOfPoints = unique(unionOfPoints, 'rows');
  S(i).coverage = size(unionOfPoints,1);
  S(i).compressionRatio = S(i).coverage/...
    (S(i).cardinality + S(i).occurrences - 1);
  expOcc = expectedOccurrences(...
    S(i).pattern(:, proj4exp), S(i).cardinality, cardD, dim2, S(i).span,...
    empiricalMass, tfPitch);
  % If a very large pattern is passed to the function expectedOccurrences,
  % the probabilistic calculations could zero-out, and a nan get returned.
  % This if condition catches this eventuality and sets it to zero.
  if isnan(expOcc)
    S(i).expectedOccurrences = 0;
  else
    S(i).expectedOccurrences = expOcc;
  end
  S(i).normExpectedOccurrences = a*S(i).expectedOccurrences^b;
  S(i).rating = alpha + beta1*S(i).compactness...
    + beta2*S(i).normExpectedOccurrences + beta3*S(i).compressionRatio;
end

% Removing supposedly repeated discoveries.
fprintf('Removing supposedly repeated discoveries.\n')
if ~isempty(S)
  A = [[S.compactness]' [S.expectedOccurrences]' [S.compressionRatio]'];
  [b, m] = unique(A, 'rows');
  S = S(m);
  
  % Sort by rating.
  fprintf('Sorting by rating.\n')
  a = [S.rating];
  [b, m] = sort(a,'descend');
  ratedOutput = S(m);
else
  ratedOutput = S;
end

end
