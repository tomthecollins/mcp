function x = expectedOccurrences(P, cardP, cardD, dimension, span,...
    empiricalMass, tfPitch)

% 9/5/2011 Tom Collins

% This function takes a pattern P of known cardinality cardP and the
% cardinality cardD of the dataset in which it was discovered. The
% dimension of P is also known, as is the span of P in D. A precalculated
% empirical mass function is used to determine the number of expected
% occurrences of the pattern P (or one of its permissible translations) in
% the dataset D. The last argument tfPitch = 1 if P includes a dimension
% for pitch, and tfPitch = 0 otherwise.

% Example pattern and empirical mass.
% P = [3 71 1/2; 7/2 70 1/2];
% cardP = 2;
% cardD = 9;
% dimension = 3;
% empiricalMass = [62 1/2 1 1/16; 63 1/2 1 1/16; 65 1 1 1/16;...
%     68 1/2 2 1/8; 70 1/2 2 1/8; 70 3/2 2 1/8; 71 1/2 3 3/16;...
%     73 1 2 1/16; 75 1/2 2 1/16];
% tfPitch = 1;


% Probability
p = empiricalProbability(P, cardP, dimension, empiricalMass, tfPitch);
% Number of places in which the pattern (or one of its permissible
% translations) can occur.
K = nchoosek(span, cardP) + (cardD - span)*nchoosek(span - 1, cardP - 1);
x = K*p;