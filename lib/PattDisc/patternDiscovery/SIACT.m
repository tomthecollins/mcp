function [SIACToutput, runtime, FRT] = SIACT(D, compactThresh,...
    cardinaThresh, regionType)

% Copyright 11/5/2011 Tom Collins

% This function takes a point-set representation (called a dataset) as its
% first argument. The rest of the arguments---compactThresh, cardinaThresh,
% and regionType---are parameters, with more information given below.

% The first step of SIACT is to run SIA. This produces a vector of structs
% called S1. Each struct contains a maximal translatable pattern (MTP).
% Each MTP is checked, by calling the function comapctSubpatterns, for
% consecutive subpatterns that have a cardinality (number of points)
% greater than or equal to the variable cardinaThresh, and a compactness
% (number of points divided by number of points in the region spanned by
% the pattern) greater than or equal to the variable compactnessThresh.
% Such subpatterns are included in the returned variable SIACToutput,
% another vector of structs.

% More details about SIACT can be found in Tom Collins, Jeremy Thurlow,
% Robin Laney, Alistair Willis, and Paul H. Garthwaite. (2010). A
% comparative evaluation of algorithms for discovering translational
% patterns in Baroque keyboard works. In J.S. Downie & R. Veltkamp (Eds.),
% Proceedings of the International Symposium on Music Information Retrieval
% (pp. 3-8), Utrecht: International Society for Music Information
% Retrieval.

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  compactThresh is a parameter in (0, 1], giving the minimum compactness a
%   pattern occurrence must have in order to be included in the output.
%  cardinaThresh is a positive integer parameter, giving the minimum number
%   points that compactness a pattern occurrences must have in order to be
%   included in the output.
%  regionType is a string equal to 'lexicographic' or 'convex hull',
%   indicating which definition of region should be used for calculating
%   the compactness of patterns.

% Run SIA.
tStart = tic;
[S1, ~, ~] = SIA(D);
dimension = size(D, 2);
N = size(S1, 2);
SIACToutput = repmat(struct('pattern', [], 'cardinality', [],...
    'compactness', [], 'region', [], 'span', [], 'regionType', [],...
    'vector', []), 1, N);
i = 1; % Increment over S1.
j = 1; % Increment over SIACToutput.
while i <= N
    % Check each MTP for compact subpatterns.
    S = compactSubpatterns(S1(i).pattern, D, dimension, compactThresh,...
        cardinaThresh, regionType);
    if isfield(S, 'pattern');
        % Include the generating vector of the MTP.
        [S(:).vector] = deal(S1(i).vector);
        % Append compact subpatterns to SIACT output.
        k = size(S,  2);
        SIACToutput(j:j + k - 1) = S;
        j=j+k;
    end
    i=i+1;
end
SIACToutput = SIACToutput(1:j-1);
runtime = toc(tStart);
FRT = runtime;

end
