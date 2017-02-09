function P2hist = P2getHistogram(P, D)

% Copyright Tom Collins 22/6/2013

% This function returns a histogram consisting of (ontime, similarity)
% pairs, indicating the similarity (in the interval [0, 1]) between P and
% the points in D beginning at a particular ontime.

% INPUT
%  P is a point set.
%  D is a point set. Typically P occurs in D, with one or more
%   exact/inexact occurrences.

% Apply P2 by Ukkonen et al. (2003).
[vectors, vecCount] = translatorsOfPartialPatternInDataset(P, D);
% Normalise.
lP = size(P, 1);
simP = vecCount/lP;

% Define all possible translations in ontime, and use this to put maximum
% match for each ontime into a match-time plot.
unqon = unique(vectors(:, 1));
nunq = size(unqon, 1);
simP2maxPerOn = zeros(nunq, 1);
for iunq = 1:nunq
  % Find rows of vectors corresponding to this unique ontime.
  rel_idx = vectors(:, 1) == unqon(iunq);
  simP2maxPerOn(iunq) = max(simP(rel_idx));
end

P2hist = [unqon simP2maxPerOn];

end
