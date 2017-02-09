function ssls = fpSlidingLevelSet(D, pieceStr, winSize, hopSize,...
  noteWise, topVoiceOnly, dbNames, similarParam, finalOntimes, lvl,...
  excludeIntra, excludeInter)

% Copyright Tom Collins 14/11/2014

% Given a point set D consisting of (onset, pitch) pairs for a piece,
% some parameters that control iteration over time windows of that piece,
% at least one fingerprint database name, a fingerprint type, and the final
% ontime of pieces in that database, this function performs fingerprint
% analyses for segments of D. It returns an array of structs, where each
% struct contains an ontime, an offtime, the segment (points) in that
% time window, and the level set numerator and denominator for each
% database queried.

% INPUT
%  D is an n x 2 matrix representing a set of (ontime, pitch) pairs.
%  pieceStr is a string to identify the composer and name of the piece
%   represented by the point set D.
%  winSize is a positive real. If noteWise = 1, then winSize defines the
%   number of notes to take when defining a query. If noteWise = 0, then
%   winSize defines the duration of each time window to excerpt from D as a
%   query.
%  hopSize is a positive real defining how far along to move along for each
%   successive segment from D. This is defined irrespective of noteWise.
%  noteWise is a logical, taking the value one if windows are defined in
%   terms of numbers of notes, and taking the value zero windows are
%   defined in terms of ontimes.
%  topVoice is a logical, taking the value one if only the highest pitch at
%   each ontime should be included in the query, and the value zero if all
%   points should be included.
%  dbNames is a 1 x m cell of strings. Each string refers to a fingerprint
%   database that will be queried with each segment from the piece.
%  similarParam is a string argument specifying the fingerprint type
%   ('normal', 'pitchindependent', 'tempoindependent', or
%   'tempoandpitchindependent').
%  finalOntimes is a vector of positive real. If the pieces in the ith
%   database were all concatenated, the ith finalOntime would be the final
%   ontime of the final piece in that database.
%  lvl is a real in (0, 1). It is the cut off used for calculating the
%   cardinality of the superlevel set.
%  excludeIntra is a nonnegative integer, k, indicating an index for one of
%   the pieces contained in the database. When the histogram comes back
%   from the database, each row contains a reference to the index of the
%   piece where matches were found. For k > 0, matches from the kth piece
%   will be excluded. This enables control over whether matches from within
%   the same piece are counted. For k = 0 here and for the next paramter,
%   all matches are included.
%  excludeInter is a nonnegative integer, k, indicating an index for one of
%   the pieces contained in the database. When the histogram comes back
%   from the database, each row contains a reference to the index of the
%   piece where matches were found. For k > 0, only matches from the kth
%   piece will be included.

if nargin < 12
  excludeInter = 0;
end
if nargin < 11
  excludeIntra = 0;
end
similarFunc = 'normalised matching score';
mdb = size(dbNames, 2);
Dc = struct('dbName', [], 'finalOntime', []);
for idb = 1:mdb
  Dc(idb).dbName = dbNames{idb};
  Dc(idb).finalOntime = finalOntimes(idb);
  Dc(idb).pieceFinalOntime = D(end, 1);
end

n = size(D, 1);
D1st = D(1, 1);
Dlst = D(n, 1);
nhop = floor(Dlst/hopSize);

% First make ssls.
ssls = repmat(struct('pieceStr', [], 'segOn', [],'segOff', [],...
  'segD', [], 'superlevelSets', {}), 1, nhop);
for ihop = 1:nhop
  % Define the ith segment from D.
  if ihop == 1 && D1st < 0
    segOn = D1st;
  else
    segOn = hopSize*(ihop - 1);
  end
  if noteWise % Segment to be defined in terms of number of notes.
    relIdx = (D(:, 1) >= segOn);
    segD = D(relIdx, :);
    if topVoiceOnly % Segment consists of highest MNN at each ontime.
      segD = sortrows(segD, [1 -2]);
      [~, unqOnIdx] = unique(segD(:, 1));
      segD = segD(unqOnIdx, :);
    end
    segD = segD(1:min(winSize, size(segD, 1)), :);
    segOff = segD(end, 1);
  else % Segment to be defined in terms of time window.
    segOff = segOn + winSize;
    relIdx = (D(:, 1) >= segOn) + (D(:, 1) < segOff) == 2;
    segD = D(relIdx, :);
    if topVoiceOnly % Segment consists of highest MNN at each ontime.
      segD = sortrows(segD, [1 -2]);
      [~, unqOnIdx] = unique(segD(:, 1));
      segD = segD(unqOnIdx, :);
    end
  end
  ssls(ihop).pieceStr = pieceStr;
  ssls(ihop).segOn = segOn;
  ssls(ihop).segOff = segOff;
  ssls(ihop).segD = segD;
end

% Calculate the level set for this query segD.
% lvls = levelSet(segD, Dc(idb), lvl, similarFunc, similarParam);
% ssls(ihop).superlevelSets{idb} = lvls;

lvls_cell = cell(mdb, nhop);
parfor idb = 1:mdb % Iterate over each fingerprint database.
  for ihop = 1:nhop % Iterate over segments in each piece.
    if mod(ihop, 20) == 0
      % Print a progress string.
      fprintf('Analysing db %d with %dth excerpt of %d from piece %s.\n',...
        idb, ihop, nhop, pieceStr);
    end
    % Calculate the level set for this query segD.
    lvls_cell{idb, ihop} = levelSet(ssls(ihop).segD, Dc(idb), lvl,...
      similarFunc, similarParam, excludeIntra, excludeInter);
  end
end

% Transfer the values from lvls_cell to ssls.
for idb = 1:mdb
  for ihop = 1:nhop
    ssls(ihop).superlevelSets{idb} = lvls_cell{idb, ihop};
  end
end

end
