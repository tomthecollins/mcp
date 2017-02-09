function ssls = fpSlidingLevelSet(D, pieceStr, winSize, hopSize,...
  noteWise, topVoiceOnly, dbNames, similarParam, finalOntimes, lvl)

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
%  winSize is a positive real defining the duration of each time window to
%   excerpt from D.
%  hopSize is a positive real defining how far along to move along for each
%   successive segment from D.
%  noteWise is a logical, taking the value one if windows and hops are
%   defined in terms of numbers of notes, and taking the value zero if
%   windows and hops are defined in terms of ontimes.
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

similarFunc = 'normalised matching score';
mdb = size(dbNames, 2);
Dc = struct('dbName', [], 'finalOntime', []);
for idb = 1:mdb
  Dc(idb).dbName = dbNames{idb};
  Dc(idb).finalOntime = finalOntimes(idb);
end

n = size(D, 1);
D1st = D(1, 1);
Dlst = D(n, 1);
if noteWise
  nhop = floor(n/hopSize);
else
  nhop = floor(Dlst/hopSize);
end

% First make ssls.
ssls = repmat(struct('pieceStr', [], 'segOn', [],'segOff', [],...
  'segD', [], 'superlevelSets', {}), 1, nhop);
for ihop = 1:nhop
  % Define the ith segment from D.
  if noteWise
    segOn = hopSize*(ihop - 1) + 1;
    segOff = segOn + winSize - 1;
    segD = D(segOn:min([segOff n]), :);
  else
    if ihop == 1 && D1st < 0
      segOn = D1st;
      segOff = winSize;
    else
      segOn = hopSize*(ihop - 1);
      segOff = segOn + winSize;
    end
    relIdx = (D(:, 1) >= segOn) + (D(:, 1) < segOff) == 2;
    segD = D(relIdx, :);
    if topVoiceOnly
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
for idb = 1:mdb % Iterate over each fingerprint database.
  for ihop = 1:nhop % Iterate over segments in each piece.
    if mod(ihop, 20) == 0
      % Print a progress string.
      fprintf('Analysing db %d with %dth excerpt of %d from piece %s.\n',...
        idb, ihop, nhop, pieceStr);
    end
    % Calculate the level set for this query segD.
    lvls_cell{idb, ihop} = levelSet(ssls(ihop).segD, Dc(idb), lvl,...
      similarFunc, similarParam);
  end
end

% Transfer the values from lvls_cell to ssls.
for idb = 1:mdb
  for ihop = 1:nhop
    ssls(ihop).superlevelSets{idb} = lvls_cell{idb, ihop};
  end
end

end
