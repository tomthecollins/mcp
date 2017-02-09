function h = fpgethistogram2(D, P, fpType, plotData, timeTolerance,...
  n1, n2, d)

% Copyright Tom Collins 9/1/2013

% This function is an implementation of the fingerprinter of Arzt, B??ck,
% and Widmer (2012), based on the description in that paper. It is less
% stable and slower than their implementation. Any discrepancies between
% results of the two implementations should be attributed to errors on my
% part.

% Two aspects not yet implemented are timeZone and noteZone. For timeZone
% (default 10000), if the time interval between two consectutive notes is
% greater than 10 s, the note pair will be skipped. For noteZone, (default
% 24), if the pitch interval between two consecutive MIDI notes is greater
% than two octaves, the note pair will be skipped.

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  P is an l x k matrix representing an occurrence of a pattern.
%  fpType is a string, one of 'normal', 'pitchindependent',
%   'tempoindependent', and 'tempoandpitchindependent'.
%  plotData is an optional argument taking the value one if the matching
%   time stamps and resulting histogram should be plotted, and zero
%   otherwise.
%  time tolerance is an optional real in [0, 1] (default .85, and .75 in
%   the Arzt et al. (2012) paper). Part of a token is the time difference
%   t2 - t1, or the time difference ratio r = (t3 - t2)/(t2 - t1). When
%   lookup is performed with timeTolerance < 1, some inexactness in this
%   differene (ratio) is permitted: a value of .75 for instance permits a
%   difference of .25 either way in the case of time difference, and 25%
%   either way in the case of ratio.
%  n1 and n2 give the number of pairs per note (default 10, 5 in paper).
%   Pairs (and then triplets) will be constructed between the current note
%   and the next 10 notes.
%  d is the minimum time distance (default 10 ms, 50 ms in paper). If the
%   time interval between two consecutive notes is less than this amount,
%   the note pair will be skipped;

% EXAMPLE INPUT
% D = [-1,59;0,48;0,64;1,55;1,57;1,60;1.5,65;2,52;2,57;2,59;2,66;2.5,68;...
%   3,59;3,62;3,64;3,69;3.5,67;4,45;4,66;5,56;5,61;6,58;6,61;6,63;7,52;...
%   7,58;7.5,64;8,46;8,53;8,62;9,55;9,57;9,60;10,45;10,52;10,63;10.5,64;...
%   11,55;11,57;11,61;11,65;11.5,66;12,44;12,51;12,67;13,55;13,60;14,43;...
%   14,50;14.5,67;15,55;15,59;15,60;15.5,66;16,42;16,49;16,66;17,53;...
%   17,60;17,65;18,55;18,59;18,61;18.5,64;19,53;19,60;19,64;...
%   19.3333333333333,63;19.8333333333333,62;20,49;20,62;21,53;21,58;...
%   21,63;22,47;22.5,58;23,52;23,56;23,58;23,59;23.5,65;24,48;24,64;...
%   25,52;25,57;25,61;25.5,62;26,46;26,61;26.3333333333333,62;...
%   26.6666666666667,61;27,55;27,56;27,58;27,62;27.3333333333333,63;...
%   27.6666666666667,60;28,45;28,62;29,57;29,59;30,56;30,61;...
%   30.3333333333333,68;30.8333333333333,67;31,59;31,61;31,65;31,67;...
%   31.3333333333333,66;31.6666666666667,65;32,48;32,64;33,55;33,57;...
%   33,60;33.5,65;34,52;34,57;34,59;34,66;34.25,69;34.5,70;...
%   34.75,71;35,59;35,62;35,64;35,70;35.1666666666667,69;...
%   35.3333333333333,66;35.5,67;35.6666666666667,64;35.8333333333333,67;...
%   36,45;36,66;37,56;37,61;38,58;38,61;38,63;39,45;39,52;39.5,64;40,46;...
%   40,53;40,62;41,55;41,57;41,60;41,61;41.5,62;42,45;42,52;42,63;...
%   42.75,64;43,55;43,57;43,61;43,65;43.5,66;44,44;44,51;44,67;45,55;...
%   45,60;46,43;46,50;46.5,67;47,55;47,59;47,62;47.5,66;48,42;48,49;...
%   48,65;48,66;48,67;49,56;49,60;49,62;49,65;50,41;50,48;50.5,66;51,57;...
%   51,60;51,64;51,67;51.5,67;52,40;52,47;52,68;53,58;53,60;53,63;...
%   53.5,67;54,50;54,54;54,59;54,65];
% P = [104,62;105,55;105,58;105.5,63;106,50;106,57;106,64;106.5,66;...
%   107,57;107,62;107,67;107.5,65;108,43;108,64];
% fpType = 'pitchindependent';

if isstruct(fpType)
  similarParam = fpType;
  fpType = similarParam.fpType;
  plotData = similarParam.plotData;
  timeTolerance = similarParam.timeTolerance;
  n1 = similarParam.n1;
  n2 = similarParam.n2;
  d = similarParam.d;
else
  if nargin < 8
    d = .01;
  end
  if nargin < 7
    n2 = 10;
  end
  if nargin < 6
    n1 = 10;
  end
  if nargin < 5
    timeTolerance = .85;
  end
  if nargin < 4
    plotData = 0;
  end
end

% Setting some parameters.
ID = []; % String identifier for piece (not really necessary here).
switch fpType
  case 'normal'
    trans_invar = 0;
    use_ratio = 0;
  case 'pitchindependent'
    trans_invar = 1;
    use_ratio = 0;
  case 'tempoindependent'
    trans_invar = 0;
    use_ratio = 1;
  case 'tempoandpitchindependent'
    trans_invar = 1;
    use_ratio = 1;
end
% res is the histogram resolution (default 1000 ms, 1000 ms in paper). This
% is the bin size used for constructing the histogram.
res = 1;
% Remove duplicates (default true, not used in paper). When performing
% transposition-invariant search, it is possible for duplicate fingerprints
% to be generated. This parameter controls whether they should be removed.
removeDuplicates = 1;

% Re-define the query to begin at zero.
P = P - repmat([P(1, 1) zeros(1, size(P, 2) - 1)], size(P, 1), 1);

% Create the fingerprints.
[X_T, X_C] = symbolicFingerprint(D, ID, n1, n2, d, trans_invar,...
  use_ratio, removeDuplicates);
[Y_T, Y_C] = symbolicFingerprint(P, ID, n1, n2, d, trans_invar,...
  use_ratio, removeDuplicates);

% Calculate the histogram.
h = matchScoreHistogram(X_T, X_C, Y_T, Y_C, res, timeTolerance,...
  use_ratio, plotData);

end
