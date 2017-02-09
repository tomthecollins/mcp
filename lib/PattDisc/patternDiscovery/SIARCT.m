function [SIARCToutput, runtime, FRT] = SIARCT(D, r, compactThresh,...
    cardinaThresh, regionType, SIARoutput, SIARruntime, SIAR_FRT, quick)

% Copyright Tom Collins 20/8/2014

% Given a dataset D, this function returns compact subsets of maximal
% translatable patterns (MTP) that occur in D, and for which a conjugate
% generating vector lies on or within the first r superdiagonals of the
% similarity array for D.

% Could improve the function compactSubpatterns.m by returning indices.
% 28/1/2013 It is possible to adapt the final loop for parallel processing,
% as suggested by the comments. FRT code will not work in this instance.
% 1/4/2013 Added optional arguments for passing the output of SIAR, if for
%  instance SIAR is being run as well in a comparative evaluation.

% It is assumed that D is in lexicographic order. Otherwise
% D = sortrows(D);
% can be used to achieve this. Alternatively,
% D = unique(D, 'rows');
% will also remove duplicate datapoints.

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  r is a positive integer between 1 and n - 1, giving the number of
%   superdiagonals of the similarity array for D that will be used.
%  compactThresh is a parameter in (0, 1], giving the minimum compactness a
%   pattern occurrence must have in order to be included in the output.
%  cardinaThresh is a positive integer parameter, giving the minimum number
%   points that compactness a pattern occurrences must have in order to be
%   included in the output.
%  regionType is a string equal to 'lexicographic' or 'convex hull',
%   indicating which definition of region should be used for calculating
%   the compactness of patterns.
%  SIARoutput is an optional argument, containing the output of the SIAR
%   algorithm (vector of structs).
%  SIARruntime is an optional argument, giving the runtime in seconds of
%   the SIAR algorithm.
%  SIAR_FRT is an optional argument, giving the fifth return time in
%   seconds of the SIAR algorithm.
%  quick is an optional logical argument (set to one by default). It will
%   call a quick verison of the function in the default case, but this
%   version is sensitive to even very slight differences between decimal
%   values (look out for tuplets). The slow version is more robust to these
%   differences (down to 5 decimal places).

% EXAMPLE INPUT
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% r = 1;
% compactThresh = 2/3;
% cardinaThresh = 5;
% regionType = 'lexicographic';

if nargin < 10
  quick = 1;
end

if nargin < 6
  % Need to run SIAR, it has not been run already.
  [SIARoutput, SIARruntime, SIAR_FRT] = SIAR(D, r, quick);
end

% SIAR has been run. Now just need to apply the compactness trawler.
d = size(D,2);
tStart = tic;
nM = size(SIARoutput, 2);
SIARCTcell = cell(1, nM);
% fifthMeasure = 1; % Logical for measuring fifth return time (FRT).
% running_card = 0; % Keep track of cardinality of output.
% CT_FRT = 0; % Defaults to zero in case running_card is not exceeded.
% for i = 1:nM
parfor i = 1:nM
  % Measuring the 5th return time is not possible in a parfor loop,
  % so it is estimated at the end of the loop.
  % if fifthMeasure && running_card >= 5
  %     % Measure the 5th return time.
  %     CT_FRT = toc(tStart);
  %     fifthMeasure = 0;
  % end
  P = SIARoutput(i).pattern;
  if ~isempty(P)
    % Check the MTP for compact subpatterns.
    S = compactSubpatterns(P, D, d, compactThresh, cardinaThresh,...
      regionType);
    SIARCTcell{i} = S;
    % Measuring running_card is not possible in a parfor loop.
    % if fifthMeasure
    %   running_card = running_card + size(S, 2);
    % end
  else
    % Display a warning about rounding errors, as the pattern should not
    % be empty.
    warning('d:ream', 'Empty pattern %d. Check for rounding error.', i)
  end
end
% Remove empty entries from SIARCTcell, and return a vector of structs.
j = 1; % Increment to populate SIARCToutput.
for iM = 1:nM
  S = SIARCTcell{iM};
  if isfield(S, 'pattern');
    k = size(S, 2);
    SIARCToutput(j:j + k - 1) = S;
    j=j+k;
  end
end
% Measuring the 5th return time is not possible in a parfor loop.
% if fifthMeasure
%   CT_FRT = toc(tStart);
% end
runtime = SIARruntime + toc(tStart);
CT_FRT = 5*runtime/nM;
FRT = SIAR_FRT + CT_FRT;

end
