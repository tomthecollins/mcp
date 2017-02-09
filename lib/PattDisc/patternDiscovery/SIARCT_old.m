function [SIARCToutput, runtime, FRT] = SIARCT(D, r, compactThresh,...
    cardinaThresh, regionType, SIARoutput, SIARruntime, SIAR_FRT)

% Copyright Tom Collins 29/6/2011

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

% EXAMPLE INPUT
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% r = 1;
% compactThresh = 2/3;
% cardinaThresh = 5;
% regionType = 'lexicographic';

if nargin < 6
  % Need to run SIAR, it has not been run already.
  tStart = tic;
  n = size(D,  1);
  d = size(D, 2);
  if r >= n
    r = n - 1;
  end
  N = n*(n - 1)/2;
  
  % Calculate the reduced similarity matrix, but leave it as a vector.
  V = zeros(N, d + 1);
  L = 1;
  for i = 1:r
    for j = i+1:n
      V(L, :) = [D(j, :) - D(j - i, :) j - i];
      L=L+1;
    end
  end
  V = V(1:L - 1, :);
  V = sortrows(V);
  
  % Parse V and define a new set of datapoints each time a new difference
  % vector is encountered. Store the results as a struct, W.
  W = repmat(struct('vector', [], 'datapoints',[]), 1, L - 1);
  i = 1; % Increment over V.
  j = 1; % Increment over W.
  W(j).vector = V(i,  1:d);
  W(j).datapoints = D(V(i,d + 1), :);
  % W(j).indices = V(i,d + 1);
  for i = 2:L - 1
    % Did difference between datapoints result in the same vector?
    if V(i, 1:d) == V(i - 1, 1:d)
      % Yes: include datapoint in existing set.
      W(j).datapoints = [W(j).datapoints; D(V(i, d + 1), :)];
      % W(j).indices = [W(j).indices; V(i, d + 1)];
    else
      % No: define a new set of datapoints.
      W(j + 1).vector = V(i, 1:d);
      W(j + 1).datapoints = D(V(i, d + 1), :);
      % W(j + 1).indices = V(i,d + 1);
      j=j+1;
    end
  end
  W = W(1:j);
  
  % Parse W and run SIA on an element of W if it contains more than one
  % datapoint, recording the results in one list L.
  L = zeros(N, d);
  s = 1; % Increment over L.
  t = 1; % Increment over W.
  T = j; % Length of W.
  while t <= T
    nWt = size(W(t).datapoints, 1);
    if nWt > 1
      for i = 1:nWt
        for j = i + 1:nWt
          L(s, :) = W(t).datapoints(j, :) - W(t).datapoints(i, :);
          s = s+1;
        end
      end
    end
    t=t+1;
  end
  L = L(1:s - 1, :);
  
  % The more numerous the vector in L, the more likely it is to correspond
  % to a salient MTP. So we count the vectors in L and then sort them by
  % number of occurrences. The number of occurrences is not retained.
  M = count(L, 'rows');
  M = [M(:, d + 1) M(:, 1:d)];
  M = unique(M, 'rows');
  M = flipud(M(:, 2:d + 1));
  nM = size(M, 1);
  
  % For each element w of the list M, calculate the maximal translatable
  % pattern.
  
  % These three lines seemed to be generating an error at line 133, but I
  % have no idea why, as the corresponding code from SIACT does not
  % generate an error.
  % SIARCToutput = repmat(struct('pattern', [], 'indices', [],...
  %     'cardinality', [], 'compactness', [], 'region', [], 'span', [],...
  %     'regionType', [], 'vector', []), 1, nM);
  
  SIARCTcell = cell(1, nM);
  j = 1; % Increment over SIARCToutput.
  fifthMeasure = 1; % Logical for measuring fifth return time (FRT).
  for i = 1:nM
  % parfor i = 1:nM
    if fifthMeasure && j >= 5
        % Measure the 5th return time.
        FRT = toc(tStart);
        fifthMeasure = 0;
    end
    v = M(i,:);
    [P, ~] = maximalTranslatablePattern(v, D, n);
    % Check the MTP for compact subpatterns.
    if ~isempty(P)
      S = compactSubpatterns(P, D, d, compactThresh, cardinaThresh,...
        regionType);
    else
      S = [];
    end
    nS = size(S, 2);
    if nS > 0
      for iS = 1:nS
        S(iS).vector = v;
      end
    end
    SIARCTcell{i} = S;
    j = j + nS;
    % if isfield(S,'pattern');
    %   % Include the generating vector of the MTP.
    %   [S(:).vector] = deal(v);
    %   % Indices default to empty at present. Could be improved.
    %   [S(:).indices] = deal([]);
    %   % Append compact subpatterns to SIACT output.
    %   k = size(S, 2);
    %   SIARCToutput(j:j + k - 1) = S;
    %   j=j+k;
    % end
  end
  % Remove empty entries from SIARCTcell, and return a vector of structs.
  j = 1; % Increment to populate SIARCToutput.
  for iM = 1:nM
    S = SIARCTcell{iM};
    if isfield(S, 'pattern');
      % Include the generating vector of the MTP.
      % [S(:).vector] = deal(M(iM, :));
      % Indices default to empty at present. Could be improved.
      % [S(:).indices] = deal([]);
      % Append compact subpatterns to SIACT output.
      k = size(S, 2);
      if k > 0
        SIARCToutput(j:j + k - 1) = S;
      end
      j=j+k;
    end
  end
  if fifthMeasure
    FRT = toc(tStart);
  end
  runtime = toc(tStart);
else
  % SIAR has been run already, just need to apply the compactness trawler.
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

end
