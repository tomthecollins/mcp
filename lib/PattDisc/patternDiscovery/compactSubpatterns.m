function S = compactSubpatterns(P, D, dimension, compactThresh,...
  cardinaThresh, regionType)

% 9/5/2011 Copyright Tom Collins

% This function takes a pattern P (usually an MTP) and a dataset, of which
% P is a subset, as its first two arguments. The dimension of the pattern
% is provided, as are parameters compactThresh, cardinaThresh, and
% regionType. More information about these parameters is given below (see
% also the function SIACT).

% Consecutive subsets of P are considered, e.g. {p1}, {p1, p2},
% {p1, p2, p3}, and while the compactness of these subsets in the dataset
% remains above the threshold compactThresh, more and more datapoints are
% considered. When the compactness falls below the threshold for some set
% {p1, p2,..., pL}, the subset {p1, p2,..., p_{L-1}} along with other
% information is added to the output, if L-1 >= cardinaThresh.

% Could improve this function by returning indices.

% INPUT
%  P is an l x k matrix representing an occurrence of a pattern.
%  D is an n x k matrix representing a point set in which the pattern
%   occurs.
%  dimension is a positive integer for the dimension of the point set
%   (equal to k).
%  compactThresh is a parameter in (0, 1], giving the minimum compactness a
%   pattern occurrence must have in order to be included in the output.
%  cardinaThresh is a positive integer parameter, giving the minimum number
%   points that compactness a pattern occurrences must have in order to be
%   included in the output.
%  regionType is a string equal to 'lexicographic' or 'convex hull',
%   indicating which definition of region should be used for calculating
%   the compactness of patterns.

% EXAMPLE INPUT
% P = [12 71 0.5; 12.5 68 0.5; 13 69 1; 14 65 1; 14 67 1; 15 68 0.5;...
%     15.5 66 0.5; 16 64 2];
% D = [0 69 0.5; 0.5 66 0.5; 1 67 1; 2 65 1; 3 66 0.5; 3.5 64 0.5;...
%     4 62 2; 5 55 1; 5 59 1; 5 61 1; 6 63 0.5; 6.5 64 0.5; 7 55 1;...
%     7 56 1; 7 61 1; 7 65 1; 8 55 1; 8 57 1; 8 61 1; 8 66 1; 9 51 1;...
%     9 67 0.5; 9.5 70 0.5; 10 55 1; 10 60 1; 10 69 2; 11 51 1; 12 50 1;...
%     12 71 0.5; 12.5 68 0.5; 13 57 1; 13 60 1; 13 62 1; 13 69 1;...
%     14 60 1; 14 62 1; 14 65 1; 14 67 1; 15 68 0.5; 15.5 66 0.5;...
%     16 57 1; 16 61 1; 16 63 1; 16 64 2; 17 57 1; 17 59 1; 17 63 1;...
%     18 65 0.5; 18.5 66 0.5; 19 57 1; 19 58 1; 19 63 1; 19 67 1;...
%     20 57 1; 20 59 1; 20 63 1; 20 68 1];
% dimension = 3;
% cardinaThresh = 3;
% compactThresh = 0.5;
% regionType = 'lexicographic';

cardP = size(P,1);
[~, firstIndex] = ismember(P(1,:), D, 'rows');
[~, lastIndex] = ismember(P(cardP,:), D, 'rows');
% Relevant part of the dataset.
relD = D(firstIndex:lastIndex, :);
% Output will be a struct.
S = repmat(struct('pattern', [], 'cardinality', [], 'compactness', [],...
  'region', [], 'span', [], 'regionType', []), 1, cardP);
i = 1; % Increment over struct.
Q = P(1, :); % Possible pattern to include in S.
L = 1; % Cardinality of this pattern.
compactness = 1;
previousCompactness = 1;
previousRegion = Q;
previousSpan = 1;
j = 2; % Increment over P.
while j <= cardP
  L=L+1;
  Q(L,:) = P(j,:);
  if strcmp(regionType, 'convex hull')
    if L > 2 && ~collinearTest(Q(:,1:2)) % Check there are
      % enough points to construct hull, and that these points are
      % not collinear.
      % Using a two-dimensional convex hull, so project Q on to two
      % dimensions.
      ch = convhull(Q(:,1),Q(:,2));
      % x-coordinates of polygon vertices.
      xv = Q(ch,1);
      % y-coordinates of polygon vertices.
      yv = Q(ch,2);
      % x-coordinates of points to test.
      x = relD(:,1);
      % y-coordinates of points to test.
      y = relD(:,2);
      in = inpolygon(x,y,xv,yv);
      % plot(xv,yv,x(in),y(in),'r+',x(~in),y(~in),'bo')
      region = relD(in,:);
    else
      region = Q;
    end
  else
    [~, regStart] = ismember(Q(1,:), relD, 'rows');
    [~, regEnd] = ismember(Q(end,:), relD, 'rows');
    region = relD(regStart:regEnd, :);
  end
  span = size(region,1);
  compactness = L/span;
  if compactness < compactThresh % Has compactness dropped below the
    % threshold?
    if L > cardinaThresh % Does the pattern contain enough points to
      % be admitted?
      S(i).pattern = Q(1:end-1,:);
      S(i).cardinality = size(S(i).pattern, 1);
      S(i).compactness = previousCompactness;
      S(i).region = previousRegion;
      S(i).span = previousSpan;
      S(i).regionType = regionType;
      i=i+1;
    end
    Q = P(j,:);
    L = 1;
    previousCompactness = 1;
    previousRegion = Q;
    previousSpan = 1;
  else
    previousCompactness = compactness;
    previousRegion = region;
    previousSpan = span;
  end
  j=j+1;
end

% This code catches any compact subpatterns that are right at the end of
% the argument pattern P.
if compactness >= compactThresh && L >= cardinaThresh
  S(i).pattern = Q;
  S(i).cardinality = size(S(i).pattern, 1);
  S(i).compactness = compactness;
  S(i).region = region;
  S(i).span = span;
  S(i).regionType = regionType;
  i=i+1;
end
S = S(1:i-1);

end
