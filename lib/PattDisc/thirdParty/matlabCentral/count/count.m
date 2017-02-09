function [freq] = count(X, ROWS)

% Developed by Elleke Janssen.

% Counts the number of different elements in vector X or rows in matrix X
% if ROWS is set to 'rows'. If ROWS is not specified or set to another
% value, COUNT counts the number of different elements in matrix X. NaNs
% are considered to be the same.
%
% The output consists of a matrix FREQ. The first column(s) are the unique
% elements (rows) of X; the one but last column contains of the number of
% times the element (row) appears and the last column displays the relative
% frequency.
%
% Example:
% x = [Inf NaN NaN; 1 2 3; 1 2 -Inf; 1 2 -Inf; 1 2 3; 1 2 5; Inf NaN NaN];
% 
% yr = count(x, 'rows')
% yr =
%     1.0000    2.0000      -Inf    2.0000    0.2857
%     1.0000    2.0000    3.0000    2.0000    0.2857
%     1.0000    2.0000    5.0000    1.0000    0.1429
%        Inf       NaN       NaN    2.0000    0.2857
% 
% yr = count(x)
% yr =
%       -Inf    2.0000    0.0952
%     1.0000    5.0000    0.2381
%     2.0000    5.0000    0.2381
%     3.0000    2.0000    0.0952
%     5.0000    1.0000    0.0476
%        Inf    2.0000    0.0952
%        NaN    4.0000    0.1905
%
% See also FIND, HIST, HISTC

if isempty(X)
  % If input is empty, output is empty as well.
  freq = [];
  return
end

% Set defaults
if nargin < 2 || min(size(X)) == 1
  ROWS = 'nrows';
else
  ROWS = lower(ROWS);
end

% Error if the rows of N-dimensional (N>2) matrices have to be counted.
if ndims(X) > 2 && strcmp(ROWS, 'rows');
  error('Counting rows is only possible when the dimension of X is 2.');
end

if ~strcmp(ROWS, 'rows')
  X = X(:);
end

% Sort X
X = sortrows(X);

% Start counting

% N = nr of rows of X and check if all elements are nonNaN / nonInf
N         = size(X, 1);
allfinite = all(isfinite(X(:)));

if ~allfinite
  % If not all elements are nonNaNs / nonInfs, replace NaNs, Infs and
  % -Infs, such that counting is easier.
  
  % Find a number to replace NaNs with such that that number is not in the
  % X and replace the NaNs, if any
  noninf = find(isfinite(X));
  if isempty(noninf);
    nannr = 1;
  else
    nannr = max(max(X(noninf))) + 2;
  end
  knan    = find(isnan(X));
  X(knan) = nannr;

  % Check if Infs and -Infs are included and replace by noninf numbers such
  % that NaNs are listed after Infs
  neginf = find(X == -Inf);
  posinf = find(X == Inf);
  if isempty(noninf);
    X(neginf) = -1;
    X(posinf) = 0;
  else
    X(neginf) = min(min(X(noninf)))-1;
    X(posinf) = max(max(X(noninf)))+1;
  end
end

% Get the unique rows of X
[Xu,i,j] = unique(X,'rows');
if size(Xu,1) == 1
  % If only 1 row in X, its frequency is N
  frqs = N;
else
  % Count the number of time each rows occurs by looking at when two
  % subsequent rows differ
  Xd    = sum(abs(diff(X)), 2);
  rowch = find(Xd ~= 0);
  frqs  = diff([0;rowch;N]);
end
if ~allfinite
  % Place back the -Infs, Infs and NaNs, if necessary
  X(neginf) = -Inf;
  X(posinf) = Inf;
  X(knan)   = NaN;
end
% Set output
freq = [X(i,:), frqs, frqs/N];