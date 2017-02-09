function [] = plotPattern(S, D, i, includeInexact, tfWait)

% 17/5/2011 Copyright Tom Collins

% This function takes a vector of structs with information about a musical
% pattern as its first argument, a point-set representation (called a
% dataset) of a piece of music (or excerpt) as its second argument, and an
% index i referring to an element of the vector of structs as its third
% argument. It plots the dataset as points in the plane, and pressing a
% keyboard button will cause the first occurrence of the pattern to be
% highlighted. Subsequent presses of the keyboard will highlight subsequent
% occurrences of the pattern, until all have been highlighted.

% INPUT
%  S is a vector of structs, and each struct must at least contain fields
%   for pattern and translators.
%  D is a k x n matrix representing a k-dimensional set of n points.
%  i is a positive integer indicating the struct in S that will be
%   visualised.
%  includeInexact is an optional logical argument, determining whether to
%   look for and then plot inexact occurrences of patterns as well.
%  tfWait is an optional logical argument, determining whether the
%   waitforbuttonpress command is invoked.
%  projIdx is an optional vector argument, specifying which columns of D
%   were used for discovering patterns that are output as S.

if nargin < 5
  tfWait = 1;
end
if nargin < 4
  includeInexact = 0;
end
MarkerSize = 8;

s = S(i);
% Determine the projection of the dataset to which the pattern belongs.
if isfield(s, 'projection')
  if strcmp(s.projection, 'Ontime and MIDI note number')
    d = unique(D(:, 1:2),'rows');
    yAxisLabel = 'Pitch (MIDI note number)';
  elseif strcmp(s.projection, 'Ontime and morphetic pitch number')
    d = unique([D(:, 1) D(:, 3)],'rows');
    yAxisLabel = 'Pitch (morphetic pitch number)';
  else
    d = unique([D(:, 1) D(:, 4)],'rows');
    yAxisLabel = 'Duration (crotchet beats)';
  end
else
  d = D;
  yAxisLabel = 'Pitch (middle C = 60)';
end

% Define and name parts of the figure.
figure
hold on
plot(d(:, 1), d(:, 2), 'Color', [.5 .5 .5], 'LineStyle', 'none',...
  'Marker', 'x', 'MarkerSize', MarkerSize);
axis([(min(d(:, 1)) - 1) (max(d(:, 1)) + 1) (min(d(:, 2)) - 1) ...
  (max(d(:, 2)) + 1)]);
xlabel('Ontime (crotchet beats)')
ylabel(yAxisLabel)
if isfield(s, 'projection')
  title(strcat(s.projection, ' pattern'))
end
grid
% Add occurrences of patterns to plot.
c = ['c' 'b' 'm' 'g' 'r' 'y'];
j = 1;
while j <= s.occurrences
  if tfWait
    waitforbuttonpress
  end
  x = s.pattern(:, 1) + s.translators(j, 1);
  y = s.pattern(:, 2) + s.translators(j, 2);
  plot(x, y, 'Color', c(mod(j, 6)+1),  'LineStyle', 'none',...
    'Marker', '>', 'MarkerSize', MarkerSize);
  j=j+1;
end
% Add inexact occurrences of patterns to plot.
if includeInexact
  regions = s.inexactOccurrences.regions;
  nocc = size(regions, 2);
  j = 1;
  while j <= nocc
    if tfWait
      waitforbuttonpress
    end
    plot(regions{j}(:, 1), regions{j}(:, 2), 'Color', 'k',...
      'LineStyle', 'none', 'Marker', '>', 'MarkerSize', MarkerSize);
    j=j+1;
  end
end
hold off

end
