function [] = plotPatternOccurrence(S, D, i, j, allOcc)

% 17/5/2011 Copyright Tom Collins

% This function takes a vector of structs with information about a musical
% pattern as its first argument, a point-set representation (called a
% dataset) of a piece of music (or excerpt) as its second argument, an
% index i referring to an element of the vector of structs as its third
% argument, and an index j of occurrence as its fourth argument. It plots
% the dataset as points in the plane, and the jth occurrence of the pattern
% is highlighted. The variable allOcc takes value one if all occurrences
% are to be displayed, and zero otherwise.

% INPUT
%  S is a vector of structs, and each struct must at least contain fields
%   for pattern and translators.
%  D is a k x n matrix representing a k-dimensional set of n points.
%  i is a positive integer indicating the struct in S that will be
%   visualised.
%  j is a positive integer indicating the occurrence of the pattern that
%   will be visualised.
%  allOcc is a logical, taking the value one if all occurrences are to be
%   highlighted.

s = S(i);
% Determine the projection of the dataset to which the pattern belongs.
if isfield(s, 'projection')
    if strcmp(s.projection, 'Ontime and MIDI note number')
        d = unique(D(:,1:2),'rows');
        yAxisLabel = 'Pitch (MIDI note number)';
    elseif strcmp(s.projection, 'Ontime and morphetic pitch number')
        d = unique([D(:,1) D(:,3)],'rows');
        yAxisLabel = 'Pitch (morphetic pitch number)';
    else
        d = unique([D(:,1) D(:,4)],'rows');
        yAxisLabel = 'Duration (crotchet beats)';
    end
else
    d = D;
    yAxisLabel = 'Staff height (middle C = 60)';
end

x = (s.pattern(:,1) + s.translators(j,1))/4 + 1;
y = s.pattern(:,2) + s.translators(j,2);
    
% Define and name parts of the figure.
FontSize = 16;
FontName = 'Helvetica';
figure
hold on
xlim([min(x) - 1 max(x) + 1])
ylim([min(y) - 4 max(y) + 4])
plot(d(:,1)/4 + 1,d(:,2),'kx')
xlabel('Bar', 'FontName', FontName, 'FontSize', FontSize)
ylabel(yAxisLabel, 'FontName', FontName, 'FontSize', FontSize)
m = s.occurrences;
if allOcc
    title(sprintf('Pattern %d, occurrence %d', i, j), 'FontSize',...
        FontSize, 'FontName', FontName)
else
    title(sprintf('Pattern %d has %d occurrences', i, m), 'FontSize',...
        FontSize, 'FontName', FontName)
end
grid
set(gca, 'FontSize', FontSize);
set(gca, 'FontName', FontName);    
plot(x,y,'bo');
hold off

end
