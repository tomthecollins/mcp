function outputStruct = filterOutOverlaps(inputStruct, covPercent,...
  intersectEdit, cardinaThresh)

% Copyright Tom Collins 16/11/2013

% Returns discovered translational patterns from D that do not have too
% many overlapping occurrences. This is quantified by dividing the coverage
% (the total number of unique points across all occurrences of a pattern)
% by the product of the pattern's cardinality and number of occurrences.

% INPUT
%  inputStruct contains patterns discovered in D.
%  D is an n x k matrix representing a k-dimensional set of n points.
%  covPercent is a real number in (0, 1]. The ratio of coverage to the
%   product of cardinality and occurrences is calculated. If the result is
%   greater than or equal to covPercent, the pattern remains, otherwise it
%   is filtered out.
%  intersectEdit takes the value one if overlapping patterns with two
%   occurrences should be intersected and the overlap removed, prior to
%   placing in the output struct. Placement in the output struct only
%   occurs if the resulting pattern still has enough notes compared with
%   cardinaThres.
%  cardinaThresh is a positive integer, used in the instance that
%   intersectEdit is equal to one. When overlapping patterns are edited
%   rather than immediately filtered out, cardinaThresh is required to test
%   that the edited patterns still have enough notes.

if nargin < 4
  cardinaThresh = 0;
end
if nargin < 3
  intersectEdit = 0;
end
if nargin < 2
  covPercent = .9;
end

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
  if mod(patti, 10000) == 0
    fprintf('Filtering pattern %d of %d.\n', patti, pattn)
  end
  if S(patti).coverage/(S(patti).cardinality*S(patti).occurrences) >=...
      covPercent
    % Add a field to the output struct to indicate any patterns that have
    % been edited.
    s = S(patti);
    s.notaBene = [];
    if pattj == 1
      outputStruct = s;
    else
      outputStruct(pattj) = s;
    end
    pattj = pattj + 1;
  elseif intersectEdit && S(patti).occurrences == 2
    % Here we remove the overlap between a two-occurrence pattern by using
    % setdiff.
    l = size(S(patti).pattern, 1);
    P1 = S(patti).pattern + repmat(S(patti).translators(1, :), l, 1);
    P2 = S(patti).pattern + repmat(S(patti).translators(2, :), l, 1);
    Q2 = setdiff(P2, P1, 'rows');
    lQ = size(Q2, 1);
    Q1 = Q2 - repmat(S(patti).translators(2, :), lQ, 1);
    % Code for plots.
    % fileName = fullfile('~', 'Data', 'Music', 'JKU',...
    %   'JKUPDD-noAudio-Aug2013', 'groundTruth', 'chopinOp24No4',...
    %   'polyphonic', 'lisp', 'mazurka24-4.txt');
    % D = lispStylePointSet2Matrix(fileName);
    % [D2, idx] = unique(D(:, [1 3]), 'rows');
    % plotPattern(S, D2, pattj, 1, 1)
    % figure
    % plot(D2(:, 1), D2(:, 2), 'k.')
    % hold on
    % plot(Q1(:, 1), Q1(:, 2), 'bo')
    % plot(Q2(:, 1), Q2(:, 2), 'gx')
    if lQ >= cardinaThresh
      s = S(patti);
      s.pattern = Q1;
      s.cardinality = lQ;
      s.notaBene = 'Edited overlap with setdiff.';
      if pattj == 1
        outputStruct = s;
        % Should really amend other properties of the pattern as well.
      else
        outputStruct(pattj) = s;
      end
      pattj = pattj + 1;
    end
  end
end

end
