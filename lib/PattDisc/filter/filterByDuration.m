function outputStruct = filterByDuration(inputStruct, dur)

% Copyright Tom Collins 9/4/2013

% Returns discovered translational patterns from D that have at least dur
% duration, and also sorts by duration.

% INPUT
%  inputStruct contains patterns discovered in D.
%  D is an n x k matrix representing a k-dimensional set of n points.
%  dur is a real number. The value in the first dimension of the first
%   element of each pattern is subtracted from the value in the first
%   dimension of the last element of each pattern. If the result is greater
%   than or equal to dur, the pattern remains, otherwise it is filtered
%   out.

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
  if mod(patti, 10000) == 0
    fprintf('Filtering pattern %d of %d.\n', patti, pattn)
  end
  S(patti).dur = S(patti).pattern(end, 1) - S(patti).pattern(1, 1);
  if S(patti).dur >= dur
    if pattj == 1
      outputStruct = S(patti);
    else
      outputStruct(pattj) = S(patti);
    end
    pattj = pattj + 1;
  end
end

% Sort outputStruct by duration field.
fprintf('Finished filtering, now sorting by occurrences.\n')
filtn = size(outputStruct, 2);
if filtn > 1
  a = [outputStruct.dur];
  [~, idx] = sort(a, 'descend');
  outputStruct = outputStruct(idx);
end

end

