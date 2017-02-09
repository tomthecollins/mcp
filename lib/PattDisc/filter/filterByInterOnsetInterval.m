function outputStruct = filterByInterOnsetInterval(inputStruct, ioi)

% Copyright Tom Collins 11/2/2015

% Returns discovered translational patterns from D that do not have too
% long an inter-onset interval between any pair of consecutive points.

% INPUT
%  inputStruct contains patterns discovered in D.
%  ioi (inter-onset interval) is a positive real. If any pair of
%   consecutive notes has an inter-onset interval greaeter than or equal to
%   ioi, the pattern is not included in the output struct. Otherwise it is
%   included.

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
  if mod(patti, 10000) == 0
    fprintf('Filtering pattern %d of %d.\n', patti, pattn)
  end
  s = S(patti);
  max_ioi = max(diff(s.pattern(:, 1)));
  if max_ioi < ioi;
    if pattj == 1
      outputStruct = s;
    else
      outputStruct(pattj) = s;
    end
    pattj = pattj + 1;
  end
end

end
