function outputStruct = filterByUniquePitchClasses(inputStruct,...
  pitchDim, m, n)

% Copyright Tom Collins 10/4/2013

% Returns discovered translational patterns from D that have at least m
% unique pitch classes mod n.

% INPUT
%  inputStruct contains patterns discovered in D.
%  pitchDim is an integer specifying the dimension of D that contains the
%   pitch representation of interest.
%  m is an integer specifying the threshold for the number of unique pitch
%   clasees.
%  n is an integer specifying the value to use for m modulo n (e.g., 12 for
%   MIDI note numbers, and 7 for morphetic pitch numbers).

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
  if mod(patti, 1000) == 0
    fprintf('Filtering pattern %d of %d.\n', patti, pattn)
  end
  if size(unique(mod(S(patti).pattern(:, pitchDim), n)), 1) >= m
    if pattj == 1
      outputStruct = S(patti);
    else
      outputStruct(pattj) = S(patti);
    end
    pattj = pattj + 1;
  end
end

% Could sort if we wanted.
% fprintf('Finished filtering, now sorting by compression ratio.\n')
% filtn = size(outputStruct, 2);
% if filtn > 1
%   a = [outputStruct.rating];
%   [~, idx] = sort(a, 'descend');
%   outputStruct = outputStruct(idx);
% end

end
