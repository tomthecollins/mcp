function activations_resampled = change_frame_rate_simple(activations,...
  fr_source, fr_target)

% Tom Collins 4/8/2015.

% This function is extracted and simplified from Florian Kreb's
% Feature.m file, and changes the framerate of a feature sequence.

% 1. Convert time index
dimension = size(activations, 2);
len_source_sec = length(activations) / fr_source;
numframes_target = round(len_source_sec * fr_target);
framelength_target = 1 / fr_target;
t = (0:length(activations)-1) / fr_source;
if abs(fr_source - fr_target) > 0.001
  if (len_source_sec - numframes_target*fr_target) > 0.001 % add samples
    delta_t = 1/fr_source;
    num_f = ceil((numframes_target*framelength_target-t(end)) ...
      / delta_t);
    act = 0.5*ones(num_f,1);
    activations = [activations; act];
    t = [t t(end)+(1:num_f)*delta_t];
  end
  t2 = (0:numframes_target-1)*framelength_target;
  a1 = zeros(numframes_target, dimension);
  if fr_source > fr_target
    % target vector smaller than source vector, use only max
    % values within a certain window
    for i=1:numframes_target-1
      a1(i, :) = max(activations((((t-t2(i)) > -0.001) ...
        & ((t-t2(i+1)) < -0.001)), :));
    end
  else
    % target vector bigger than source vector, interpolate!
    for i = 1:dimension
      a1(:, i) = interp1(t, activations, t2);
    end
  end
  a1(end, :) = mean(activations);
  activations_resampled = a1;
else
  % no conversion needed - return source values:
  activations_resampled = activations;
end

end
