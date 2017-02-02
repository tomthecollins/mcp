function beats = find_beat_times_simple(params, position_state, rhythm_state)

% Tom Collins 4/8/2015.

% This is a simplified version of Florian Krebs' find_beat_times from
% HMM.m. The output is an nBeats x 2 matrix, giving the beat times in
% seconds and bar.beatnumber.

% INPUT
%  params is a struct consisting of values for the fields R and Meff.
%  position_state is a sequence of position states (usually the m_path
%   variable).
%  rhythm_state is a sequence of rhythm states.

numframes = length(position_state);
% Set up a cell array with beat position for each meter.
beatpositions = cell(params.R, 1);
for i_r=1:params.R
  beatpositions{i_r} = round(linspace(1, params.Meff(i_r), ...
    params.rhythm2meter(i_r, 1) + 1));
  beatpositions{i_r} = beatpositions{i_r}(1:end - 1);
end

beats = [];
for i = 1:numframes-1
  if rhythm_state(i) == 0
    % Silence state.
    continue;
  end
  for j = 1:length(beatpositions{rhythm_state(i)})
    beat_pos = beatpositions{rhythm_state(i)}(j);
    beat_detected = false;
    if position_state(i) == beat_pos;
      % Current frame = beat frame.
      bt = i;
      beat_detected = true;
    elseif ((position_state(i+1) > beat_pos) ...
        && (position_state(i+1) < position_state(i)))
      % Bar transition between frame i and frame i + 1.
      bt = interp1([position_state(i);...
        params.Meff(rhythm_state(i)) + position_state(i + 1)], ...
        [i; i + 1], params.Meff(rhythm_state(i)) + beat_pos);
      beat_detected = true;
    elseif ((position_state(i) < beat_pos) ...
        && (position_state(i+1) > beat_pos))
      % Beat position lies between frame i and frame i + 1.
      bt = interp1([position_state(i); position_state(i + 1)], ...
        [i; i + 1], beat_pos);
      beat_detected = true;
    end
    if beat_detected
      beats = [beats; [round(bt), j]];
      break;
    end
  end
end
if ~isempty(beats)
  beats(:, 1) = beats(:, 1) * params.frame_length;
end

end
