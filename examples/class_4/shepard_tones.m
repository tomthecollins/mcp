function shepardTones(tempo, durPercent, direction, centerFreq, nTone,...
    nRepeat, location)

% 10/10/2012 Tom Collins

% This function generates wav files containing sequences of Shepard tones.
% The arguments are parameters governing properties of the sequences.

% REFERENCE
%  Shepard, Roger N, Circularity in Judgements of Relative Pitch, in 
%   Journal of the Acoustical Society of America, 36(12) (1964), 2346-2353.

% INPUT
%  tempo is a vector of tempos in beats per minute. (One tone per beat is
%   generated.)
%  durPercent is a vector of percentages specifying the amount of inter-
%   onset interval that is taken up by each tone.
%  direction is a cell of strings containing one or both of 'up' and
%   'down', for generating ascending or descending sequences.
%  centerFreq is the frequency (Hz) of the middle tone.
%  nTone is the number of tones in each sequence.
%  nRepeat is the number of times to repeat the nTones.
%  location is a string specifying where the wav files should be stored.

% EXAMPLE
% tempo = [80 200];
% durPercent = [25 90];
% direction = {'up' 'down'};
% centerFreq = 261.6255653005986;
% nTone = 12;
% nRepeat = 4;
% location = fullfile('/home', 'tommyc', 'projects', 'shepardForEve',...
%     'wavFiles');

% Some parameters.
amp = .8;
Fs = 44100; % Sampling rate.
nPartial = 21; % Number of partials in each complex tone.
durInt = 60./tempo;

% Express different tones and partials in steps.
startStep = floor(nTone/2);
startOctave = floor(nPartial/2);
octave = -startOctave:startOctave;

% Get sizes.
nTempo = size(tempo, 2);
nDurPercent = size(durPercent, 2);
nDirection = size(direction, 2);

% Loop over input parameters and generate wav files.
for iTempo = 1:nTempo % Loop over tempos.
    onTime = 0:durInt(iTempo):durInt(iTempo)*nTone; % Calculate ontimes.
    onSamp = round(Fs*onTime) + 1; % Convert to samples.
    for iDurPercent = 1:nDurPercent % Loop over duration percentages.
        durSamp = round(onSamp(2)*durPercent/100);
        t = (0:durSamp(iDurPercent)-1)/Fs;
        for iDirection = 1:nDirection % Loop over directions.
            signal = zeros(onSamp(end), 1); % Pre-allocate signal variable.
            if strcmp(direction(iDirection), 'up')
                steps = -startStep:startStep - 1;
            else
                steps = startStep-1:-1:-startStep;
            end
            % Convert steps back into frequences.
            freq = centerFreq*2.^(steps/12);
            for iTone = 1:nTone % Loop over tones in a sequence.
                % Pre-allocate the signal for a tone.
                y = zeros(durSamp(iDurPercent), 1);
                for iPartial = 1:nPartial % Loop over partials.
                    % Calculate the frequency of a partial.
                    currFreq = freq(iTone)*2^octave(iPartial);
                    % Define the sinusoid for that partial.
                    x = sin(currFreq*(2*pi)*t)*...
                        normpdf(log(currFreq) - log(centerFreq), log(2)/4);
                    % plot(t, x);
                    % Add sinusoid to signal for the tone (superposition of
                    % waves).
                    y = y + x';
                    % plot(t, y);
                end
                % Normalize tone so that the maximum value equals amp.
                y = amp*y/max(y);
                % Add it to the signal for the whole sequence
                % (superposition of waves).
                signal(onSamp(iTone):...
                    onSamp(iTone) + durSamp(iDurPercent) - 1) =...
                    signal(onSamp(iTone):...
                    onSamp(iTone) + durSamp(iDurPercent) - 1) + y;
            end
            % Repeat the signal nRepeat times.
            signal = repmat(signal, nRepeat, 1);
            % Create filename and generate wav file.
            fName = [num2str(tempo(iTempo)) '_bpm-'...
                num2str(durPercent(iDurPercent)) '_dpc-'...
                direction{iDirection} '.wav'];
            wavwrite(signal, Fs, fullfile(location, fName));
        end
    end
end

end
