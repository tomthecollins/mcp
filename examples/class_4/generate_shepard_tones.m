% 10/10/2012 Tom Collins

tempo = [80 200];
durPercent = [25 90];
direction = {'up' 'down'};
centerFreq = 261.6255653005986;
nTone = 12;
nRepeat = 4;
location = fullfile('.');
shepardTones(tempo, durPercent, direction, centerFreq, nTone, nRepeat,...
             location);

