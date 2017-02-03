% Seminar in Music Computing and Psychology
% Class 2 - Audio Representations of Music

% Create a sine wave with frequency 440 Hz and play it.
% The time interval from 0-1 s, in increments of 1/44100.
t = (0:44100)/44100;
% The frequency of the sine wave.
w = 300;
% The amplitude (loudness) of the sine wave.
a = 0.5;
% Construct the waveform.
Y = a*sin(2*pi*w*t);
% Plot the waveform and zoom into an appropriate time window.
plot(t, Y);
xlim([0, 0.05]);
xlabel('Time (s)');
ylabel('Amplitude (Arbitrary Scale)');
set(gca,'FontSize', 18);
% Play the waveform.
player = audioplayer(Y,44100);
% play(player)

% t1 = (0:6)/6;
W2 = 660;
Y2 = a*sin(2*pi*w2*t);

Z = Y + Y2; 
plot(Z); 
player = audioplayer(Z,44100);
play(player)

t2 = (0:7)/7;
W3 = 660;
Y3 = a*sin(2*pi*w2*t);

Z1 = Y + Y2 + Y3; 
plot(Z1); 
player = audioplayer(Z1,44100);
play(player)

