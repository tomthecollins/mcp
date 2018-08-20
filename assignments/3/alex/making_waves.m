
freq = [14 16 24 260 12000 20000];
% freq = 260;
dur = 180;
Fs = 44100;
nsamp = Fs*dur;

for i = 1:size(freq, 2);
  % Make the signal.
  t = 0:1/Fs:dur;
  s = sin(freq(i)*2*pi*t);
  plot(t, s);
  % Play the signal.
  % player = audioplayer(s, Fs);
  % play(player);
  % stop(player);
  % Save the signal.
  audiowrite(['./audio/' num2str(freq(i)) '.wav'], s, Fs);
end


