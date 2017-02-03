
dest = fullfile('/home', 'tommyc', 'projects', 'nancySignalProcessing');
% Load music excerpt.
[y, Fsy] = wavread(fullfile(dest, 'liberaMePerf.wav'));
y = y(:, 1); % Just left channel.
% Load ringtone.
[x, Fsx] = wavread(fullfile(dest, 'cellPhone2.wav'));

% Superpose some of the cell phone on the music excerpt.
z = y;
Fsz = Fsy;
z(176401:210400, 1) = z(176401:210400, 1) + x(1:34000, 1);
plot(z);

wavwrite(z, Fsy, fullfile(dest, 'corrupt.wav'))

% Fourier transform of corrupted signal.
L = 34000; % Length of signal.
NFFT = 2^nextpow2(L); % Next power of 2 from length of z.
Z = fft(z(176401:210400), NFFT)/L;
freq = Fsz/2*linspace(0, 1, NFFT/2 + 1); % Frequencies analyzed.
% Plot amplitude spectrum.
plot(freq,2*abs(Z(1:NFFT/2 + 1))) 
title('Amplitude Spectrum of Signals')
xlabel('Frequency (Hz)')
ylabel('|Z(f)|')