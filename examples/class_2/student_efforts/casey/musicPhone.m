
dest = fullfile('.');
% Load music excerpt.
[y, Fsy] = audioread(fullfile(dest, 'liberaMePerf.wav'));
y = y(:, 1); % Just left channel.
% Load ringtone.
[x, Fsx] = audioread(fullfile(dest, 'cellPhone2.wav'));

% Superpose some of the cell phone on the music excerpt.
z = y;
Fsz = Fsy;
z(176401:210400, 1) = z(176401:210400, 1) + x(1:34000, 1);
plot(z);

audiowrite(fullfile(dest, 'corrupt.wav'), z, Fsy)

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

% Fourier transform of the cell phone.
X = fft(x(1:34000), NFFT)/L;
% Plot amplitude spectrum.
hold on
plot(freq,2*abs(X(1:NFFT/2 + 1)), '--r')
hold off

% Subtract spectrum of X (cell phone) from spectrum of Z (corrupted
% signal).
Y_hat = Z - X;
hold on
plot(freq,2*abs(Y_hat(1:NFFT/2 + 1)), 'g')
hold off
% Convert spectrum back into time-domain function.
y_hat_insert = ifft(L*Y_hat);
y_hat = [z(1:176400); zeros(34000, 1); z(210401:end)];
y_hat = [z(1:176400); y_hat_insert; z(210401:end)];
y_hat = [z(1:176400); y_hat_insert(1:L); z(210401:end)];
plot(y_hat)
% Save to wav file.
audiowrite(fullfile(dest, 'uncorrupt.wav'), y_hat, Fsy);


