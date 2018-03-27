% Copyright Tom Collins 3/25/2018

% Probe db for a randomly selected and noisy audio clip.
clipLength = 2.5;

% Load dbase.
load('dbase');

% Add a fast 2D peak finder.
addpath('./FastPeakFind');

% Paths to audio files.
audPaths = {...
  ...%'./../class_7/Labyrinth.wav'
  ...%'./../class_6/beat_tracking/Kendrick Lamar - HUMBLE.wav'
  './fragment_up.wav'
  './fragment_down.wav'
  ...
  };
naud = size(audPaths, 1);

% Spectrogram parameters.
nfft = 2048;
win = hann(nfft);
overlap = nfft/4; % 25% overlap between adjacent spectra.
nrows = 50; % Most of the spectrum (representing higher frequencies) is empty.

% Fingerprint parameters.
timeThresMin = 0.1;
timeThresMax = 1;
pidxThresMin = 0;
pidxThresMax = 5;

% Choose an audio file at random.
audPath = audPaths{randi(naud)};
% Choose a clip of it at random.
[sig, Fs] = audioread(audPath);
startIdx = randi(size(sig, 1) - Fs*clipLength);
sig = sig(startIdx:startIdx + Fs*clipLength - 1, 1);
nsamp = size(sig, 1);
player = audioplayer(sig, Fs);
play(player);
% Add some noise.
signoise = 0.92*sig + 0.08*2*(rand(nsamp, 1) - 0.5);
player2 = audioplayer(signoise, Fs);
play(player2);

% Spectrogram and peak pick.
[s, w, t] = spectrogram(signoise, win, overlap, nfft);
s = abs(s(1:50, :));
% Show spectrogram.
close all; imagesc(-s); colormap 'gray'; axis xy;
xlabel('Time (Spectrogram Increment)');
ylabel('Frequency (Spectrogram Increment)')
% Pick peaks.
thres = quantile(s(:), 0.85);
IJ = FastPeakFind(s, thres);
I = IJ(2:2:end);
J = IJ(1:2:end);
hold on; plot(J, I, 'r+'); hold off;
  
% Create fingerprints.
matches = [];
npeak = size(I, 1);
nmatch = 0;
for ii = 1:npeak
  ind1 = [I(ii) J(ii)];
  jj = ii + 1;
  while jj <= npeak
    ind2 = [I(jj) J(jj)];
    time_diff = (ind2(2) - ind1(2))*overlap/Fs;
    pitch_diff = abs(ind2(1) - ind1(1));
    % Decide whether to make a fingerprint.
    if time_diff > timeThresMin && time_diff < timeThresMax &&...
        pitch_diff > pidxThresMin && pitch_diff < pidxThresMax
      % Make a fingerprint. Start with the hash.
      time_hash = num2str(time_diff);
      time_hash = time_hash(3:4);
      pitch_hash = num2str(pitch_diff);
      hash = ['fd' time_hash pitch_hash];
      if ~isfield(dbase, hash)
        % No fp found.
      else
        % Fp found!
        tstampIdPairs = dbase.(hash);
        npair = size(tstampIdPairs, 1);
        for ipair = 1:npair
          currPair = tstampIdPairs(ipair);
          matches = [matches;...
            struct('dbts', currPair.timestamp, 'pbts', ind1(2)*overlap,...
            'idpiece', currPair.idpiece)];
          nmatch=nmatch+1;
        end
      end
    end
    % Move on to the next value of ii if we are too far away in time.
    if time_diff >= timeThresMax
      jj = npeak;
    end
    jj=jj+1;
  end
end

% Plot of matches.
plot([matches.dbts], [matches.pbts], '.b');
figure;
[counts, centers] = hist([matches.dbts] - [matches.pbts], 100);



% Example transformation.
x = [4 5 6 7 8 9];
y = [1 2 3 4 5 6];
z = x - y;
plot(z, y, '.b');
xlim([0 10])
ylim([0 7])

