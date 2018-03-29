% Copyright Tom Collins 3/25/2018

% Probe db for a randomly selected and noisy audio clip.
clipLength = 2;

% Load dbase.
load('dbase');

% Add a fast 2D peak finder.
addpath('./FastPeakFind');

% Paths to audio files.
audPaths = {...
  './audio/guitar.wav'...
  './audio/piano.wav'...
  './audio/strings.wav'...
  % './audio/12 La Vie En Rose excerpt.mp3'...
  % './audio/Jaymes Young - Don''t You Know.mp3'...
  % './audio/02 The Greatest Man That Ever Lived (Variations On a Shaker Hymn).mp3'...
  % './../class_7/Labyrinth.wav'
  % './../class_6/beat_tracking/Kendrick Lamar - HUMBLE.wav'
  ...
  };
naud = size(audPaths, 2);

% Spectrogram parameters.
nfft = 8192;
win = hann(nfft);
overlap = 7*nfft/8; % 25% overlap between adjacent spectra.
step = nfft - overlap;
nrows = 500; % Most of the spectrum (representing higher frequencies) is empty.

% Fingerprint parameters.
timeThresMin = 0.1;
timeThresMax = 1;
pidxThresMin = 0;
pidxThresMax = 150;

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
% signoise = sig;
signoise = 0.96*sig + 0.04*2*(rand(nsamp, 1) - 0.5);
player2 = audioplayer(signoise, Fs);
play(player2);

% Spectrogram and peak pick.
[s, w, t] = spectrogram(signoise, win, overlap, nfft);
s = abs(s(1:nrows, :));
% Show spectrogram.
close all; imagesc(-s); colormap 'gray'; axis xy;
xlabel('Time (Spectrogram Increment)', 'FontSize', 18);
ylabel('Frequency (Spectrogram Increment)', 'FontSize', 18);
% Pick peaks.
thres = quantile(s(:), 0.95);
IJ = FastPeakFind(s, thres);
I = IJ(2:2:end);
J = IJ(1:2:end);
hold on; plot(J, I, 'r+'); hold off;
  
% Create fingerprints.
matches = [];
nmatch = 0;
fp = [];
nfp = 0;
npeak = size(I, 1);
for ii = 1:npeak
  ind1 = [I(ii) J(ii)];
  jj = ii + 1;
  while jj <= npeak
    ind2 = [I(jj) J(jj)];
    time_diff = (ind2(2) - ind1(2))*step/Fs;
    pitch_diff = abs(ind2(1) - ind1(1));
    % Decide whether to make a fingerprint.
    if time_diff > timeThresMin && time_diff < timeThresMax &&...
        pitch_diff > pidxThresMin && pitch_diff < pidxThresMax
      % Make a fingerprint. Start with the hash.
      time_hash = num2str(round(1000*time_diff));
      if time_diff < .01
        time_hash = ['00' time_hash];
      elseif time_diff < .1
        time_hash = ['0' time_hash(1:2)];
      else
        time_hash = time_hash(1:3);
      end
      pitch_hash = num2str(pitch_diff);
      hash = ['fp' time_hash pitch_hash];
      if ~isfield(dbase, hash)
        % No fp found.
      else
        % Fp found!
        % fprintf('ii = %d, jj = %d.\n', ii, jj);
        tstampIdPairs = dbase.(hash);
        npair = size(tstampIdPairs, 1);
        for ipair = 1:npair
          currPair = tstampIdPairs(ipair);
          matches = [matches;...
            struct('dbts', currPair.timestamp, 'pbts', ind1(2)*step,...
            'idpiece', currPair.idpiece)];
          nmatch=nmatch+1;
        end
      end
      nfp=nfp+1;
      fp = [fp; [ind1 ind2]];
      % fprintf(...
      %   'Made a fp for ind1 = [%d, %d] and ind2 = [%d, %d].\n',...
      %   ind1(1), ind1(2), ind2(1), ind2(2));
    end
    % Move on to the next value of ii if we are too far away in time.
    if time_diff >= timeThresMax
      jj = npeak;
    end
    jj=jj+1;
  end
end
% Plot some of the fingerprints.
hold on;
for ifp = 1:nfp
  pause(0.01);
  line([fp(ifp, 2) fp(ifp, 4)], [fp(ifp, 1) fp(ifp, 3)]);
end
hold off;

% Plot of matches.
figure; plot([matches.dbts], [matches.pbts], '.b');
xlabel('Time in Database (Samples)', 'FontSize', 18);
ylabel('Time in Query (Samples)', 'FontSize', 18);
for i = 1:naud
  line([dbase.songs(i).cumuSamp + 1 ...
        dbase.songs(i).cumuSamp + 1],...
       [0 max([matches.pbts] + 500)],...
       'Color', 'k', 'LineStyle', '--');
end
% Histogram.
figure;
hist([matches.dbts] - [matches.pbts], 100)
xlabel('Transformed Time in Database (Samples)', 'FontSize', 18);
ylabel('Number of Matches', 'FontSize', 18);
[counts, centers] = hist([matches.dbts] - [matches.pbts], 100);
[C, I] = max(counts);

% Determine name of winning song from max in histogram.
winningSample = centers(I);
winningSongIdx = find(winningSample >= [dbase.songs.cumuSamp], 1, 'last');
% THIS IS THE SONG 'SHAZAM' THINKS IS IN THE CLIP!
winningSongNam = dbase.songs(winningSongIdx).fnam
















% Example transformation.
x = [4 5 6 7 8 9];
y = [1 2 3 4 5 6];
close all
plot(x, y, '.b');
xlim([0 10])
ylim([0 7])
xlabel('Time in Database (Samples)', 'FontSize', 18);
ylabel('Time in Query (Samples)', 'FontSize', 18);
% Apply transformation.
z = x - y;
figure; plot(z, y, '.b');
xlim([0 10])
ylim([0 7])
xlabel('Transformed Time in Database (Samples)', 'FontSize', 18);
ylabel('Time in Query (Samples)', 'FontSize', 18);
