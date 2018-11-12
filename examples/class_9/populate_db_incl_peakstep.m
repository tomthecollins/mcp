% Copyright Tom Collins 3/24/2018

% Create db for a bunch of audio files.

% Add a fast 2D peak finder.
addpath('./FastPeakFind');

% Paths to audio files.
audPaths = {...
  % First three here are good initial testing data.
  % './audio/guitar.wav'...
  % './audio/piano.wav'...
  % './audio/strings.wav'...
  './audio/02 The Greatest Man That Ever Lived (Variations On a Shaker Hymn).mp3'...
  './audio/12 La Vie En Rose excerpt.mp3'...
  './audio/Jaymes Young - Don''t You Know.mp3'...
  './audio/Labyrinth.wav'...
  './audio/Kendrick Lamar - HUMBLE.mp3'...
  './audio/Pixies __ Where Is My Mind.wav'...
  ...
  };
naud = size(audPaths, 2);
csvOut = 1;

% Spectrogram parameters.
nfft = 8192;
win = hann(nfft);
overlap = 7*nfft/8; % 50% overlap between adjacent spectra.
step = nfft - overlap;
nrows = 500; % Most of the spectrum (representing higher frequencies) is empty.
minThresh = 5;
% Number of time bins of spectrogram to peak pick on each loop. Corresponds
% to peakstep/(step/Fs) seconds covered by each loop. This parameter is
% intended to ensure quiet parts of a song appear among the picked peaks
% too.
peakstep = 130;

% Fingerprint parameters.
timeThresMin = 0.1;
timeThresMax = 1;
pidxThresMin = 0;
pidxThresMax = 150;

% Variable to hold all the fingerprints.
dbase = struct;
dbase.songs = [];
cumuSamp = 0
for i=1:naud
  fprintf('Processing file %d of %d.\n', i, naud);
  % Import wav file.
  [sig, Fs] = audioread(audPaths{i});
  % Spectrogram and peak pick.
  [s, w, t] = spectrogram(sig(:, 1),...
    win, overlap, nfft); % Just left channel.
  s = abs(s(1:nrows, :));
  for row = 1:size(s, 1)
    for col = 1:size(s, 2)
      if s(row, col) <= minThresh
        s(row, col) = 0;
      end
    end
  end
  % Show spectrogram.
  % close all; imagesc(-s); colormap 'gray'; axis xy
  % xlabel('Time (Spectrogram Increment)', 'FontSize', 18);
  % ylabel('Frequency (Spectrogram Increment)', 'FontSize', 18);
  % Pick peaks.
  I = [];
  J = [];
  sind = 1; % Steps over s.
  while sind < size(s, 2)
    sindend = min(sind + peakstep - 1, size(s, 2));
    currs = s(:, sind:sindend);
    thres = quantile(currs(:), 0.99); % Was 0.975.
    currIJ = FastPeakFind(currs, thres);
    currI = currIJ(2:2:end);
    currJ = currIJ(1:2:end) + sind - 1;
    I = [I; currI];
    J = [J; currJ];
    sind = sind + peakstep;
  end
  % hold on; plot(J, I, 'r+'); hold off;
  if csvOut
    [~, fnam, ~] = fileparts(audPaths{i});
    csvwrite(sprintf("./csv/allSongs/%s.csv",...
      fnam), [I J]);
  end

  % Create fingerprints.
  fp = []; % Record current fps just for plot purposes.
  npeak = size(I, 1);
  nfp = 0;
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
          % Make a new entry in dbase.
          dbase.(hash) = struct('timestamp', cumuSamp + ind1(2)*step,...
            'idpiece', audPaths{i});
        else
          % Add to an existing entry in dbase.
          dbase.(hash) = [dbase.(hash);...
            struct('timestamp', cumuSamp + ind1(2)*step,...
            'idpiece', audPaths{i})];
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
  % hold on;
  % for ifp = 1:nfp
  %   pause(0.01);
  %   line([fp(ifp, 2) fp(ifp, 4)], [fp(ifp, 1) fp(ifp, 3)]);
  % end
  % hold off;
  
  % Update cumuSamp.
  dbase.songs = [dbase.songs...
    struct('fnam', audPaths{i}, 'cumuSamp', cumuSamp)];
  cumuSamp = cumuSamp + size(sig, 1)
end

% Save dbase.
save('dbase', 'dbase');
