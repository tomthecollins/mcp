function winningSongNam = fp_and_query(signoise, Fs, dbase,...
  plotTF, csvOut, snippetBatchNo)

% Spectrogram parameters.
nfft = 8192;
win = hann(nfft);
overlap = 7*nfft/8; % 25% overlap between adjacent spectra.
step = nfft - overlap;
nrows = 500; % Most of the spectrum (representing higher frequencies) is empty.
minThresh = 5;

% Fingerprint parameters.
timeThresMin = 0.1;
timeThresMax = 1;
pidxThresMin = 0;
pidxThresMax = 150;

% Spectrogram and peak pick.
[s, w, t] = spectrogram(signoise, win, overlap, nfft);
s = abs(s(1:nrows, :));
for row = 1:size(s, 1)
  for col = 1:size(s, 2)
    if s(row, col) <= minThresh
      s(row, col) = 0;
    end
  end
end
if plotTF
  % Show spectrogram.
  close all; imagesc(-s); colormap 'gray'; axis xy;
  xlabel('Time (Spectrogram Increment)', 'FontSize', 18);
  ylabel('Frequency (Spectrogram Increment)', 'FontSize', 18);
end
% Pick peaks.
thres = quantile(s(:), 0.975); % Was 0.95.
IJ = FastPeakFind(s, thres);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT BOUNDARY CASE HANDLING %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if size(IJ, 1) == 0
  % No peaks gave rise to valid fingerprints. Return.
  winningSongNam = "";
  return;
end
I = IJ(2:2:end);
J = IJ(1:2:end);
if csvOut
  csvwrite(sprintf("./csv/snippet_batch_%d/%d.csv",...
    snippetBatchNo, csvOut), [I J]);
end
% V = jsonencode([J I]);
if plotTF
  hold on; plot(J, I, 'r+'); hold off;
end
  
% Create fingerprints.
matches = [];
nmatch = 0;
fp = [];
nfp = 0;
npeak = size(I, 1);
for ii = 1:npeak
  % fprintf("Working with peak %d of %d.\n", ii, npeak);
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
if plotTF
  % Plot some of the fingerprints.
  hold on;
  for ifp = 1:nfp
    pause(0.001);
    line([fp(ifp, 2) fp(ifp, 4)], [fp(ifp, 1) fp(ifp, 3)]);
  end
  hold off;

  % Plot of matches.
  figure; plot([matches.dbts], [matches.pbts], '.b');
  xlabel('Time in Database (Samples)', 'FontSize', 18);
  ylabel('Time in Query (Samples)', 'FontSize', 18);
  ylim([1 max([matches.pbts]) + 500])
  for i = 1:size(dbase.songs, 2)
    line([dbase.songs(i).cumuSamp + 1 ...
          dbase.songs(i).cumuSamp + 1],...
         [0 max([matches.pbts] + 1000)],...
         'Color', 'k', 'LineStyle', '--');
  end
  % xlim([5.6e5 6.5e5])
end

% Histogram.
if plotTF
  figure;
  hist([matches.dbts] - [matches.pbts], 1000)
  xlabel('Transformed Time in Database (Samples)', 'FontSize', 18);
  ylabel('Number of Matches', 'FontSize', 18);
end
[counts, centers] = hist([matches.dbts] - [matches.pbts], 1000);
[C, I] = max(counts);
if plotTF
  for i = 1:size(dbase.songs, 2)
    line([dbase.songs(i).cumuSamp + 1 ...
          dbase.songs(i).cumuSamp + 1],...
         [0 C],...
         'Color', 'k', 'LineStyle', '--');
  end
end

% Determine name of winning song from max in histogram.
winningSample = centers(I);
winningSongIdx = find(winningSample >= [dbase.songs.cumuSamp], 1, 'last');
% THIS IS THE SONG 'SHAZAM' THINKS IS IN THE CLIP!
winningSongNam = dbase.songs(winningSongIdx).fnam;
[~, winningSongNam, ~] = fileparts(winningSongNam);

end
