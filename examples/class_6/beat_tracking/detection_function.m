function [detFunc, new_fr] = detection_function(fpath, nfft, hopsize,...
  win, filterbank, freq_bands, lambda, mvavg_win, frame_length)

save_intermediary_results = 0;
% results_path = fullfile('~', 'Dropbox', 'musicPredictionAppData',...
%       'fftExamples', 'results', '20150923 no channel avg');
% results_path = fullfile('~', 'Dropbox', 'musicPredictionAppData',...
%   'beatTrackingExamples', 'results', '20150810');

% 0. Audio import.
% Import the audio file and sampling rate.
[y, Fs] = audioread(fpath);
% If there are multiple channels in the audio, take the mean of these
% channels.
if size(y, 2) > 1
  y = mean(y, 2);
  % y = y(:, 1);
end
fr = Fs/hopsize;
% Will the frame rate have to be adjusted at the end of calculating this
% feature?
if abs(1/fr - frame_length) > 0.001
  adj_frame_rate = 1;
  new_fr = 1/frame_length;
else
  adj_frame_rate = 0;
  new_fr = fr;
end

% s is a vector [1 442 883 1324... 615196] of the sample inidices to which
% we will hop.
s = 1:hopsize:(size(y, 1) - nfft);
ns = size(s, 2);
% 1. Hop over and window the audio.
fprintf('1. Hop over and window the imported audio.\n');
% I'm going to take those portions of the audio data beginning at each
% s(i), of length nfft, multiply each of them against the Hann window, and
% put each down a column of the matrix X.
X = zeros(nfft, ns);
for i = 1:ns
  X(:, i) = win.*y(s(i):s(i) + nfft - 1);
end
% Save as csv file.
% csvwrite(fullfile(results_path,...
%   '1. Portions of the Audio Data Beginning at Each Sample Hop.csv'), X);
% imagesc(X)
% title('Portions of the Audio Data Beginning at Each Sample Hop')
% xlabel('Sample Hop Index');
% ylabel('Audio Data Index');
% % Save plot to file.
% print('-dpsc', '-append',...
%   fullfile(results_path, 'gettingToDetectionFunction.ps'))

% 2. Calculate the FFTs and power spectra.
fprintf('2. Calculate the FFTs and power spectra.\n');
% Frequency bins.
nbins = nfft/2 + 1; % Or should it be nfft/2?
freq = Fs/2*linspace(0, 1, nbins);
Y = fft(X, nfft)/nfft;
% Calculate the power spectrum.
ps = 2*abs(Y(1:nbins, :));
if save_intermediary_results
  % Save as csv file.
  csvwrite(fullfile(results_path,...
    '2. Power Spectra Beginning at Each Sample Hop.csv'), ps);
  imagesc(ps)
  title('Power Spectra Beginning at Each Sample Hop')
  xlabel('Sample Hop Index');
  ylabel('Power Spectra Index');
  print('-dpsc', '-append',...
    fullfile(results_path, 'gettingToDetectionFunction.ps'))
end

% 3. Apply the filterbank.
fprintf('3. Apply the filterbank.\n');
fps = ps' * filterbank;
if save_intermediary_results
  % Save as csv file.
  csvwrite(fullfile(results_path,...
    '3. Filtered Power Spectra at Each Sample Hop.csv'), fps);
  imagesc(fps)
  title('Filtered Power Spectra at Each Sample Hop')
  xlabel('Filtered Power Spectra Index');
  ylabel('Sample Hop Index');
  print('-dpsc', '-append',...
    fullfile(results_path, 'gettingToDetectionFunction.ps'))
end

% Setup the output variable.
nbands = size(freq_bands, 1);
detFunc = [];
% detFunc = zeros(ns + mvavg_win - 1, nbands);

for iband = 1:nbands
  fprintf('Performing flux calculations for band %d of %d.\n',...
    iband, nbands);
  % Hop over the audio to compute the flux for this frequency band.
  min_f = freq_bands(iband, 1);
  max_f = freq_bands(iband, 2);
  % Find the corresponding frequency bin in Hz.
  [~, min_ind] = min(abs(freq - min_f));
  [~, max_ind] = min(abs(freq - max_f));
  % Find the corresponding frequency band of the filterbank.
  min_ind = find(filterbank(max([min_ind, 2]), :));
  % filterbank only uses frequencies up to 16.75 kHz (bin 778).
  [~, max_ind] = max(filterbank(min([778, max_ind]), :));
  
  % This could still be pulled out of the loop, but I'll leave it in for
  % now.
  
  % 4. Calculate the log-filtered spectra. Begin by restricting the
  % filtered power spectra to the current frequency band.
  fps_restricted = fps(:, min_ind:max_ind);
  lfps = log10(lambda * fps_restricted + 1);
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf('4. Band-Limited Log-Filtered Spectra Band %d.csv',...
      iband)), lfps);
    imagesc(lfps)
    title(sprintf('4. Band-Limited Log-Filtered Spectra Band %d.csv',...
      iband));
    xlabel('Band-Limited Log-Filtered Power Spectra Index');
    ylabel('Sample Hop Index');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  % 5. Compute the flux.
  difforder = 1;
  Sdif = lfps - [lfps(1:difforder,:); lfps(1:end-difforder,:)];
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf('5. Spectral Flux Band %d.csv', iband)), Sdif);
    imagesc(Sdif)
    title(sprintf('Spectral Flux Band %d', iband));
    xlabel('Spectra Index');
    ylabel('Sample Hop Index');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  % 6. Halfwave rectification.
  Sdif = (Sdif + abs(Sdif))/2;
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf('6. Halfwave-Rectified Spectral Flux Band %d.csv',...
      iband)), Sdif);
    imagesc(Sdif)
    title(sprintf('Halfwave-Rectified Spectral Flux Band %d', iband));
    xlabel('Spectra Index');
    ylabel('Sample Hop Index');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  % 7. Sum over frequency bands.
  DetFunc = sum(Sdif,2);
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf(['7. Band-Summed Halfwave-Rectified Spectral Flux '...
      'Band %d.csv'], iband)), DetFunc);
    plot(DetFunc);
    title(sprintf(['Band-Summed Halfwave-Rectified Spectral Flux Band '...
      '%d'], iband));
    xlabel('Sample Hop Index');
    ylabel('Energy (Arbitrary Units)');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  % 8. Apply moving average.
  Z = zeros(ns, 1);
  DetFuncPreAvg = [DetFunc; zeros(mvavg_win - 1, 1)];
  
  % Version post-10/8/2015.
  for i = 1:ns
    Z(i) = mean(DetFuncPreAvg(i:i + mvavg_win - 1));
  end
  % Version pre-10/8/2015.
  % Z(1:mvavg_win - 1) = mean(DetFunc(1:mvavg_win - 1));
  % for i = mvavg_win:ns
  %   Z(i) = mean(DetFuncPreAvg(i - mvavg_win + 1:i));
  % end
  
  DetFunc = DetFunc - [0; Z(1:end-1)];
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf('8. Moving-Average Spectral Flux Band %d.csv', iband)),...
      DetFunc);
    plot(DetFunc);
    title(sprintf('Moving-Average Spectral Flux Band %d', iband));
    xlabel('Sample Hop Index');
    ylabel('Energy (Arbitrary Units)');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  % 9. Adjust framerate of features.
  if adj_frame_rate
    DetFunc = change_frame_rate_simple(DetFunc, round(1000*fr)/1000,...
      1/frame_length);
  end
  if save_intermediary_results
    % Save as csv file.
    csvwrite(fullfile(results_path,...
      sprintf('9. Adjusted Frame Rate Band %d.csv', iband)), DetFunc);
    plot(DetFunc);
    title(sprintf('9. Adjusted Frame Rate Band %d', iband));
    xlabel('Resampled Hop Index');
    ylabel('Energy (Arbitrary Units)');
    print('-dpsc', '-append',...
      fullfile(results_path, 'gettingToDetectionFunction.ps'))
  end
  
  detFunc = [detFunc DetFunc];
  
end

end
