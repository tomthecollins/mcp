function MM = sensoryDissonance(fName)

% Copyright Tom Collins 20/2/2005

% This function reads a wav file and computes the instantaneous acoustic
% dissonance according to Plomp and Levelt (1965) and Helmholtz

% REFERENCE
%  Helmholtz, Hermann von, On the sensations of tone as the physiological
%   basis for the theory of music, trans. Alexander J. Ellis, fourth
%   German edition, 1887 (London: Longmans, Green, and Co. 1895).
%  Plomp, R., and Levelt, W. J. M., Tonal consonance and critical
%   bandwidth, in Journal of the Acoustical Society of America, 38
%   (1965), 548-560.

% INPUT
%  fName is the path and name to an audio file in wav format.

% EXAMPLE INPUT
%  fName = 'tensionExample1.wav';
%  MM = sensoryDissonance(fName);
%  plot(MM(:, 2), MM(:, 3));
%  xlabel('Time (s)')
%  ylabel('Sensory Dissonance (Arbitrary Scale)')

% Parameters.
npartials = 70;
ampthresh = 1e-9;
nfft = 4096;

% Load the wav file and create the amplitude vector to be tested, tfl.
[snd, Fs, ~] = audioread(fName);
siz = size(snd);
% len = siz(1)/Fs;
tfl = snd(1:siz(1), 1);
if siz(2) > 1
  tfr = snd(1:siz(1), 2);
end

% Get Plomp and Levelt's (1965) curve for the extent to which a pair of
% pure tones cause beating.
[palxvec, palyvec] = plompLeveltCurve;

% Master matrix contains all the sampling points for any given wav file,
% the corresponding time in seconds, and sensory dissonance factors.
i = 1;
counter = nfft;
while counter <= siz(1)
  MM(i, 1) = nfft/2 + (i - 1)*nfft/4;
  counter = MM(i, 1) + .75*nfft;
  i=i+1;
end
% Time in seconds.
[m, ~] = size(MM);
i = 1;
while i <= m
  MM(i, 2) = MM(i, 1)/Fs;
  i=i+1;
end

% Calculate the sensory dissonance factors. First create a Hamming window
% and multiply it against a segment of the signal.
w = hamming(nfft);
t = 1;
% Name these variables again because m, n are overwritten below.
[mits, ~] = size(MM);

while t <= mits
  % Progress update.
  if t/500 == round(t/500)
    fprintf('Making progress. Time point %d of %d.\n', t, mits)
  end
  win = tfl(MM(t, 1) - (nfft/2 - 1):MM(t, 1) + (nfft/2));
  [m, ~] = size(win);
  % I think there is redundancy and unnecessary iteration here. It gets
  % the product of the Hamming window and signal.
  i = 1;
  while i <= m
    modwin(i) = w(i)*win(i);
    i=i+1;
  end
  % Perform the fast Fourier transform.
  fftwin = fft(modwin, nfft);
  pwin = fftwin.*conj(fftwin)/nfft;
  rtfl = pwin(1:(nfft/2 + 1));
  freq = 0:22050/(nfft/2):22050;
  % rtfl contains 2049 elements, spanning a frequency range 0 to
  % 22050 Hz in chunks of 22050/2048 = 10.8 Hz. The frequency associated
  % with element i is 22050/2048*(rtfl(i) - 1).
  
  % Pick peaks from rtfl.
  peak = [];
  if sum(rtfl) > 0
    i = 1;
    j = 1;
    while i < nfft/2
      if rtfl(i) < rtfl(i + 1)
        % Unnecessary if condition?
        while rtfl(i) < rtfl(i + 1)
          i=i+1;
          if i > nfft/2 - 2
            break
          end
          % Changed this to nfft/2 - 2 because the next section of code
          % tries to access rtfl(nfft/2 + 2), and we are not so interested
          % in amplitudes at 22 kHz.
        end
        % Define a variable peak that contains the indices and frequencies
        % of each peak in the power spectrum.
        peak(j, 1) = 22050/(nfft/2)*(i - 2);
        peak(j, 2) = rtfl(i - 1);
        peak(j, 3) = 22050/(nfft/2)*(i - 1);
        peak(j, 4) = rtfl(i);
        peak(j, 5) = 22050/(nfft/2)*i;
        peak(j, 6) = rtfl(i + 1);
        j=j+1;
      else
        while rtfl(i) > rtfl(i + 1)
          i=i+1;
          if i > nfft/2
            break
          end
        end
        % trough = [i rtfl(i)];
      end
    end
  end
  
  % Interpolate each peak to estimate the frequency corresponding to the
  % maximum in the power spectrum.
  % THIS NEEDS DOUBLE CHECKING, IT SEEMS HEAVY-HANDED.
  [m, ~] = size(peak);
  j = 1;
  while j <= m
    a = peak(j, 1);
    A = peak(j, 2);
    b = peak(j, 3);
    B = peak(j, 4);
    c = peak(j, 5);
    C = peak(j, 6);
    alpha = ((C - B)/((c - b)*(c - a)))...
      - ((A - B)/((a - b)*(c - a)));
    beta = (alpha*(b ^2 - a^2) + (A - B))/(a - b);
    gamma = A - alpha*a^2 - beta*a;
    parapeak(j, 1) = b - .5*(((b - a)^2*(B - C)) - ((b - c)^2*(B - A)))...
      /(((b - a)*(B - C)) - ((b - c) * (B - A)));
    parapeak(j, 2) = alpha*parapeak(j, 1)^2 + beta*parapeak(j, 1)...
      + gamma;
      j=j+1;
  end
  
  % Sort the peaks by power in the spectrum, and remove peaks that are
  % below the amplitude threshold.
  if m > 0
    srtnpeak = flipud(sortrows(parapeak, 2));
    nearpeak = srtnpeak(1:npartials, :);
    for i = 4:npartials
      if nearpeak(i, 2) < ampthresh
        break
      end
    end
    sortpeak = nearpeak(1:i, :);
  else
    sortpeak = [];
  end
  
  vadi = [];
  % The peaks are picked and ready to apply Hutchinson and Knopoff.
  [m, n] = size(sortpeak);
  k = 1; % Increment over contributions to sensory dissonance.
  for i = 1:m
    for j = i + 1:n
      df = abs(sortpeak(i, 1) - sortpeak(j, 1)); % Frequency difference.
      % Calculate the critical bandwidth. (The closer the two frequency
      % components, the greater the contribution to sensory dissonance,
      % subject to further processing by the Plomp and Levelt curve.)
      cbw = 1.72*(((sortpeak(i, 1) + sortpeak(j, 1))/2)^.65);
      mbw = 1.2*cbw; % Modified bandwidth.
      dfdc = df/cbw;
      if df < mbw
        h = fix(dfdc/1.2*205); % This is giving an index into the curve.
        if h < 1
          h = 1;
        end
        % I think this is some attempt at interpolation, but it really
        % needs checking!
        l = (palyvec(h) - palyvec(h + 1))/(palxvec(h) - palyvec(h + 1));
        c = palyvec(h) - palxvec(h)*l;
        pal = l*dfdc + c;
        fac = pal;
      else
        fac = 0;
      end
      % The variable vadi contains the summands.
      vadi(k) = fac*sortpeak(i, 2)*sortpeak(j, 2); %...
        % /(sortpeak(i, 2)^2 + sortpeak(j, 2)^2);
      k=k+1;
    end
  end
  % Previously tried to normalize by the overall power in the spectrum.
  % if ~isempty(sortpeak)
  %   denom = sortpeak(:, 2)'*sortpeak(:, 2);
  % else
  %   denom = 1;
  % end
  MM(t, 3) = sum(vadi); %/denom;
  t=t+1;
end
% plot(MM(:, 2), MM(:, 3));

end
