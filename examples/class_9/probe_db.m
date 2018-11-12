% Copyright Tom Collins 3/25/2018

% Probe db for a randomly selected and noisy audio clip.
clipLength = 5; % Was 2.5 for the 3 short files.
rng(12, 'twister'); % Seed the random number generator.

% Load dbase.
load('dbase');

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

% Variable to hold the ground-truth file names and estimates.
nbatch = 5;
n = 50;
for ibatch = 1:nbatch
  fnams = cell(n, 2);
  for i = 1:n
    fprintf("Running snippet %d of %d.\n", i, n);
    % Choose an audio snippet at random and add noise.
    [fnam_gt, signoise, Fs] = choose_one(audPaths, clipLength, 0);
    % player = audioplayer(signoise, Fs);
    % play(player);
    % Use it to query the dbase.
    fnam_est = fp_and_query(signoise, Fs, dbase,...
      0, i, ibatch);
    % See if fnam_gt and fnam_est agree by storing them in fnams.
    fnams{i, 1} = fnam_gt;
    fnams{i, 2} = fnam_est;
  end
  % Save the ground truth and my estimates to file.
  writetable(table(fnams),...
    sprintf("./csv/snippet_batch_%d_gt.csv", ibatch));
end














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
