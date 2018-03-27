fpath = fullfile('Kendrick Lamar - HUMBLE.wav');
x = audioread(fullfile(fpath));
exist(fpath)
beatVarDir = fullfile('.', 'variables');
exist(beatVarDir)
results = beat_tracker_hmm(fpath, beatVarDir);

Fs = 44100;
t = (0:661500)/Fs;
y = zeros(size(x, 1), 1);
click = audioread(fullfile('.', 'click.wav'));
for i = 1:size(results.beats, 1)
    idx = Fs*results.beats(i, 1);
    % v = y(idx, idx + size(click, 1) - 1);
    y(idx:idx + size(click, 1) - 1) = .4*click';
end
plot(y)

z = x(:, 1) + y;
audiowrite(fullfile('.', 'output.wav'), z, Fs);

