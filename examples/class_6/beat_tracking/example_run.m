fpath = fullfile('/Volumes', 'M-Z', 'toc215', 'Teaching', '397',...
    'audio', 'Pixies __ Where Is My Mind.wav');
exist(fpath)
beatVarDir = fullfile('.', 'variables');
exist(beatVarDir)
results = beat_tracker_hmm(fpath, beatVarDir);
