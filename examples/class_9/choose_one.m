function [fnam, signoise, Fs] = choose_one(audPaths, clipLength, playTF)

naud = size(audPaths, 2);
audPath = audPaths{randi(naud)}; % = './audio/strings.wav'
% Choose a clip of it at random.
[sig, Fs] = audioread(audPath);
startIdx = randi(size(sig, 1) - Fs*clipLength); % 44656
sig = sig(startIdx:startIdx + Fs*clipLength - 1, 1);
nsamp = size(sig, 1);
% player = audioplayer(sig, Fs);
% play(player);
% Add some noise.
% signoise = sig;
signoise = 0.96*sig + 0.04*2*(rand(nsamp, 1) - 0.5);
if playTF
  player2 = audioplayer(signoise, Fs);
  play(player2);
end
% audiowrite('./audio/probe.wav', signoise, Fs);

[~, fnam, ~] = fileparts(audPath);

end
