% Copyright Tom Collins 3/11/2014

% Test the effect of rounddec.m on the speed of SIAR.m.

% 
pieceName = fullfile('~', 'Data', 'Music', 'beethovenPianoSonatas',...
  'beethovenOp101Mvt1', 'deadSym', 'polyphonic', 'lisp', 'op101_mv1.txt');
D = lispStylePointSet2Matrix(pieceName);

% 
Dp = unique(D(:, [1 3]), 'rows');

% Open a pool of workers.
myPool = parpool('local', 6);
[S, runtime, FRT] = SIAR(Dp, 1, 0);

% Without rounddec, runtime = 112.8630. Size S = 17375.
% With rounddec, runtime = 114.9430. S same size. Hardly any difference in
% runtime.

% Close the pool of workers.
delete(gcp('nocreate'))
