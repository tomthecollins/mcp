% Copyright Tom Collins 30/7/2013

% Running a pattern discovery algorithm on Beethoven's Piano Sonata no.1
% op.2 no.1 mvt.3.

% Project paths.
coreRoot = fullfile('~', 'repos', 'collCodeInit', 'private', 'core',...
  'matlab', 'pattDisc');
dataRoot = fullfile(coreRoot, 'examples', 'exampleData');
resultPath = fullfile(coreRoot, 'examples', 'exampleResults');

% Add the pattDisc function paths.
addpath(fullfile(coreRoot, 'analysis'),...
  fullfile(coreRoot, 'filter'),...
  fullfile(coreRoot, 'formatUtils'),...
  fullfile(coreRoot, 'patternDiscovery'),...
  fullfile(coreRoot, 'patternMatching'),...
  fullfile(coreRoot, 'rating'),...
  fullfile(coreRoot, 'thirdParty', 'matlabCentral', 'count'),...
  fullfile(coreRoot, 'thirdParty', 'matlabCentral', 'peakfinder'));

% Set a projection, load a piece, and apply the projection.
fileName = fullfile(dataRoot, 'sonata01-3.txt');
compID = 'beet_';
projectionIdx = [1 3];
k = size(projectionIdx, 2);
[~, fStub, ~] = fileparts(fileName);
D = lispStylePointSet2Matrix(fileName);
D2 = unique(D(:, projectionIdx), 'rows');

% Algorithm parameters.
r = 1;
compactThresh = 9/10;
cardinaThresh = 8;
regionType = 'convex hull';
similarThresh = .5;
similarFunc = 'normalised matching score';
similarParam = 'pitchindependent';
ratingField = 'rating';

% Run SIARCT.
[S1, runtime1, FRT1] = SIARCT(D2, r, compactThresh,...
  cardinaThresh, regionType);
SIARCT_fName = fullfile(resultPath, [compID fStub '_SIARCT.mat']);
save(SIARCT_fName, 'S1', 'runtime1', 'FRT1');
% load(SIARCT_fName, 'S1', 'runtime1', 'FRT1');

% Rate the output.
% projIdx2 indicates which columns of D2 to use to calculate an empirical
% mass function. Here, the full point set has dimensions for ontime and
% morphetic pitch. We wish to focus on morphetic pitch and ontime is
% excluded automatically, so let projIdx2 = 2.
projIdx2 = 2;
tfPitch = 1;
tStart = tic;
warning off
S2 = rateOutput(S1, D2, tfPitch, projIdx2);
warning on
runtime2 = toc(tStart);
rated_fName = fullfile(resultPath, [compID fStub '_rated.mat']);
save(rated_fName, 'S2', 'runtime2');
% load(rated_fName, 'S2', 'runtime2');

% Now categorise by similarity and find extra inexact occurrences.
[S3, runtime3, FRT3] = SIARCT_CFP(D2, r,...
  compactThresh, cardinaThresh, regionType, similarThresh, similarFunc,...
  similarParam, ratingField, S2, runtime1 + runtime2, FRT1);
SIARCT_CFP_fName = fullfile(resultPath, [compID fStub '_SIARCT-CFP.mat']);
save(SIARCT_CFP_fName, 'S3', 'runtime3', 'FRT3');
% load(SIARCT_CFP_fName, 'S3', 'runtime3', 'FRT3');

% For saving, and representing as a text file.
includeExtraInfo = 1;
fNameOut = fullfile(resultPath, [compID fStub '_SIARCT-CFP.txt']);
SoutStyleStruct2PattAllOccAll(S3, D2, fNameOut, includeExtraInfo)

% Save information about parameters and processing route.
params.piece.fileName = fileName;
params.piece.compID = compID;
params.piece.projectionIdx = projectionIdx;
params.proc = {'SIARCT' SIARCT_fName {'S1' 'runtime1' 'FRT1'};...
  'rateOutput' rated_fName {'S2', 'runtime2'};...
  'SIARCT_CFP' SIARCT_CFP_fName {'S3' 'runtime3' 'FRT5'}};
params.SIARCT.r = r;
params.SIARCT.compactThresh = compactThresh;
params.SIARCT.cardinaThresh = cardinaThresh;
params.SIARCT.regionType = regionType;
params.rateOutput.projIdx2 = projIdx2;
params.rateOutput.tfPitch = 1;
params.SIARCT_CFP.similarThresh = similarThresh;
params.SIARCT_CFP.similarFunc = similarFunc;
params.SIARCT_CFP.similarParam = similarParam;

% Visualise some of the discovered patterns using the function plotPattern,
% and pressing a button to advance from one pattern occurrence to the next.
plotPattern(S3, D2, 1)
