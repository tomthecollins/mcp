% Copyright Tom Collins 9/6/2014

% Welcome to PattDisc-Jun2014! This example file loads symbolic
% representations of five pieces of music, runs an algorithm called
% SIARCT_CFP_motif_sectn on them (Collins, Arzt, Flossmann, & Widmer,
% 2013), saves the output, and then evaluates this output according to
% metrics set out in the 2013 Music Information Retrieval Evaluation
% EXchange task on Discovery of Repeated Themes and Sections:
% 
% http://www.music-ir.org/mirex/wiki/2013:Discovery_of_Repeated_Themes_%26_Sections
% 
% The code below is written to be executed one chunk at a time. For the
% longer pieces, such as the Chopin, it will take approx. 30 minutes on a
% standard desktop machine. Limiting the for loops to the shorter pieces
% 1 and 4 (by Bach and Gibbons) will produce results for a couple of pieces
% more quickly. The pattern discovery functions in this package have been
% designed for use with Matlab's Parallel Computing Toolbox, but are
% compatible with Matlab in the absence of this toolbox. If you do not
% have the Parallel Computing Toolbox installed, then best to avoid lines
% 67 and 102 in this file, in case of error.

% For more information about parameters, see the documentation at the top
% of the function SIARCT_CFP_sectn_motif.m. It runs the algorithm
% SIARCT_CFP twice; once with parameters aimed toward the discovery of
% repeated sections, and again with parameters aimed toward the discovery
% of smaller repetitive elements, such as motifs, themes, sequences.

% There are more functions in pattDisc than the function exemplified below
% (e.g., some algorithms from Meredith, Lemström, & Wiggins, 2002), so
% users are encouraged to open some of the other m-files to discover
% further possibilities.

% REFERENCES
% Tom Collins, Andreas Arzt, Sebastian Flossmann, and Gerhard Widmer.
% SIARCT-CFP: improving precision and the discovery of inexact musical
% patterns in point-set representations. In Proceedings of the
% International Symposium on Music Information Retrieval, pages 549-554,
% Curitiba, Brazil.
% 
% David Meredith, Kjell Lemström, and Geraint A. Wiggins. Algorithms for
% discovering repeated patterns in multidimensional representations of
% polyphonic music. Journal of New Music Research, 31(4):321–345, 2002.

% Define some paths, beginning with wherever you have put the pattDisc
% package.
coreRoot = fullfile('~', 'blah',...
  'PattDisc-Jun2014');
dataRoot = fullfile(coreRoot, 'examples', 'exampleData');
resultPath = fullfile(coreRoot, 'examples', 'exampleResults');
pattDiscOut = fullfile(coreRoot, 'examples', 'examplePattDiscOut');
compIDs = {'bach_' 'beet_' 'chop_' 'gbns_' 'mzrt_'};
pieceNames = {'wtc2f20' 'sonata01-3' 'mazurka24-4' 'silverswan'...
  'sonata04-2'};
bib = [4 3 3 4 3]; % Beats per bar for each piece (used for filtering).

% Add the pattDisc function paths.
addpath(fullfile(coreRoot, 'analysis'),...
  fullfile(coreRoot, 'filter'),...
  fullfile(coreRoot, 'formatUtils'),...
  fullfile(coreRoot, 'patternDiscovery'),...
  fullfile(coreRoot, 'patternMatching'),...
  fullfile(coreRoot, 'rating'),...
  fullfile(coreRoot, 'thirdParty', 'matlabCentral', 'count'),...
  fullfile(coreRoot, 'thirdParty', 'matlabCentral', 'peakfinder'));

% Open a pool of workers.
myPool = parpool('local', 6);

% Run algorithm across the pieces.
npiece = size(pieceNames, 2);
for ipiece = 1:npiece
  % Load piece. You can also use csvread if you have your music data in csv
  % format, but beware of rounding errors.
  piecePath = fullfile(dataRoot, [pieceNames{ipiece} '.txt']);
  D = lispStylePointSet2Matrix(piecePath);
  % Setup the parameters struct.
  params = struct;
  params.pieceName = [compIDs{ipiece} pieceNames{ipiece}];
  params.pattDiscOut = pattDiscOut;
  % Set a projection on to ontime and morphetic pitch number.
  params.projectionIdx = [1 3];
  % Set parameters for a filter that focuses on smaller repetitive elements
  % close to the beginning of the piece or close to the beginning of large
  % repeated sections.
  fbeps = struct;
  fbeps.occIn1stPiece = 1;
  fbeps.occIn1stSectn = bib(ipiece);
  fbeps.topN = 5;
  fbeps.transOrOcc = 'translators';
  params.filterByEarlyPieceSectn = fbeps;
  % Run the algorithm.
  [S, runtime, FRT, params] = SIARCT_CFP_sectn_motif(D, params);
  % Intermediary and final results of the algorithm are saved to the
  % location specified by params.pattDiscOut. The verbose file names
  % indicate parameter settings. The downside of this: it is cumbersome to
  % deal with long file names. The upside: if the algorithm finds an
  % existing file with the appropriate name, it will load previous results,
  % rather than calculating the same results all over again.
end

% Close the pool of workers.
delete(gcp('nocreate'))

% Evaluate the algorithm's output. This chunk of code creates a
% comma-separated text file containing the evaluation results.
similarThresh = .75;
similarFunc = 'cardinality score';
% Open a results text file.
fid = fopen(fullfile(resultPath, 'results.txt'), 'a');
% Print column headings.
fprintf(fid, ['Piece Idx, Piece Name, Alg Idx, Alg Name, '...
  'n_P, n_Q, P_est, R_est, F1_est, '...
  'P_occ(c=.75), R_occ(c=.75), F_1occ(c=.75), '...
  'P_3, R_3, TLF_1\n']);
% Evaluate the output for each piece.
for ipiece = 1:npiece
  % Load ground truth for the current piece. This is from the Johannes
  % Kepler University Patterns Development Database (JKUPDD-Aug2013).
  GTf = pattAllOccAll2struct(fullfile(dataRoot,...
    ['patterns_' pieceNames{ipiece} '.txt']));
  % Put in an appropriate struct for passing to the evaluation functions.
  GT = struct;
  GT.details = GTf;
  GT.k = size(params.projectionIdx, 2); % Dimension of the point set.
  % Load output for the current piece. Although above, pattern discovery
  % was run on ontime and morphetic pitch, this has already been converted
  % in a late stage of SIARCT_CFP_sectn_motif to ontime and MIDI note
  % number, for the purposes of standardised evaluation. The next seven
  % lines of code find that file.
  contents = dir(fullfile(pattDiscOut,...
    [compIDs{ipiece} pieceNames{ipiece} '*.txt']));
  fnl = zeros(size(contents, 1), 1);
  for fni = 1:size(fnl, 1)
    fnl(fni) = size(contents(fni).name, 2);
  end
  [~, relIdx] = max(fnl);
  S = pattAllOccAll2struct(fullfile(pattDiscOut, contents(relIdx).name));
  % Print algorithm index and name, and piece index and name.
  fprintf(fid, '%d, ', 1);
  fprintf(fid, '%s, ', 'SIARCT_CFP_sectn_motif');
  fprintf(fid, '%d, ', ipiece);
  fprintf(fid, '%s, ', [compIDs{ipiece} pieceNames{ipiece}]);
  % Print the size of ground truth and algorithm output.
  fprintf(fid, '%d, %d, ', size(GTf, 2), size(S, 2));
  % Calculate and print establishment precision and recall.
  [p_est, r_est, simMtx] = estPrecRecMat(GT, S, similarFunc);
  if p_est == 0 && r_est == 0
    f1_est = 0;
  else
    f1_est = 2*p_est*r_est/(p_est + r_est);
  end
  fprintf(fid, '%6.5f, %6.5f, %6.5f, ', p_est, r_est, f1_est);
  % Calculate and print occurrence precision and recall for c = .75.
  [p_occ, r_occ, ~, ~] = occPrecRecMat(GT, S, similarThresh, similarFunc);
  if p_occ == 0 && r_occ == 0
    f1_occ = 0;
  else
    f1_occ = 2*p_occ*r_occ/(p_occ + r_occ);
  end
  fprintf(fid, '%6.5f, %6.5f, %6.5f, ', p_occ, r_occ, f1_occ);
  % Calculate and print three-layer precision and recall.
  [p3, r3, TLF] = threeLayerF1({GT.details(:).occurrences}, {S.occurrences});
  fprintf(fid, '%6.5f, %6.5f, %6.5f\n', p3, r3, TLF);
end
fclose(fid);

return

% As a quick example, let us get an overview of the performance of the
% algorithm on the last piece (ipiece = 5), since the variable simMtx is
% still in the workspace. This matrix shows the ground truth patterns as
% rows and the algorithm output patterns as columns. If there is a reddish
% value in element (i, j) of simMtx, then (at least) one of the occurrences
% of algorithm output pattern j is really similar to (at least) one of the
% occurrences of ground truth pattern i.
imagesc(simMtx)
% Because there are reddish values across rows 1 and 5-9, we see that the
% algorithm has done a good job of returning patterns similar to ground
% truth items 1 and 5-9. It has done less well for patterns 2-4.
