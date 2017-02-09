function [S, runtime, FRT, params] = SIARCT_CFP_sectn_motif(D, params)

% Copyright Tom Collins 6/3/2014

% Given a dataset D, this function:
% (1) Runs the SIARCT algorithm, returning compact subsets of maximal
%  translatable patterns (MTP) that occur in D, and for which a conjugate
%  generating vector lies on or within the first r superdiagonals of the
%  similarity array for D;
% (2) Categorises the patterns into exemplars and other patterns that have
%  higher-than-threshold similarity to an exemplar;
% (3) Finds other inexact occurrences of each exemplar in the point set.

% It is assumed that D is in lexicographic order. Otherwise
% D = sortrows(D);
% can be used to achieve this. Alternatively,
% D = unique(D, 'rows');
% will also remove duplicate datapoints.

% WHERE SHOULD BIB GO?!

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  params is an optional parameter struct with the following fields:
%   pieceName is a string containing the name/ID of the piece that D
%    represents (default value 'piece').
%   pattDiscOut is a string containing a folder path (default value '~').
%    Intermediate and final output is written to this location. The
%    contents of the location are also checked to see if pre-existing
%    calculations can be loaded (to avoid unnecessary recalculation).
%   projectionIdx is a k-element vector (default value [1 3]) specifying
%    the dimensions on to which some original point set was projected in
%    order to create D.
%   r is a positive integer between 1 and n - 1 (default r = 1), giving the
%    number of superdiagonals of the similarity array for D that will be
%    used.
%   quick takes the value 0 (default) if calculation of maximal
%    translatable patterns is done with an error tolerance of 1e-5, and 1
%    if calculation is done with a lower error tolerance. The default
%    setting is recommended to avoid rounding errors.
%   cardinaThresh is a five-element vector of positive integers (default
%    value [2000; 8; 30; 18; 60]). The first element specifies that if
%    n <= 2000, discovered 'motif' sets must have cardinality between 8 and
%    29 (second and third elements), and discovered 'section' sets must
%    have cardinality of at least 30. If n > 2000, discovered 'motif' sets
%    must have cardinality between 18 and 59 (fourth and fifth elements),
%    and discovered 'section' sets must have at cardinality of at least 60.
%   compactThresh is a two-element vector (default value [.95 1]) with
%    values in (0, 1], giving the minimum compactness a 'motif' or
%    'section' set must have in order to be included in the output.
%   regionType is a two-element cell of strings (default value
%    {'lexicographic' 'convex hull'} equal to either 'lexicographic' or
%    'convex hull'. These strings indicate which definition of region
%    should be used for calculating the compactness of subsets in the point
%    set.
%   filterByCardinality is a two-element vector of positive integers
%    (default value [NaN cardinaThresh(3) - 1]) that can be used to filter
%    out 'section' or 'motif' sets with more than the stated number of
%    points.
%   filterByUniquePitchClasses is a struct (default empty) with fields for
%    filtering out 'motif' sets that have an insufficient number of unique
%    pitch classes. The possible fields are:
%     pitchDim is a positive integer (default value 2) specifying the
%      dimension (column) of D that contains the pitch information to be
%      tested.
%     mUnqPitch is a positive integer (default value 3) specifying the
%      minimum number of unique pitch classes that a point set must have in
%      order to remain in the output.
%    nPitchInOct is a positive integer (default value 7) specifying the
%     number of pitch classes in an octave. It has default value 7 because
%     the default projection is on to scale step rather than the twelve
%     semitones.
%   filterByDuration is a two-element vector of positive integers (default
%    value [NaN NaN]) that can be used to filter out sets that have too
%    short a duration. If the first ontime of a set is x0 and the last
%    ontime is x1, then the set would be excluded if x1 - x0 is less than
%    the threshold.
%   rateOutput is a struct (default .projIdx2 = 2 and .tfPitch = 1) that
%    defines two variables for rating the perceptual salience of discovered
%    sets according to the formula of Collins, Laney, Willis, and
%    Garthwaite (2011). When distributions are formed in order to calculate
%    the likelihood of pattern occurrences, projIdx2 specifies along which
%    dimension of the point set distributions are formed, and tfPitch takes
%    the value one if these distributions consist of pitches, and zero
%    otherwise (e.g., for durations).
%   filterOutOverlaps is a vector of structs (default .covPercent = .95 and
%    .intersectEdit = 1) that causes discovered sets to be filtered out or
%    edited in the event that their occurrences overlap with one another
%    too much (in the sense of sharing points). covPercent is a real number
%    in (0, 1]. The ratio of coverage to the product of cardinality and
%    occurrences is calculated. If the result is greater than or equal to
%    covPercent, the pattern remains, otherwise it may be filtered out. 
%    intersectEdit takes the value one if overlapping patterns with two
%    occurrences should be intersected and the overlap removed, prior to
%    placing in the output struct. Placement in the output struct only
%    occurs if the resulting pattern still has enough notes compared with
%    the relevant value from cardinaThresh.
%   filterByEarlyPieceSectn is a struct (default .occIn1stPiece = 1,
%    .occIn1stSectn = 4, .topN = 5, and .transOrOcc = 'translators') for
%    controlling whether output 'motif' sets should be limited to those
%    that are either early in the whole set, or early relative to a
%    'section' set:
%     occIn1stPiece is a positive real number, referring to the first
%      occIn1stPiece beats of a piece beyond its first ontime.
%     occIn1stSectn is a positive real number, referring to an offset of
%      occIn1stPiece beats from the beginning of a 'section' set.
%     topN is a positive integer, determining how many 'section' sets are
%      considered.
%     transOrOcc is an optional string argument, taking the value
%      'translators' or 'occurrences'. It specifies which field is used to
%      determine the earliest occurrence of an occurrence set. If
%      'translators' is used (default), then the earliest occurrence will
%      be the earliest translationally exact occurrence. If 'occurrences'
%      is used, then the earliest occurrence may be an inexact occurrence.
%   categorisation is a vector of structs (default first element
%    .similarThresh = .85, .similarFunc = 'normalised matching score',
%    .similarParam = 'pitchindependent', .ratingField = 'rating', and
%    default second element .similarThresh = 1/3, .similarFunc =
%    'normalised matching score', .similarParam = 'pitchindependent',
%    .ratingField = 'rating'). It controls when discovered sets are
%    categorised, so that the prototypical occurrence set remains in the
%    output, but other discovered sets that are considered too similar to
%    the prototype are removed from consideration (but stored in the .mat
%    file output). The fields, in a bit more detail, are:
%     similarThresh is a value in [0, 1). If the similarity of the current
%      highest-rated set and some other set is greater than this threshold,
%      then the second set will be categorised as an instance of the 
%      prototype. Otherwise the second set may become a prototype in a 
%      subsequent step.
%     similarFunc is a string indicating which function should be used for
%      calculating the symbolic music similarity, either
%      'cardinality score' or 'normalised matching score'.
%     similarParam is an optional argument. If similarFunc = 'cardinality
%      score', then similarParam takes one of two values (one if
%      calculation of cardinality score allows for translations, and zero
%      otherwise). If similarFunc = 'normalised matching score', then
%      similarParam takes a string value ('normal', 'pitchindependent',
%      'tempoindependent', or 'tempoandpitchindependent', see
%      fpgethistogram2 for details).
%     ratingField is an optional string indicating which field of the
%      struct should be used to order the repeated patterns prior to
%      categorisation.
%   inexactThresh is a two-element vector of reals in [0, 1) (default value
%    [.9 .8]) that can be used to include extra, inexact occurrences of
%    sets in the final output. If the similarity of some inexact occurrence
%    of a pattern is greater than this threshold, then it will be included
%    in the output.
%   savePastRating is a logical indicating whether the output should be
%    saved as mat files after rating (step 6). Steps after rating are
%    usually quick to calculate, and so it makes sense to reduce the burden
%    on storage and not save these results.
%   savePattAllOccAll is a logical indicating whether the output should be
%    saved as a text file with all pattern occurrences listed.
%   convertMPNpatt2MNN is a logical indicating whether the output should be
%    converted from morphetic pitch numbers to MIDI note numbers (for
%    standardised evaluation purposes).

%% Assign default values to any missing parameters.
% tStart = tic;
if nargin < 2
  params = struct;
end
if ~isfield(params, 'pieceName')
  params.pieceName = 'piece';
end
if ~isfield(params, 'pattDiscOut')
  params.pattDiscOut = '~';
end
if ~isfield(params, 'projectionIdx')
  params.projectionIdx = [1 3];
end
if ~isfield(params, 'r')
  params.r = 1;
end
if ~isfield(params, 'quick')
  params.quick = 0;
end
if ~isfield(params, 'cardinaThresh')
  params.cardinaThresh = [2000; 8; 30; 18; 60];
end
if ~isfield(params, 'compactThresh')
  params.compactThresh = [.95 1];
end
if ~isfield(params, 'regionType')
  params.regionType = {'lexicographic' 'convex hull'};
end
if ~isfield(params, 'filterByCardinality')
  params.filterByCardinality = [NaN params.cardinaThresh(3) - 1];
end
if ~isfield(params, 'filterByUniquePitchClasses')
  params.filterByUniquePitchClasses = struct;
  params.filterByUniquePitchClasses(2) = struct;
end
if ~isfield(params, 'filterByDuration')
  params.filterByDuration = [NaN NaN];
end
if ~isfield(params, 'rateOutput')
  ro = struct;
  ro.projIdx2 = 2;
  ro.tfPitch = 1;
  params.rateOutput = ro;
  clear rO
end
if ~isfield(params, 'filterOutOverlaps')
  foo = struct;
  foo.covPercent = .95;
  foo.intersectEdit = 1;
  foo.cardinaThresh = params.cardinaThresh(5);
  foo(2).covPercent = .95;
  foo(2).intersectEdit = 1;
  foo(2).cardinaThresh = params.cardinaThresh(3);
  params.filterOutOverlaps = foo;
  clear foo
end
if ~isfield(params, 'filterByEarlyPieceSectn')
  fbeps = struct;
  fbeps.occIn1stPiece = 1;
  fbeps.occIn1stSectn = 4;
  fbeps.topN = 5;
  fbeps.transOrOcc = 'translators';
  params.filterByEarlyPieceSectn = fbeps;
  clear fbeps
end
if ~isfield(params, 'categorisation')
  catgn = struct;
  catgn.similarThresh = .85;
  catgn.similarFunc = 'normalised matching score';
  catgn.similarParam = 'pitchindependent';
  catgn.ratingField = 'rating';
  catgn(2).similarThresh = 1/3;
  catgn(2).similarFunc = 'normalised matching score';
  catgn(2).similarParam = 'pitchindependent';
  catgn(2).ratingField = 'rating';
  params.categorisation = catgn;
  clear catgn
end
if ~isfield(params, 'inexactThresh')
  params.inexactThresh = [.9 .8];
end
if ~isfield(params, 'savePastRating')
  params.savePastRating = 1;
end
if ~isfield(params, 'savePattAllOccAll')
  params.savePattAllOccAll = 1;
end
if ~isfield(params, 'convertMPNpatt2MNN')
  params.convertMPNpatt2MNN = 0;
end
% Store the params variable as defs, to avoid it being overwritten when
% loading saved data.
defs = params;
clear params

n = size(D, 1);
Dp = unique(D(:, defs.projectionIdx), 'rows');
k = size(defs.projectionIdx, 2);

fprintf('Running discovery algorithm on %s.\n', defs.pieceName)

for irun = 1:2
  if irun == 1
    fprintf('\tRun with parameters aimed towards section discovery.\n')
  elseif irun == 2
    fprintf('\tRun with parameters aimed towards motif discovery.\n')
  end

  % 1. SIAR.
  fprintf('\t1. SIAR, r=%d.\n', defs.r)
  % Turn projection index into a string.
  projStr = '[';
  for ki = 1:k - 1
    projStr = [projStr num2str(defs.projectionIdx(ki)) ','];
  end
  projStr = [projStr num2str(defs.projectionIdx(k)) ']'];
  % Define file names.
  fileName = [defs.pieceName '_p=' projStr ',r=' num2str(defs.r)...
    ',q=' num2str(defs.quick)];
  SIAR_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
  if exist(SIAR_fName, 'file')
    load(SIAR_fName, 'S', 'runtime', 'FRT', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
  else
    [S, runtime, FRT] = SIAR(Dp, defs.r, defs.quick);
    params = defs;
    save(SIAR_fName, 'S', 'runtime', 'FRT', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    clear S runtime FRT params
  end
  
  % 2. Run CT (compactness trawler).
  if n <= defs.cardinaThresh(1)
    if irun == 1
      cT = defs.cardinaThresh(3);
    else
      cT = defs.cardinaThresh(2);
    end
  else
    if irun == 1
      cT = defs.cardinaThresh(5);
    else
      cT = defs.cardinaThresh(4);
    end
  end
  fprintf('\t2. Compactness trawler, a=%1.3f, b=%d, reg=%s.\n',...
    defs.compactThresh(irun), cT, defs.regionType{irun})
  if strcmp(defs.regionType{irun}, 'lexicographic')
    regStr = 'LX';
  else
    regStr = 'CH';
  end
  fileName = [fileName...
    ',a=' regexprep(sprintf('%1.3f', defs.compactThresh(irun)), '\.', 'p')...
    ',b=' num2str(cT)...
    ',reg=' regStr];
  SIARCT_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
  if exist(SIARCT_fName, 'file')
    load(SIARCT_fName, 'S', 'runtime', 'FRT', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
  else
    [S, runtime, FRT] = SIARCT(Dp, defs.r, defs.compactThresh(irun),...
      cT, defs.regionType{irun}, S_curr, runtime_curr, FRT_curr);
    params = defs;
    save(SIARCT_fName, 'S', 'runtime', 'FRT', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    clear S runtime FRT params
  end
  
  % 3. Filter by cardinality.
  if ~isnan(defs.filterByCardinality(irun))
    upperL = defs.filterByCardinality(irun);
    fprintf('\t3. Filter by cardinality, L=%d.\n', upperL)
    fileName = [fileName ',L=' num2str(upperL)];
    cardinaFilt_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(cardinaFilt_fName, 'file')
      load(cardinaFilt_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      S = filterByCardinality(S_curr, cT, upperL);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      save(cardinaFilt_fName, 'S', 'runtime', 'FRT', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % 4. Filter by unique pitch classes.
  if ~isempty(fieldnames(defs.filterByUniquePitchClasses(irun)))
    pitchDim = defs.filterByUniquePitchClasses(irun).pitchDim;
    mUnqPitch = defs.filterByUniquePitchClasses(irun).mUnqPitch;
    nPitchInOct = defs.filterByUniquePitchClasses(irun).nPitchInOct;
    fprintf('\t4. Filter by unique pitch classes, u=%d.\n', mUnqPitch)
    fileName = [fileName ',d=' num2str(pitchDim)...
      ',u=' num2str(mUnqPitch) ',o=' num2str(nPitchInOct)];
    UPCfilt_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(UPCfilt_fName, 'file')
      load(UPCfilt_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      S = filterByUniquePitchClasses(S_curr, pitchDim, mUnqPitch,...
        nPitchInOct);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      save(UPCfilt_fName, 'S', 'runtime', 'FRT', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % 5. Filter by duration.
  if ~isnan(defs.filterByDuration(irun))
    minDur = defs.filterByDuration(irun);
    fprintf('\t5. Filter by duration, z=%d.\n', minDur)
    fileName = [fileName ',z=' num2str(minDur)];
    durFilt_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(durFilt_fName, 'file')
      load(durFilt_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      S = filterByDuration(S_curr, minDur);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      save(durFilt_fName, 'S', 'runtime', 'FRT', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % 6. Rate discovered sets.
  if ~isempty(fieldnames(defs.rateOutput))
    fprintf('\t6. Rate discovered sets.\n')
    projIdx2 = defs.rateOutput.projIdx2;
    tfPitch = defs.rateOutput.tfPitch;
    fileName = [fileName ',s=' num2str(projIdx2) ',t=' num2str(tfPitch)];
    rated_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(rated_fName, 'file')
      load(rated_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      warning off
      S = rateOutput(S_curr, Dp, tfPitch, projIdx2);
      warning on
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      save(rated_fName, 'S', 'runtime', 'FRT', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % 7. Filter out pattern if own occurrences overlap too much.
  if ~isempty(fieldnames(defs.filterOutOverlaps(irun)))
    fprintf('\t7. Filter out patterns if own occurrences overlap too much.\n')
    covPercent = defs.filterOutOverlaps(irun).covPercent;
    intersectEdit = defs.filterOutOverlaps(irun).intersectEdit;
    fooCT = defs.filterOutOverlaps(irun).cardinaThresh;
    fileName = [fileName...
      ',v=' regexprep(sprintf('%1.3f', covPercent), '\.', 'p')...
      ',i=' num2str(intersectEdit)];
    covFilt_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(covFilt_fName, 'file')
      load(covFilt_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      S = filterOutOverlaps(S_curr, covPercent, intersectEdit, fooCT);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      if defs.savePastRating
        save(covFilt_fName, 'S', 'runtime', 'FRT', 'params');
      end
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % (Filtering by early in piece or section can only be executed for
  % 'motif' point sets.)
  if irun == 2
    % 8. Filter by point sets being early in piece or section
    if ~isempty(fieldnames(defs.filterByEarlyPieceSectn))
      fprintf('\t8. Retain patterns occurring early in piece or sections.\n')
      occIn1stPiece = defs.filterByEarlyPieceSectn.occIn1stPiece;
      occIn1stSectn = defs.filterByEarlyPieceSectn.occIn1stSectn;
      topN = defs.filterByEarlyPieceSectn.topN;
      transOrOcc = defs.filterByEarlyPieceSectn.transOrOcc;
      if strcmp(transOrOcc, 'translators')
        tocStr = 'tr';
      else
        tocStr = 'oc';
      end
      fileName = [fileName...
        ',op=' num2str(occIn1stPiece)...
        ',os=' num2str(occIn1stSectn)...
        ',top=' num2str(topN)...
        ',toc=' num2str(tocStr)];
      earlyFilt_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
      if exist(earlyFilt_fName, 'file')
        load(earlyFilt_fName, 'S', 'runtime', 'params');
        S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      else
        % contents = dir(fullfile(resultsPathSect, [compID fStub...
        %   '_SIARCT-CFP*.mat']));
        if ~exist('S_sectn', 'var')
          load(sectnResultsMat, 'S');
          S_sectn = S; clear S
        end
        tStart = tic;
        S = filterByEarlyPieceSectn(S_curr, D(1, 1), occIn1stPiece,...
          occIn1stSectn, topN, S_sectn, transOrOcc);
        runtime = runtime_curr(end) + toc(tStart);
        FRT = FRT_curr + toc(tStart);
        params = defs;
        if defs.savePastRating
          save(earlyFilt_fName, 'S', 'runtime', 'FRT', 'params');
        end
        S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
        clear S runtime FRT params
      end
    end
  end

  % 9. Categorisation.
  if ~isempty(fieldnames(defs.categorisation(irun)))
    fprintf('\t9. Categorise patterns by similarity, output prototype only.\n')
    similarThresh = defs.categorisation(irun).similarThresh;
    if strcmp(defs.categorisation(irun).similarFunc,...
        'normalised matching score')
      similarFunc = 'NSM';
    else
      similarFunc = 'card';
    end
    if strcmp(defs.categorisation(irun).similarParam, 'normal')
      similarParam = 'nrm';
    elseif strcmp(defs.categorisation(irun).similarParam, 'pitchindependent')
      similarParam = 'pi';
    elseif strcmp(defs.categorisation(irun).similarParam,...
        'tempoandpitchindependent')
      similarParam = 'tpi';
    elseif strcmp(defs.categorisation(irun).similarParam, 'tempoindependent')
      similarParam = 'ti';
    elseif isnumeric(defs.categorisation(irun).similarParam)
      similarParam = num2str(defs.categorisation(irun).similarParam);
    end
    if strcmp(defs.categorisation(irun).ratingField, 'rating')
      ratingField = 'r';
    end
    fileName = [fileName...
      ',c=' regexprep(sprintf('%1.3f', similarThresh), '\.', 'p')...
      ',cf=' similarFunc...
      ',cp=' similarParam...
      ',cr=' ratingField];
    SIARCT_C_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(SIARCT_C_fName, 'file')
      load(SIARCT_C_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      % if n <= defs.cardinaThresh(1)
      %   cT = defs.cardinaThresh(3);
      % else
      %   cT = defs.cardinaThresh(5);
      % end
      S = SIARCT_C(Dp, defs.r, defs.compactThresh(irun), cT,...
        defs.regionType{irun}, similarThresh,...
        defs.categorisation(irun).similarFunc,...
        defs.categorisation(irun).similarParam,...
        defs.categorisation(irun).ratingField, S_curr, runtime_curr, FRT_curr);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      if defs.savePastRating
        save(SIARCT_C_fName, 'S', 'runtime', 'FRT', 'params');
      end
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  
  % 10. FP (Fingerprinting).
  if ~isnan(defs.inexactThresh(irun))
    inexactThresh = defs.inexactThresh(irun);
    fprintf('\t10. FP, inexactThresh=%1.3f, func=%s.\n', inexactThresh,...
      defs.categorisation(irun).similarFunc)
    fileName = [fileName...
      ',fpi=' regexprep(sprintf('%1.3f', inexactThresh), '\.', 'p')];
    SIARCT_CFP_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
    if exist(SIARCT_CFP_fName, 'file')
      load(SIARCT_CFP_fName, 'S', 'runtime', 'params');
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    else
      tStart = tic;
      S = SIARCT_CFP(Dp, defs.r, defs.compactThresh(irun), cT,...
        defs.regionType{irun}, similarThresh,...
        defs.categorisation(irun).similarFunc,...
        defs.categorisation(irun).similarParam,...
        defs.categorisation(irun).ratingField, inexactThresh, S_curr,...
        'SIARCT_C', runtime_curr, FRT_curr);
      runtime = runtime_curr(end) + toc(tStart);
      FRT = FRT_curr + toc(tStart);
      params = defs;
      if defs.savePastRating
        save(SIARCT_CFP_fName, 'S', 'runtime', 'FRT', 'params');
      end
      S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
      clear S runtime FRT params
    end
  end
  if irun == 1
    S_sectn = S_curr;
    runtime_sectn = runtime_curr;
    FRT_sectn = runtime_curr;
    sectnResultsFnm = fileName;
    sectnResultsMat = SIARCT_CFP_fName;
  else
    S_motif = S_curr;
    runtime_motif = runtime_curr;
    FRT_motif = runtime_curr;
    motifResultsFnm = fileName;
    motifResultsMat = SIARCT_CFP_fName;
  end

  % 11. Saving all pattern occurrences to a text file. Might have to convert
  % MPNs to MNNs for evaluation purposes.
  if defs.savePattAllOccAll
    fprintf('\t11. Saving all pattern occurrences to a text file.\n')
    if strcmp(projStr, '[1,3]')
      fileName = [fileName ',conv=[1,2]'];
      pattAllOccAll_fName = fullfile(defs.pattDiscOut, [fileName '.txt']);
      if ~exist(pattAllOccAll_fName, 'file')
        S_curr = convertMPNpatt2MNN2(S_curr, D);
        D1 = unique(D(:, [1 2]), 'rows');
        SoutStyleStruct2PattAllOccAll(S_curr, D1, pattAllOccAll_fName);
      end
    else
      pattAllOccAll_fName = fullfile(defs.pattDiscOut, [fileName '.txt']);
      if ~exist(pattAllOccAll_fName, 'file')
        SoutStyleStruct2PattAllOccAll(S_curr, Dp, pattAllOccAll_fName);
      end
    end
    % if irun == 1
    %   sectnResultsTxt = pattAllOccAll_fName;
    % else
    %   motifResultsTxt = pattAllOccAll_fName;
    % end
  end
end
  
%% Combine discovered sections and motifs in one file for evaluation.
if ~exist('S_sectn', 'var')
  load(sectnResultsMat)
  S_sectn = S; runtime_sectn = runtime; FRT_sectn = FRT;
  clear S runtime FRT params
end
if ~exist('S_motif', 'var')
  load(motifResultsMat)
  S_motif = S; runtime_motif = runtime; FRT_motif = FRT;
  clear S runtime FRT params
end

S = [S_sectn S_motif];
runtime = runtime_sectn + runtime_motif;
FRT = FRT_sectn + FRT_motif;
if defs.convertMPNpatt2MNN
  S = convertMPNpatt2MNN2(S, D);
  % Remove translator field to avoid erroneous logic in estPrecMat.
  S = rmfield(S, {'translators' 'occurrences'});
end

% 12. Saving combined section and motif results to .mat and .txt files.
% Might have to convert MPNs to MNNs for evaluation purposes.
if defs.savePastRating
  fileName = [sectnResultsFnm ',',...
    regexprep(motifResultsFnm, [defs.pieceName '_'], '')];
  SIARCT_CFP_fName = fullfile(defs.pattDiscOut, [fileName '.mat']);
  if exist(SIARCT_CFP_fName, 'file')
    load(SIARCT_CFP_fName, 'S', 'runtime', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
  else
    params = defs;
    save(SIARCT_CFP_fName, 'S', 'runtime', 'FRT', 'params');
    S_curr = S; runtime_curr = runtime; FRT_curr = FRT; params_curr = params;
    % clear S runtime FRT params
  end
  if defs.savePattAllOccAll
    fprintf('\t12. Saving combined section and motif results to text file.\n')
    if strcmp(projStr, '[1,3]')
      fileName = [fileName ',conv=[1,2]'];
      pattAllOccAll_fName = fullfile(defs.pattDiscOut, [fileName '.txt']);
      if ~exist(pattAllOccAll_fName, 'file') && ~defs.convertMPNpatt2MNN
        S_curr = convertMPNpatt2MNN2(S_curr, D);
        D1 = unique(D(:, [1 2]), 'rows');
        SoutStyleStruct2PattAllOccAll(S_curr, D1, pattAllOccAll_fName);
%       else
%         D1 = unique(D(:, [1 2]), 'rows');
%         SoutStyleStruct2PattAllOccAll(S, D1, pattAllOccAll_fName);
      end
    else
      pattAllOccAll_fName = fullfile(defs.pattDiscOut, [fileName '.txt']);
      if ~exist(pattAllOccAll_fName, 'file')
        SoutStyleStruct2PattAllOccAll(S_curr, Dp, pattAllOccAll_fName);
      end
    end
  end
end

end
