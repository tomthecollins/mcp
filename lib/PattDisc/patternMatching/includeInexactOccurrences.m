function S_out = includeInexactOccurrences(S_in, D, similarThresh,...
  similarFunc, similarParam)

% Copyright Tom Collins 22/6/2013

% This function runs a matching algorithm on each pattern in the vector of
% structs S, treating the first occurrence of the pattern as the query, and
% the point set D as the database. 

% INPUT
%  S is a vector of structs output by a pattern discovery algorithm. It is
%   assumed that they have a field for 'translators' and 'occurrences'.
%  D is a point set.
%  similarThresh is a number in (0, 1]. If a match on the query is
%   greater than or equal to the threshold, then this is included in the
%   newly created inexactOccurrences struct.
%  similarFunc is a string indicating which function should be used for
%   calculating the symbolic music similarity, either 'cardinality score'
%   or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent',
%   'tempoindependent', or 'tempoandpitchindependent', see fpgethistogram2
%   for details).

if nargin < 5
  if strcmp(similarFunc, 'cardinality score')
    similarParam = 0;
  else
    similarParam = 'normal';
  end
end

% If using normalised matching score, check which implementation has been
% defined.
if strcmp(similarFunc, 'normalised matching score')
  if exist('fpgethistogram') == 3
    h_nsm = @fpgethistogram;
  else
    h_nsm = @fpgethistogram2;
  end
end

S_out = S_in;
if ~isempty(fieldnames(S_out))
    S_out(1).inexactOccurrences = struct;
end
nS = size(S_out, 2);
parfor iS = 1:nS
  if isfield(S_out(iS), 'translators')
    T = S_out(iS).translators;
  else
    T = zeros(1, size(D, 2));
  end
  l = S_out(iS).cardinality;
  P = S_out(iS).pattern + repmat(T(1, :), l, 1);
  patt_dur = P(l, 1) - P(1, 1);
  if strcmp(similarFunc, 'normalised matching score')
    % If using normalised matching score, calculate maximum possible
    % score for fingerprinting, and weight the histogram by this.
    sim_hist = h_nsm(P, P, similarParam);
    maxscore = max(sim_hist(:, 2));
    sim_hist = h_nsm(D, P, similarParam);
    if isempty(maxscore)
      % Introduced because the passing of a pattern as query and db to
      % fpgethistogram did not seem stable for smaller (non-sectional)
      % patterns.
      maxscore = max(sim_hist(:, 2));
    end
    if ~isempty(sim_hist)
      sim_hist = [sim_hist(:, 1) sqrt(sim_hist(:, 2)/maxscore)];
    end
  elseif strcmp(similarFunc, 'cardinality score')
    sim_hist = P2gethistogram(P, D);
  end
  % Now determine entries in the histogram that are similar enough to
  % include. 
  
  % NEED TO TEST THIS PEAKFINDER MORE THOROUGHLY. Especially as it
  % introduces two extra parameters, sel and thresh, which here are set to
  % defaults.
  
  if ~isempty(sim_hist)
    sel = (max(sim_hist(:, 2)) - min(sim_hist(:, 2)))/4;
    % thresh = [];
    [sim_idx, peakMag] = peakfinder(sim_hist(:, 2), sel, similarThresh);
    % [sim_idx, peakMag] = peakfinder(sim_hist(:, 2));
    % [peakLoc, peakMag] = peakfinder(sim_hist(:, 2),...
    %   (max(sim_hist(:, 2)) - min(sim_hist(:, 2)))/8, similarThresh);
    % Old version.
    % sim_idx = find(sim_hist(:, 2) >= similarThresh);
  else
    sim_idx = [];
  end
  
  occn = size(sim_idx, 1);
  inexactOccurrences = struct;
  regions = cell(1, occn);
  similarityRatings = zeros(occn, 1);
  exact_ons = T(:, 1) + S_out(iS).pattern(1, 1);
  % Increment to populate regions and similarityRatings (do not wish to
  % include exact occurrences here):
  occj = 1;
  for occi = 1:occn
    % Get the ontime of the occurrence, and use the pattern duration to
    % define each region of the point set D that is similar to the pattern.
    occ_on = sim_hist(sim_idx(occi), 1);
    % If the region of the inexact occurrence has the same ontime as that
    % of an exact occurrence, do not include it in the output.
    %
    % One issue with using ~ismember(occ_on, exact_ons) is that if x is the
    % ontime of an exact occurrence and x + epsilon is the ontime of an
    % inexact occurrence, then the inexact occurrence is included, even
    % though it probably should not be.
    %
    % So instead, if the occ_on and closest exact_ons differ by more than
    % 15% of the pattern duration, include it. Otherwise exclude it.
    percent_diff = min(abs(occ_on - exact_ons))/patt_dur;
    if occ_on > 0 && percent_diff > .15
      occ_off = occ_on + patt_dur;
      rel_idx = sum([D(:, 1) >= occ_on D(:, 1) <= occ_off], 2) == 2;
      regions{occj} = D(rel_idx, :);
      similarityRatings(occj) = sim_hist(sim_idx(occi), 2);
      occj = occj + 1;
    end
  end
  regions = regions(1:occj - 1);
  similarityRatings = similarityRatings(1:occj - 1);
  inexactOccurrences.regions = regions;
  inexactOccurrences.similarityRatings = similarityRatings;
  S_out(iS).inexactOccurrences = inexactOccurrences;
  
end

end
