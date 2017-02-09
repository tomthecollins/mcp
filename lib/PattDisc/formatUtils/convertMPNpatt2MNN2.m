function S_out = convertMPNpatt2MNN2(S, origD, ontime_idx, MNN_idx,...
  MPN_idx, tole)

% Copyright 19/11/2013 Tom Collins

% This function converts a vector of structs consisting of ontime-MPN
% patterns into a vector of structs consisting of ontime-MNN patterns
% instead. Unlike the function convertMPNpatt2MNN, this function outputs
% inexact occurrences as well.

% INPUT
%  S is a vector of structs output by a pattern discovery algorithm,
%   consisting of ontime-MPN patterns.
%  origD is the original point set.
%  MNN_idx is a positive integer indicating the column in origD of the MIDI
%   note numbers.
%  MPN_idx is a positive integer indicating the column in origD of the
%   morphetic pitch numbers.

% EXAMPLE
% P = 

if nargin < 6
  tole = 5;
end
if nargin < 5
  MPN_idx = 3;
end
if nargin < 4
  MNN_idx = 2;
end
if nargin < 3
  ontime_idx = 1;
end

nS = size(S, 2);
if nS == 0
  S_out = S;
else
  for iS = 1:nS
    P = S(iS).pattern;
    card = size(P, 1);
    % Took this line out because it is prone to rounding errors.
    % [~, loc] = ismember(P, origD(:, [ontime_idx MPN_idx]), 'rows');
    [~, loc] = ismember(round(P*10^tole)/10^tole,...
        round(origD(:, [ontime_idx MPN_idx])*10^tole)/10^tole, 'rows');
    s = struct;
    s.pattern = origD(loc, [ontime_idx MNN_idx]);
    T = S(iS).translators;
    occn = size(T, 1);
    if isfield(S(iS), 'inexactOccurrences')
      regions = S(iS).inexactOccurrences.regions;
      occm = size(regions, 2);
    else
      regions = [];
      occm = 0;
    end
    % Create a variable to store the occurrences of the pattern.
    patt_all = cell(1, occn + occm);
    % Store the ontime of each occurrence, in case they need to be presented
    % in temporal order.
    patt_on = zeros(occn + occm, 1);
    % Store the similarity rating as well, in case the exactness needs to be
    % displayed when the pattern is shown.
    sim_rat = zeros(occn + occm, 1);
    occj = 1; % Increment to populate patt_all.
    for occi = 1:occn
      curr_patt = P + repmat(T(occi, :), card, 1);
      % Use rounding to number of decimal places specified by tole.
      [~, loc] = ismember(round(curr_patt*10^tole)/10^tole,...
        round(origD(:, [ontime_idx MPN_idx])*10^tole)/10^tole, 'rows');
      patt_all{occj} = origD(loc, [ontime_idx MNN_idx]);
      patt_on(occj) = curr_patt(1, 1); % Occurrence ontime.
      sim_rat(occj) = 1; % Exactness = 1 for exact translation.
      occj = occj + 1;
    end
    % Get the information for each inexact occurrence of the pattern. If
    % there are no inexact occurrences then this loop will not execute as
    % occm = 0.
    for occi = 1:occm
      curr_patt = regions{occi};
      [~, loc] = ismember(round(curr_patt*10^tole)/10^tole,...
        round(origD(:, [ontime_idx MPN_idx])*10^tole)/10^tole, 'rows');
      patt_all{occj} = origD(loc, [ontime_idx MNN_idx]);
      patt_on(occj) = curr_patt(1, 1); % Occurrence ontime.
      if isfield(S(iS).inexactOccurrences, 'similarityRatings')
        % Exactness.
        curr_sim = S(iS).inexactOccurrences.similarityRatings(occi);
        if curr_sim > 1 % Correct for matches that exceed 1 slightly.
          curr_sim = 1;
        end
        sim_rat(occj) = curr_sim;
      end
      occj = occj + 1;
    end
    
    % Populate S_out.
    s.cardinality = size(s.pattern, 1);
    s.inexactOccurrences.regions = patt_all;
    s.inexactOccurrences.similarityRatings = sim_rat;
    s.inexactOccurrences.pattOn = patt_on;
    s.translators = [];
    s.occurrences = 0;
    if iS == 1
      S_out = s;
    else
      S_out(iS) = s;
    end
  end
end

end
