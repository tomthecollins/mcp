function SoutStyleStruct2PattAllOccAll(S, D, fname, includeExtraInfo, tole)

% Copyright Tom Collins 28/4/2013

% This function converts a vector of structs consisting of the output of a
% pattern discovery algorithm into a csv file that can be read by another
% function for evaluating the algorithm output against a ground truth. This
% function also converts a human ground truth into a pattAllOccAll file
% format.

% INPUT
%  S is a vector of structs output by a pattern discovery algorithm. It is
%   assumed that they have a field for 'rating'.
%  D is a point set.
%  fname is a path and name to which the anchor csv file will be written.
%  includeExtraInfo is an optional logical argument, taking the value one
%   if name, rating, and exactness fields should be included with the
%   output, and zero otherwise.
%  tole is a tolerance in terms of 10e. That is, a value of 3 means that
%   membership is checked up to 3 decimal places.

if nargin < 5
  tole = 5;
end
if nargin < 4
  includeExtraInfo = 0;
end

k = size(D, 2);
nstruct = size(S, 2);
fid = fopen(fname, 'w');
for istruct = 1:nstruct
  % Information about current pattern.
  s = S(istruct);
  if isfield(s, 'cardinality')
    card = s.cardinality;
  end
  if isfield(s, 'translators')
    T = s.translators;
    occn = size(T, 1);
  elseif isfield(s, 'occurrences') % For human ground truths.
    occn = size(s.occurrences, 2);
  else
    occn = 0;
  end
  
  % Retain functionality for extension to inexact occurrences.
  if isfield(s, 'inexactOccurrences')
    regions = s.inexactOccurrences.regions;
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
    if isfield(s, 'translators')
      curr_patt = s.pattern + repmat(T(occi, :), card, 1);
    else % For human ground truths.
      curr_patt = s.occurrences{occi};
    end
    [~, loc] = ismember(round(curr_patt*10^tole)/10^tole,...
      round(D*10^tole)/10^tole, 'rows');
    patt_all{occj} = D(loc, :);
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
      round(D*10^tole)/10^tole, 'rows');
    patt_all{occj} = D(loc, :);
    if ~isempty(curr_patt)
      patt_on(occj) = curr_patt(1, 1); % Occurrence ontime.
      if isfield(s.inexactOccurrences, 'similarityRatings')
        curr_sim = s.inexactOccurrences.similarityRatings(occi); % Exactness.
        if curr_sim > 1 % Correct for matches that exceed 1 slightly.
          curr_sim = 1;
        end
        sim_rat(occj) = curr_sim;
      end
      occj = occj + 1;
    end
  end
  % Put the occurrences in temporal order. Actually don't. Display the
  % exact occurrences before the inexact occurrences.
  % [patt_on, occ_idx] = sort(patt_on, 'ascend');
  % patt_all = patt_all(occ_idx);
  % sim_rat = sim_rat(occ_idx);
  % Save them into the struct for writing to file in the final loop below.
  S(istruct).patt_on = patt_on;
  S(istruct).patt_all = patt_all;
  if isfield(s, 'inexactOccurrences') &&...
      isfield(s.inexactOccurrences, 'similarityRatings')
    S(istruct).sim_rat = sim_rat;
  end
end

% Now we know the ontime of the first occurrence of each pattern (inexact
% or otherwise), so sort patterns by these ontimes. Actually don't. Leave
% the order as provided (probably that those patterns rated as most
% important are displayed first).
% all_on = zeros(nstruct, 1);
% for istruct = 1:nstruct
%   all_on(istruct) = S(istruct).patt_on(1);
% end
% [~, all_idx] = sort(all_on, 'ascend');
% S = S(all_idx);

% Write the information to a csv file.
for istruct = 1:nstruct
  s = S(istruct);
  occm = size(s.patt_on, 1);
  fprintf(fid, '%s\n', ['pattern' num2str(istruct)]);
  if includeExtraInfo
    fprintf(fid, '%s\n', ['name = ' num2str(istruct)]);
    fprintf(fid, '%s\n', ['rating = ' num2str(s.rating)]);
  end
  for occj = 1:occm
    fprintf(fid, '%s\n', ['occurrence' num2str(occj)]);
    if includeExtraInfo
      fprintf(fid, '%s\n', ['exactness = ' num2str(s.sim_rat(occj))]);
    end
    ptn = size(s.patt_all{occj}, 1);
    for pti = 1:ptn
      for dim = 1:k - 1
        fprintf(fid, '%6.5f, ', s.patt_all{occj}(pti, dim));
      end
      fprintf(fid, '%6.5f\n', s.patt_all{occj}(pti, k));
    end
  end
end
fprintf(fid, '\n');
fclose(fid);

end
