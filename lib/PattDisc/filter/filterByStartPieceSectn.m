function S_out = filterByStartPieceSectn(S_in, d1, topN, S_sectn,...
  transOrOcc)

% 7/2/2014 Copyright Tom Collins

% This function takes a pattern struct S_in as input, the first ontime in
% the piece in which members of S_in occur, and the largest topN sections
% within that piece (contained in S_sectn). If the first occurrence of a
% pattern in S_in begins has the same first ontime as the first ontime of
% the piece, or the same first ontime as at least one of the five largest
% sectional repetitions, then it is included in the output.

% INPUT
%  S_in is a vector of structs, assumed to contain fields for pattern and
%   cardinality, and one of translators or occurrences.
%  d1 is a real number indicating the first ontime in the piece.
%  topN is a positive integer, determining how many repeated sections are
%   considered.
%  S_sectn is an optional argument. It is a vector of structs, assumed to
%   contain fields for pattern and cardinality, and one of translators or
%   occurrences. It is the struct that contains repeated sections. If not
%   provided, S_sectn is set to S_in.
%  transOrOcc is an optional string argument, taking the value
%   'translators' or 'occurrences'. It specifies which field is used to
%   determine the earliest occurrence of a pattern. If 'translators' is
%   used (default), then the earliest occurrence will be the earliest
%   translationally exact occurrence. If 'occurrences' is used, then the
%   earliest occurrence may be an inexact occurrence.

if nargin < 6
  transOrOcc = 'translators';
end
if nargin < 5
  S_sectn = S_in;
end

% Reorder S_sectn by the cardinality of each pattern.
a = [S_sectn.cardinality];
[~, m] = sort(a, 'descend');
nSec = size(S_sectn, 2);
S_sectn = S_sectn(m(1:min(nSec, topN)));
nSec = size(S_sectn, 2);

% Get the first ontime for each of the first five sectional repetitions,
% and create an nSec x 2 matrix of permissible time intervals.
f_on = zeros(nSec, 1);
for kS = 1:nSec
  % Get the first ontime of the earliest occurrence of the pattern.
  if strcmp(transOrOcc, 'translators')
    T = unique(S_sectn(kS).translators, 'rows');
    P_on = S_sectn(kS).pattern(1, 1) + T(1, 1);
  else
    occs = S_sectn(kS).occurrences;
    occn = size(occs, 2);
    P_ons = zeros(occn, 1);
    for occi = 1:occn
      P_ons(occi) = occs{occi}(1, 1);
    end
    P_on = min(P_ons);
  end
  f_on(kS) = P_on;
end

% Iterate over S_in and keep the patterns that fit the onset criteria.
nS = size(S_in, 2);
jS = 1; % Increment to populate S_out.
for iS = 1:nS
  % Get the first ontime of the earliest occurrence of the pattern.
  if strcmp(transOrOcc, 'translators')
    T = unique(S_in(iS).translators, 'rows');
    P_on = S_in(iS).pattern(1, 1) + T(1, 1);
  else
    occs = S_in(iS).occurrences;
    occn = size(occs, 2);
    P_ons = zeros(occn, 1);
    for occi = 1:occn
      P_ons(occi) = occs{occi}(1, 1);
    end
    P_on = min(P_ons);
  end
  include = 0;
  % Is it near enough to the beginning of the piece?
  if P_on == d1
    include = 1;
  end
  % Is it near enough to the beginning of one of the repeated sections?
  kS = 1; % Increment over the repeated sections.
  while kS <= nSec
    if P_on == f_on(kS)
      include = 1;
      kS = nSec;
    end
    kS=kS+1;
  end
  % If so, include it in the output struct.
  if include
    if jS == 1
      S_out = S_in(iS);
    else
      S_out(jS) = S_in(iS);
    end
    jS=jS+1;
  end
end
  
end
