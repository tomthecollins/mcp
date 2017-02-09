function S_out = filterByInterOpus(S_in, onOffs, useInexactOcc)

% Copyright Tom Collins 27/7/2014

% This function takes a struct S_in as input, a variable A containing point
% set representations of multiple pieces, and a variable onOffs giving the
% minimal ontimes and maximal offtimes of each piece in the multiple point
% set. If a pattern in S_in belongs to more than one of the pieces in A,
% then it is included in S_out.

% INPUT
%  S_in is a vector of structs, assumed to contain fields for pattern and
%   translators.
%  A is an n x k matrix representing a k-dimensional set of n points. These
%   points are supposed to be from more than one movement of music.
%  onOffs is a m x 2 matrix, where m is the number of movements in A, and
%   two columns contain the minimal ontimes and maximal offtimes of each
%   movement.
%  useInexOcc is an optional logical argument, taking the value 1 if
%   inexact occurrences of patterns are to be counted (as well as exact
%   occurrences of patterns) when determining whether a pattern occurs
%   across multiple pieces.

if nargin < 3
  useInexactOcc = 0;
end

nS = size(S_in, 2);
jS = 1; % Increment over additions to S_out.
for iS = 1:nS
  include = 0;
  T = S_in(iS).translators;
  nExOcc = size(T, 1);
  if useInexactOcc
    nInexOcc = size(S_in(iS).inexactOccurrences.regions, 2);
  else
    nInexOcc = 0;
  end
  nOcc = nExOcc + nInexOcc;
  % Variable to record to which piece each pattern occurrence belongs.
  occursIn = zeros(nOcc, 1);
  jOcc = 1; % Increment over additions to occursIn.
  for iOcc = 1:nExOcc
    on1 = S_in(iS).pattern(1, 1) + T(iOcc, 1);
    occursIn(jOcc) = find(...
      sum([on1 >= onOffs(:, 1) on1 < onOffs(:, 2)], 2) == 2);
    jOcc=jOcc+1;
  end
  for iOcc = 1:nInexOcc
    on1 = S_in(iS).inexactOccurrences.regions{iOcc}(1, 1);
    occursIn(jOcc) = find(...
      sum([on1 >= onOffs(:, 1) on1 < onOffs(:, 2)], 2) == 2);
    jOcc=jOcc+1;
  end
  % See if there is more than one unique index in occursIn, which would
  % imply that there are pattern occurrences in more than one piece.
  if size(unique(occursIn), 1) > 1
    include = 1;
  end
  if include
    if jS == 1
      S_out = S_in(iS);
      S_out.occursIn = occursIn;
    else
      S_temp = S_in(iS);
      S_temp.occursIn = occursIn;
      S_out(jS) = S_temp;
    end
    jS=jS+1;
  end
end

% If jS was never incremented, then there were no inter-opus patterns, so
% assign S_out as empty.
if jS == 1
  S_out = struct([]);
end
  
end
