function [T, C] = symbolicFingerprint(D, ID, n1, n2, d, trans_invar,...
  use_ratio, removeDuplicates)

% Copyright Tom Collins 9/1/2013

% Given a two-dimensional dataset D consisting of ontimes (first column)
% and MIDI note numbers (second column, or some other numeric
% representation of pitch), this function returns symbolic fingerprints as
% described by Arzt, Bock, and Widmer (2012). For transposition-variant
% fingerprints, the format is

% [m_1 : m_2 : m_3 : r] : ID : t : d_{1,2}

% for locally constrained combinations (controlled by n1, n2, and d) of
% successive MIDI notes, where m1, m2, and m3 are MIDI note numbers,
% d_{i,j} is the difference between the onsets of MIDI notes i and j, r is
% the fraction d_{2,3}/d_{1,2}, and t is a time stamp. For the
% transposition-invariant version, replace [m_1 : m_2 : m_3] by

% [m2 - m1 : o2 - o1].

% The tokens [m_1 : m_2 : m_3 : r] are stored in T and the rest of the
% fingerprints in a cell array C.

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  ID is a string to identify the piece or movement.
%  n1 is a positive integer for the number of points to look ahead of
%   point 1.
%  n2 is a positive integer for the number of points to look ahead of
%   point 2.
%  d is a time in seconds, specifying that points must be at least d
%   seconds in the future.
%  trans_invar is a logical, indicating whether transposition-invariant
%   hash keys should be created or not.
%  use_ratio is a logical, indicating whether the ratio of the difference
%   between (notes 2 and 3) and (notes 1 and 2) should be used, or just the
%   difference between notes 1 and 2.
%  removeDuplicates is a logical, indicating whether the function unique
%   should be applied to the fingerprints (only comes in to play when
%   trans_invar = 1).

% EXAMPLE INPUT
% D = [-1 81; -3/4 76; -1/2 85; -1/4 81; 0 88; 1 57; 1 61; 1 64; 2 73;
%   9/4 69; 5/2 76; 11/4 73; 3 81; 4 45; 4 49; 4 52];
% ID = 'beethovenOp2No2Mvt3'; 
% % Default parameter values:
% n1 = 5;
% n2 = 5;
% d = .05;
% trans_invar = 0;

if use_ratio
  % Tempo-independent version. Must use triples of MIDI notes and calculate
  % r = d_{2,3}/d_{1,2}.
  n = size(D, 1);
  N = n*n1*n2;
  T = zeros(N, 4);
  C = cell(N, 3);
  ti = 1; % Increment to create T.
  for i = 1:n % Iterate over D.
    i1 = 1; % Iterate over the pair.
    j1 = 0; % Increment to use if next MIDI event is too close in ontime.
    while i + i1 + j1 <= n && i1 <= n1
      if D(i + i1 + j1, 1) - D(i, 1) < d
        j1 = j1 + 1;
      else
        i2 = 1; % Iterate over the triple.
        j2 = 0; % Increment to use if next MIDI event too close in ontime.
        while i + i1 + j1 + i2 + j2 <= n && i2 <= n2
          if D(i + i1 + j1 + i2 + j2, 1) - D(i + i1 + j1, 1) < d
            j2 = j2 + 1;
          else
            idx = [i i + i1 + j1 i + i1 + j1 + i2 + j2]; % Rel. indices.
            % Create the tokens. First work out the ratio r.
            r = (D(idx(3), 1) - D(idx(2), 1))/...
              (D(idx(2), 1) - D(idx(1), 1));
            % Manipulate the MIDI note numbers.
            if trans_invar
              T(ti, 1) = D(idx(2), 2) - D(idx(1), 2);
              T(ti, 2) = D(idx(3), 2) - D(idx(2), 2);
              T(ti, 3) = r;
            else
              T(ti, 1:3) = D(idx, 2)';
              T(ti, 4) = r;
            end
            % Populate the cell array.
            C{ti, 1} = ID;
            C{ti, 2} = D(idx(1), 1);
            C{ti, 3} = D(idx(2), 1) - D(idx(1), 1);
            ti = ti + 1;
            i2 = i2 + 1;
          end
        end
        i1 = i1 + 1;
      end
    end
  end
  T = T(1:ti - 1, :);
  C = C(1:ti - 1, :);
  % If the transposition-invariant version is being used, there will be a
  % column of zeros that needs removing.
  if trans_invar
    T = T(:, 1:3);
    % Remove duplicate fingerprints if desired.
    if removeDuplicates
      [fpUnq, idx] = unique([T [C{:, 2}]'], 'rows');
      T = fpUnq(:, 1:3);
      C = C(idx, :);
    end
  end
  
else
  % Tempo-dependent version. Must use pairs of MIDI notes and calculate
  % d = o2 - o1.
  n = size(D, 1);
  N = n*n1;
  T = zeros(N, 3);
  C = cell(N, 2);
  ti = 1; % Increment to create T.
  for i = 1:n % Iterate over D.
    i1 = 1; % Iterate over the pair.
    j1 = 0; % Increment to use if next MIDI event is too close in ontime.
    while i + i1 + j1 <= n && i1 <= n1
      if D(i + i1 + j1, 1) - D(i, 1) < d
        j1 = j1 + 1;
      else
        idx = [i i + i1 + j1]; % Rel. indices.
        % Create the tokens. First work out the ratio r.
        diffn = D(idx(2), 1) - D(idx(1), 1);
        % Manipulate the MIDI note numbers.
        if trans_invar
          T(ti, 1) = D(idx(2), 2) - D(idx(1), 2);
          T(ti, 2) = diffn;
        else
          T(ti, 1:2) = D(idx, 2)';
          T(ti, 3) = diffn;
        end
        % Populate the cell array.
        C{ti, 1} = ID;
        C{ti, 2} = D(idx(1), 1);
        C{ti, 3} = D(idx(2), 1) - D(idx(1), 1);
        ti = ti + 1;
        i1 = i1 + 1;
      end
    end
  end
  T = T(1:ti - 1, :);
  C = C(1:ti - 1, :);
  % If the transposition-invariant version is being used, there will be a
  % column of zeros that needs removing.
  if trans_invar
    T = T(:, 1:2);
    % removeDuplicate fingerprints if desired.
    if removeDuplicates
      [fpUnq, idx] = unique([T [C{:, 2}]'], 'rows');
      T = fpUnq(:, 1:2);
      C = C(idx, :);
    end
  end
  
end

end
