function idx = checkForVectorInOutput(S, v)

% 19/7/2013 Copyright Tom Collins and Ali Nikrang

% Checks for the existence of a vector v in the output of a pattern
% discovery algorithm S, and returns the index of the first instance of v
% if it exists.

% INPUT
%  S contains patterns discovered in some point set.
%  P is a point set.

idx = [];
k = size(v, 2);
n = size(S, 2);
iS = 1;
while iS <= n
  if sum(S(iS).vector == v) == k;
    idx = iS;
    iS = n;
  end
  if isfield(S, 'categoryMembers')
    % Some patterns have been placed in subcategories, so search the
    % subcategories as well.
    T = S(iS).categoryMembers;
    m = size(T, 2);
    iT = 1;
    while iT <= m
      if sum(T(iT).vector == v) == k;
        idx = [iS iT];
        iT = m;
        iS = n;
      end
      iT = iT + 1;
    end
  end
  iS = iS + 1;
end

end
