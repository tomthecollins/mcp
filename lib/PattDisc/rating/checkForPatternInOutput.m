function idx = checkForPatternInOutput(S, P)

% 10/4/2012 Copyright Tom Collins

% Checks for the existence of a pattern P in the output of a pattern
% discovery algorithm S, up to translation, and returns the index of the
% first instance of P if it exists.

% INPUT
%  S contains patterns discovered in some point set.
%  P is a point set.

idx = [];
nS = size(S, 2);
iS = 1; % Increment over S.
while iS <= nS
  if translationp(S(iS).pattern, P)
    idx = iS;
    iS = nS;
  end
  if isfield(S, 'categoryMembers')
    % Some patterns have been placed in subcategories, so search the
    % subcategories as well.
    T = S(iS).categoryMembers;
    nT = size(T, 2);
    iT = 1;
    while iT <= nT
      if translationp(T(iT).pattern, P)
        idx = [iS iT];
        iT = nT;
        iS = nS;
      end
      iT = iT + 1;
    end
  end
  iS = iS + 1;
end

end
