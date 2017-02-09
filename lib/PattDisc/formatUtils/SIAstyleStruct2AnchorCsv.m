function SIAstyleStruct2AnchorCsv(S, D, anchors, fname, tole)

% Copyright Tom Collins 28/4/2013

% This function converts a vector of structs consisting of the output of a
% pattern discovery algorithm into a csv file that can be read by
% jAnnotator, for improved browsing of discovered patterns.

% INPUT
%  S is a vector of structs output by a pattern discovery algorithm. It is
%   assumed that they have a field for 'rating'.
%  D is a point set.
%  anchors is a cell of strings in the format 'x-y', which are labels that
%   can be interpreted by jAnnotator for highlighting note collections.
%  fname is a path and name to which the anchor csv file will be written.
%  tole is a tolerance in terms of 10e. That is, a value of 3 means that
%   membership is checked up to 3 decimal places.

if nargin < 5
  tole = 5;
end

nstruct = size(S, 2);
fid = fopen(fname, 'w');
for istruct = 1:nstruct
  % Information about current pattern.
  s = S(istruct);
  card = s.cardinality;
  T = s.translators;
  occn = size(T, 1);
  % Get the anchors for each exact occurrence of the pattern.
  patt_anch = cell(occn, card);
  for occi = 1:occn
    curr_patt = s.pattern + repmat(T(occi, :), card, 1);
    [~, loc] = ismember(round(curr_patt*10^tole)/10^tole,...
      round(D*10^tole)/10^tole, 'rows');
    patt_anch(occi, :) = anchors(loc);
  end
  % Now write the information to a csv file.
  fprintf(fid, '%s\n', ['pattern ' num2str(istruct)]);
  fprintf(fid, '%s\n', ['rating = ' num2str(s.rating)]);
  for occi = 1:occn
    fprintf(fid, '%s\n', ['occurrence ' num2str(occi)]);
    fprintf(fid, '%s\n', 'exactness = 1');
    for pti = 1:card - 1
      fprintf(fid, '%s, ', patt_anch{occi, pti});
    end
    fprintf(fid, '%s\n', patt_anch{occi, card});
  end
  
  inexact = s.inexactOccurrences;
  occn = size(inexact.similarityRatings, 1);
  % Get the anchors for each exact occurrence of the pattern.
  patt_anch = cell(1, occn);
  for occi = 1:occn
    curr_patt = inexact.regions{occi};
    [~, loc] = ismember(round(curr_patt*10^tole)/10^tole,...
      round(D*10^tole)/10^tole, 'rows');
    patt_anch{occi} = anchors(loc);
  end
  % Now write the information to a csv file.
  for occi = 1:occn
    fprintf(fid, '%s\n', ['occurrence ' num2str(occi)]);
    fprintf(fid, '%s\n', ['exactness = '...
      num2str(inexact.similarityRatings(occi))]);
    ptn = size(patt_anch{occi}, 2);
    for pti = 1:ptn - 1
      fprintf(fid, '%s, ', patt_anch{occi}{pti});
    end
    fprintf(fid, '%s\n', patt_anch{occi}{ptn});
  end
end
fprintf(fid, '\n');
fclose(fid);

end
