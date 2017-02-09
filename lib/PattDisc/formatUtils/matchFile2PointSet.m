function [D, anchors] = matchFile2PointSet(matchFile, tfPerf)

% Copyright Tom Collins 22/4/2014

% This code converts a match file (format designed by Gerhard Widmer,
% Sebastian Flossmann, etc.) into a dataset. The format looks like:
%
% snote(7-1,[E,n],4,1:2,1/8,1/32,1.5,1.625,[staff1])
% -note(6,[E,n],4,1822,1852,1852,47).
%
% In the snote, the first element is a label, the second element refers to
% pitch, the third to octave. The next element gives the bar:beat on which
% the score note begins. The next element is the offset from this bar-beat
% position measured as a note type (here eighth-note). The following
% element is the note duration, also measured as a note type (here thirty-
% second note). The last three elements are ontime, offtime, and staff
% attribute. In the note, the first element is a label, the second element
% refers to pitch, the third to octave. The fourth, fifth, and sixth
% elements refer to times (it seems in milliseconds). The first time is the
% onset, the second the offset, and the third is the adjusted (for pedal?)
% offset. The final element is the note velocity.

% INPUT
%  matchFile is the path and name of the match file to be imported.
%  tfPerf is a logical indicating whether the snote fields (score) or note
%   fields (performance of a score) should be parsed/prioritised.

% EXAMPLE INPUT
% ...

if nargin < 2
  tfPerf = 0;
end

X = readMatchFile(matchFile);

nstruct = size(X, 2);
if ~tfPerf
  % The snote fields (score) should be parsed.
  D = zeros(nstruct, 5);
  anchors = cell(1, nstruct);
  irow = 1; % Increment to populate D and anchors.
  for istruct = 1:nstruct
    if ~isempty(X(istruct).snote)
      snote = X(istruct).snote;
      
      % I thought it might be necessary to find the previous member of D that
      % ties to this note, and increase its duration, but the increased
      % duration is added already.
      % MNNProbe = pitchname2Midi(snote.pitchname);
      % MPNProbe = pitchname2morphetic(snote.pitchname);
      % durProbe = snote.beatend - snote.beatstart;
      % jrow = irow - 1; % Increment back over previous rows of D.
      % while jrow >= 1
      %   if D(jrow, 2) == MNNProbe && D(jrow, 3) == MPNProbe
      %     D(jrow, 4) = D(jrow, 4) + durProbe;
      %     jrow = 1;
      %   end
      %   jrow=jrow-1;
      % end
      
      % Determine whether the note is tied from a previous note. If so,
      % ignore it.
      if ~ismember('leftOutTied', snote.scoreattributes)
        % Anchor.
        anchors{irow} = snote.anchor;
        % Ontime.
        D(irow, 1) = snote.beatstart;
        % MIDI note number.
        D(irow, 2) = pitchname2Midi(snote.pitchname);
        % Morphetic pitch number.
        D(irow, 3) = pitchname2morphetic(snote.pitchname);
        % Duration.
        D(irow, 4) = snote.beatend - snote.beatstart;
        % Staff number. THIS NEEDS GENERALISING.
        if strcmp(snote.scoreattributes{1}, 'staff2')
          D(irow, 5) = 1;
        end
        irow = irow + 1;
      end
    end
  end
  anchors0 = anchors(1:irow - 1);
  D0 = D(1:irow - 1, :);
  
  % Return the unique datapoints in lexicographic order.
  [D, idx] = unique(D0, 'rows');
  anchors = anchors0(idx);
else
  % The note fields (performance) should be parsed.
  D = zeros(nstruct, 7);
  anchors = cell(1, nstruct);
  irow = 1; % Increment to populate D and anchors.
  for istruct = 1:nstruct
    if ~isempty(X(istruct).note) || ~isempty(X(istruct).trill)
      if ~isempty(X(istruct).note)
        note = X(istruct).note;
      else
        note = X(istruct).trill;
      end
      
      anchors{irow} = note.number;
      % Onset.
      D(irow, 1) = note.onset_midi;
      % MIDI note number.
      pitchname = {note.notename note.notemod note.octave};
      D(irow, 2) = pitchname2Midi(pitchname);
      % Morphetic pitch number.
      D(irow, 3) = pitchname2morphetic(pitchname);
      % Duration.
      D(irow, 4) = note.adjOffset_midi - note.onset_midi;
      % Velocity.
      D(irow, 5) = note.velocity;
      % Offset.
      D(irow, 6) = note.offset_midi;
      % Adjusted offset.
      D(irow, 7) = note.adjOffset_midi;
      irow = irow + 1;
      
    end
  end
  anchors0 = anchors(1:irow - 1);
  D0 = D(1:irow - 1, :);
  
  % Return the unique datapoints in lexicographic order.
  [D, idx] = unique(D0, 'rows');
  anchors = anchors0(idx);
end

end
