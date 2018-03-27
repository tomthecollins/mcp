MNN2pitch_simple = function(MNN){
  // Tom Collins 6/1/2016.
  // In
  // metadata Integer mandatory
  // Out String
  // This function converts a MIDI note number into a pitch class and octave.
  // It does so in a completely naive manner (no consideration of global or
  // local key), but this is handy for things like Tone.js playback, which tend
  // to prefer "C" to "B#", "C#" to "Db" (I think), and "G" to "F##".

  var lookup = [
    "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
  ];
  var octave = Math.floor(MNN/12 - 1);
  var MNN_mod_12 = MNN % 12;
  return lookup[MNN_mod_12] + octave.toString();
}