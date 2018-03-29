// This code adapted from http://codepen.io/razh/pen/zGJGwb
'use strict';
var octave = 3;
var keys = [];
var prevKeyCode;
var noteOns = [];
var noteOffs = [];

function MNN2pitch_simple(MNN){
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

// var keyCodeToPitchClass = function (){
function keyCodeToPitchClass(keyCode){
  // console.log('keyCode:', keyCode);
  var notes = {
      65: 'Cl',
      87: 'C#l',
      83: 'Dl',
      69: 'D#l',
      68: 'El',
      70: 'Fl',
      84: 'F#l',
      71: 'Gl',
      89: 'G#l',
      72: 'Al',
      85: 'A#l',
      74: 'Bl',
      75: 'Cu',
      79: 'C#u',
      76: 'Du',
      80: 'D#u',
      59: 'Eu',
      186: 'Eu',
      222: 'Fu',
      221: 'F#u',
      220: 'Gu'
  };
  // var noteToFrequency = Tone.prototype.noteToFrequency;
  var note = notes[keyCode];
  // console.log('note:', note);
  if (!note) {
    return;
  }
  //var freq = ;
  // console.log(note.replace('l', octave).replace('u', octave + 1));
  return note.replace('l', octave).replace('u', octave + 1);
  // return noteToFrequency(note.replace('l', octave).replace('u', octave + 1));
};

function keyCode2MNNSimple(keyCode){
 var pitches = {
  65: 60,
  87: 61,
  83: 62,
  69: 63,
  68: 64,
  70: 65,
  84: 66,
  71: 67,
  89: 68,
  72: 69,
  85: 70,
  74: 71,
  75: 72,
  79: 73,
  76: 74,
  80: 75,
  59: 76,
  186: 77,
  222: 78,
  221: 79,
  220: 80
 };
 return pitches[keyCode];
}

var onKeyDown = function (){
  var listener = undefined;
  return function (piano){
    document.removeEventListener('keydown', listener);
    listener = function listener(event){
      var keyCode = event.keyCode;
      // console.log('keyCode:', keyCode);
      if (!keys[keyCode]) {
        keys[keyCode] = true;
        // console.log('keys:', keys);
        var playedPitch = keyCodeToPitchClass(keyCode);
        // console.log('playedPitch:', playedPitch);
        if (playedPitch) {
          keyboard1.toggle( keyboard1.keys[keyCode2MNNSimple(keyCode) - 60], true );
          piano.triggerAttack(playedPitch);
           
          prevKeyCode = keyCode;
          // Push it to the noteOns.
          var qos = Tone.Transport.seconds;
          // var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
          // var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;
          console.log('noteOn:', [qos, playedPitch]);
          noteOns.push([qos, playedPitch]);
        }
      }
    };
    document.addEventListener('keydown', listener);
  };
}();

var onKeyUp = function(){
  var listener = undefined;
  var prev = undefined;
  return function(piano){
    if (prev){
      console.log('We got past the if prev!');
      prev.triggerRelease();
    }
    document.removeEventListener('keyup', listener);
    prev = piano;
    listener = function listener(event) {
      var keyCode2 = event.keyCode;
      if (keys[keyCode2]) {
        keys[keyCode2] = false;
        // console.log('keys:', keys);
        var playedPitch2 = keyCodeToPitchClass(keyCode2);
        // console.log('playedPitch2:', playedPitch2);
        
        piano.triggerRelease(playedPitch2);
        keyboard1.toggle( keyboard1.keys[keyCode2MNNSimple(keyCode2) - 60], false );
        
        // Push it to the noteOffs.
        var qos = Tone.Transport.seconds;
        // var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
        // var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;
        noteOffs.push([qos, playedPitch2]);
        console.log('noteOff:', [qos, playedPitch2]);
      }
    };
    document.addEventListener('keyup', listener);
  };
}();

//document.addEventListener('keydown', function (event) {
//  if (event.keyCode === 90) {
//      octave = Math.max(octave - 1, 0);
//  }
//  if (event.keyCode === 88) {
//      octave = Math.min(octave + 1, 9);
//  }
//});

var piano = new Tone.Sampler({
  "C3": "./acoustic_grand_piano/048_keyboard_acoustic_000-100.wav",
  "E3": "./acoustic_grand_piano/052_keyboard_acoustic_000-100.wav",
  "G#3": "./acoustic_grand_piano/056_keyboard_acoustic_000-100.wav",
  "C4": "./acoustic_grand_piano/060_keyboard_acoustic_000-100.wav",
  "E4": "./acoustic_grand_piano/064_keyboard_acoustic_000-100.wav",
  "G#4": "./acoustic_grand_piano/068_keyboard_acoustic_000-100.wav",
  "C5": "./acoustic_grand_piano/072_keyboard_acoustic_000-100.wav",
  "E5": "./acoustic_grand_piano/076_keyboard_acoustic_000-100.wav",
  "G#5": "./acoustic_grand_piano/080_keyboard_acoustic_000-100.wav",
  "C6": "./acoustic_grand_piano/084_keyboard_acoustic_000-100.wav"
}, function(){
  console.log('Piano samples loaded!');
});
piano.toMaster();
piano.volume.value = -20;

(function () {
  //var piano = new Tone.PolySynth(12, Tone.MonoSynth);
  onKeyDown(piano);
  onKeyUp(piano);
}());
