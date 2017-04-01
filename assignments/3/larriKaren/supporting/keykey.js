// This code adapted from http://codepen.io/razh/pen/zGJGwb
'use strict';
var octave = 4;
var keys = [];
var prevKeyCode;
var noteOns = [];
var noteOffs = [];

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

var onKeyDown = function () {
  var listener = undefined;
  return function (synth) {
    document.removeEventListener('keydown', listener);
    listener = function listener(event) {
        var keyCode = event.keyCode;
        // console.log('keyCode:', keyCode);
        if (!keys[keyCode]) {
          keys[keyCode] = true;
          // console.log('keys:', keys);
          var playedPitch = keyCodeToPitchClass(keyCode);
          console.log('playedPitch:', playedPitch);
          if (playedPitch) {
           keyboard1.toggle( keyboard1.keys[keyCode2MNNSimple(keyCode) - 60], true );
           synth.triggerAttack(playedPitch);
            
           prevKeyCode = keyCode;
           // Push it to the noteOns.
           var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
           var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;
           // console.log(qos);
           // console.log('note:', playedPitch);
           noteOns.push([qos, playedPitch]);
          }
        }
    };
    document.addEventListener('keydown', listener);
  };
}();

var onKeyUp = function () {
  var listener = undefined;
  var prev = undefined;
  return function (synth) {
    if (prev) {
     console.log('We got past the if prev!');
      prev.triggerRelease();
    }
    document.removeEventListener('keyup', listener);
    prev = synth;
    listener = function listener(event) {
      var keyCode2 = event.keyCode;
      if (keys[keyCode2]) {
        keys[keyCode2] = false;
        // console.log('keys:', keys);
        var playedPitch2 = keyCodeToPitchClass(keyCode2);
        console.log('playedPitch2:', playedPitch2);
        
        synth.triggerRelease(playedPitch2);
        keyboard1.toggle( keyboard1.keys[keyCode2MNNSimple(keyCode2) - 60], false );
        
        //if (synth instanceof Tone.PolySynth) {
        //  synth.triggerRelease(playedPitch);
        //}
        //else if (playedPitch && keyCode === prevKeyCode) {
        //  synth.triggerRelease();
        //}
        // Push it to the noteOffs.
        var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
        var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;
        // console.log(qos);
        // console.log('note:', playedPitch);
        noteOffs.push([qos, playedPitch2]);
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
(function () {
 var synth = new Tone.PolySynth(6, Tone.MonoSynth);
 synth.toMaster();
 synth.volume.value = -20;
 onKeyDown(synth);
 onKeyUp(synth);
}());
