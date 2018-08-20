// This code adapted from http://codepen.io/razh/pen/zGJGwb
'use strict';
var octave = 4;
var keys = [];
var prevKeyCode;
var noteOns = [];
var noteOffs = [];

var ivNoteOns = [];
var ivNoteOffs = [];

// var keyCodeToPitchClass = function (){
function keyCodeToPitchClass(keyCode) {
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

function keyCode2MNNSimple(keyCode) {
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

// Regular Notes: C4, C#4, D4 , D#4, E4 , F4 , F#4, G4 , G#4, A4 , A#4, B4 , C5
// Inverse Notes: C5, B4 , A#4, A4 , G#4, G4 , F#4, F4 , E4 , D#4, D4 , C#4, C4
//This function changes the supplied note to its inverse
function mapToInverse(note) {
  var notes = ['C4', 'C#4', 'D4', 'D#4', 'E4', 'F4', 'F#4', 'G4', 'G#4', 'A4', 'A#4', 'B4', 'C5'];
  var ivNotes = ['C5', 'B4', 'A#4', 'A4', 'G#4', 'G4', 'F#4', 'F4', 'E4', 'D#4', 'D4', 'C#4', 'C4'];
  var newNote;

  for (i = 0; i < notes.length; i++) {
    if (note == notes[i]) {
      newNote = ivNotes[i];
      return newNote;
    }
  }
}

// Regular Notes: C4, C#4, D4 , D#4, E4 , F4 , F#4, G4 , G#4, A4 , A#4, B4 , C5
// Inverse Notes: C5, B4 , A#4, A4 , G#4, G4 , F#4, F4 , E4 , D#4, D4 , C#4, C4
//This function converts the note played to the corresponding row of the visualization  
var matrixRowNote;
function NoteToRow(playedPitch) {
  var regNotes = ['C4', 'C#4', 'D4', 'D#4', 'E4', 'F4', 'F#4', 'G4', 'G#4', 'A4', 'A#4', 'B4', 'C5'];
  for (i = 0; i < regNotes.length; i++) {
    if (playedPitch == regNotes[i]) {
      return 12 - i;
    }
  }
}

//Initialize some parameters for onKeyDown
var matrixCurrCol = 0;
var mat2Info = [];

//Deals with key down event
var onKeyDown = function () {
  var listener = undefined;
  return function (synth) {
    document.removeEventListener('keydown', listener);
    listener = function listener(event) {
      var keyCode = event.keyCode;
      if (!keys[keyCode]) {
        keys[keyCode] = true;
        var playedPitch = keyCodeToPitchClass(keyCode);
        var inversePitch = mapToInverse(playedPitch);

        if (playedPitch) {
          keyboard1.toggle(keyboard1.keys[keyCode2MNNSimple(keyCode) - 60], true);
          synth.triggerAttack(playedPitch);

          //Find the row of the matrix the note will be inserted into
          matrixRowNote = NoteToRow(playedPitch);
          matrix1.matrix[matrixCurrCol][matrixRowNote] = 1;

          matrix2.matrix[matrixCurrCol][Math.abs(matrixRowNote - 12)] = null;

          //Check that the number of notes does not exceed the width of the matrix
          if (matrixCurrCol < 35) {
            matrixCurrCol += 1;
          }
          //Draw the Notes to the boxes of each matrix
          matrix1.draw();

          prevKeyCode = keyCode;
          var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
          var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;
          noteOns.push([qos, playedPitch]);
          ivNoteOns.push([qos, inversePitch]);
        }
      }
    };
    document.addEventListener('keydown', listener);
  };
}();


//Function rounds a number to two decimal places
function roundTo(n, digits) {
  /*
  Taken from Stack Overflow
  Rick Calder, Apr 2 2013
  */
  var negative = false;
  if (digits === undefined) {
    digits = 0;
  }
  if (n < 0) {
    negative = true;
    n = n * -1;
  }
  var multiplicator = Math.pow(10, digits);
  n = parseFloat((n * multiplicator).toFixed(11));
  n = (Math.round(n) / multiplicator).toFixed(2);
  if (negative) {
    n = (n * -1).toFixed(2);
  }
  return n;
}

//Formats original and inverse sample data
function makeNotation() {
  var alertString = "Time (sec)" + "\t" + "Pitch (Original)" + "\t" + "Pitch (Inverse)" + "\n";
  alertString += "______________________________________________________________" + "\n"
  for (var i = 0; i < ivNoteOns.length; i++) {
    alertString += + "\t\t" + roundTo(ivNoteOns[i][0], 2) + "\t\t";
    alertString += mapToInverse(ivNoteOns[i][1]) + "\t\t\t\t" + ivNoteOns[i][1] + "\n";
  }
  alert(alertString);
}



//Deals with key up event
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

        var playedPitch2 = keyCodeToPitchClass(keyCode2);

        synth.triggerRelease(playedPitch2);
        keyboard1.toggle(keyboard1.keys[keyCode2MNNSimple(keyCode2) - 60], false);

        var pos = Tone.Transport.toSeconds(Tone.Transport.Position);
        var qos = Tone.Transport.toSeconds(pos + "@ 16n") - startQos;

        noteOffs.push([qos, playedPitch2]);
      }
    };
    document.addEventListener('keyup', listener);
  };
}();



// Defines sythn for the sound to be played
(function () {
  var synth = new Tone.PolySynth(12, Tone.MonoSynth);
  synth.toMaster();
  synth.volume.value = -20;
  onKeyDown(synth);
  onKeyUp(synth);
}());
