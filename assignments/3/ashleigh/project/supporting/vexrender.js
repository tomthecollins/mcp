function copy_array_object(arr){
  // Tom Collins 21/2/2015.
  // In
  // arr Array mandatory
  // Out Array
  // This function returns an independent copy of an array object.

  var arr2 = JSON.parse(JSON.stringify(arr));
  return arr2;
}

function min_argmin(arr){
  // Tom Collins 21/10/2014.
  // Returns the minimum element in an array and its index (argument).
  
  var min = arr[0];
  var minIndex = 0;
  for (var i = 1; i < arr.length; i++) {
    if (arr[i] < min) {
      minIndex = i;
      min = arr[i];
    }
  }
  return [min, minIndex];
}


function duration2vexflow_string(duration, timeMod){
  // Tom Collins 13/3/2015
  // In
  // duration Number mandatory
  // timeMod Array optional
  // This function takes a duration in crotchet beats and attempts to find a
  // vexflow duration string representing that duration. The object timeMod has
  // the properties actualNotes and normalNotes, and assist with the rendering
  // of tuplets.
  
  if (timeMod !== undefined){
    var permissible_durations = [
      4, 3, 2, 1.75, 1.5, 1, 0.75, 0.5, 0.375, 0.25, 0.125, 0.0625
    ];
    var notating_dur = duration*timeMod.actualNotes/timeMod.normalNotes;
    // console.log('notating_dur:');
    // console.log(notating_dur);
    var rel_idx = min_argmin(permissible_durations.map(
      function (a){
        return Math.abs(a - notating_dur);
      }
    ));
    // console.log('rel_idx:');
    // console.log(rel_idx);
    duration = permissible_durations[rel_idx[1]];
    // console.log('duration:');
    // console.log(duration);
  }
  
  switch(duration){
    case 4:
      var duration_str = "w";
      break;
    // 10/12/2015 testing.
    case 3.5:
      var duration_str = "w";
      break;
    // 10/12/2015 end.
    case 3:
      var duration_str = "hd";
      break;
    case 2:
      var duration_str = "h";
      break;
    case 1.75:
      var duration_str = "qdd";
      break;
    case 1.5:
      var duration_str = "qd";
      break;
    case 1:
      var duration_str = "q";
      break;
    case 0.75:
      var duration_str = "8d";
      break;
    case 0.5:
      var duration_str = "8";
      break;
    case 0.375:
      var duration_str = "16d";
      break;
    case 0.25:
      var duration_str = "16";
      break;
    case 0.125:
      var duration_str = "32";
      break;
    case 0.0625:
      var duration_str = "64";
      break;
  }
  return duration_str;
}


function Render(compObj){
  
  var notes = compObj.notes;
  // var notes = expand_written_ties(compObj.notes);
  var rests = compObj.rests;
  var noteRests = notes.concat(rests);
  noteRests = noteRests.sort(function(a, b){
   return a.ontime - b.ontime;
  });
  console.log('noteRests:', noteRests);
  var all_ties = [];
  var setStrict = false;
  
  var ctx = renderer.getContext();
  ctx.clear();
  ctx.setFont("Arial", 10, "").setBackgroundFillStyle("#eed");
  
  var start = 60;
  var width = 700;
  
  var treble = new VF.Stave(start, 25, width);
  treble.addClef("treble");
  // Key signature.
  var fifthSteps = compObj.keySignatures[0].fifthSteps;
  var mode = compObj.keySignatures[0].mode;
  if (mode == 0){
    var vexKeyNames = {
      "0": "C", "1": "G", "-1": "F", "2": "D", "-2": "Bb",
      "3": "A", "-3": "Eb", "4": "E", "-4": "Ab",
      "5": "B", "-5": "Db", "6": "F#", "-6": "Gb"
    };
  }
  else{
    var vexKeyNames = {
      "0": "Cm", "1": "Gm", "-1": "Fm", "2": "Dm", "-2": "Bbm",
      "3": "Am", "-3": "Ebm", "4": "Em", "-4": "Abm",
      "5": "Bm", "-5": "Dbm", "6": "F#m", "-6": "Gbm"
    };
  }
  console.log(vexKeyNames[fifthSteps]);
  var keySig = new Vex.Flow.KeySignature(vexKeyNames[fifthSteps]);
  keySig.addToStave(treble);
  
	// Time signature.
	treble.addTimeSignature(4 + "/" + 4,
                         5 + 5*Math.abs(fifthSteps)
                        );
	//var timeSig = new Vex.Flow.TimeSignature(4 + "/" + 4,
	//	23 + 7*Math.abs(fifthSteps));
	//console.log('timeSig: ', timeSig);
	//timeSig.addToStave(treble);
	
	treble.setContext(ctx);
	treble.draw();
	
	var vf_notes = [];
	for (nri = 0; nri < noteRests.length; nri++){
		var curr_note = noteRests[nri];
		// Identify if a bar needs to be drawn.
		if (nri > 0){
			if (curr_note.barOn !== noteRests[nri - 1].barOn){
				var curr_barline = new Vex.Flow.BarNote();
				vf_notes.push(curr_barline);
			}
		}
		var resttf = false;
		var curr_pitch = curr_note.pitch;
		if (curr_pitch == undefined){
		 // Dealing with a rest.
		 resttf = true;
		 curr_pitch = "b";
		 var octave = 4;
		}
		else{
		 var octave = parseInt(curr_pitch[curr_pitch.length - 1]);
		 // console.log('octave:', octave);
			
		}
		var stemDir;
		if (octave >= 5) {
		  stemDir = -1;
		} else {
		  stemDir = 1;
		}
		var curr_dur = curr_note.duration;
		// Turn incoming note into VexFlow format.
		// vf_pitch = "c/4";
		var pitch_class = curr_pitch[0];
		pitch_class = pitch_class.toLowerCase();
		// console.log('pitch_class:', pitch_class);
		var vf_pitch = pitch_class + "/" + octave;
		// var vf_pitch = "c/4";
		// console.log('vf_pitch:', vf_pitch);
		var vf_dur = duration2vexflow_string(curr_dur);
		if (resttf){
		 vf_dur = vf_dur + "r";
		}
		// "F#4" -> "f/4"
		var vf_note = new VF.StaveNote({ keys: [vf_pitch], duration: vf_dur, stem_direction: stemDir});
		// Add any dot.
		if (vf_dur[vf_dur.length - 1] == 'd'){
			console.log('Attempt at dotting made!');
			vf_note.addDotToAll();
		}
		// Add any accidentals.
		var curr_accd = undefined;
		if (curr_note.accidental !== undefined){
			switch (curr_note.accidental){
				case "natural":
					curr_accd = "n";
					break;
				case "sharp":
					curr_accd = "#";
					break;
				case "flat":
					curr_accd = "b";
					break;
				case "double-sharp":
					curr_accd = "##";
					break;
				case "flat-flat":
					curr_accd = "bb";
					break;
			}
			vf_note.addAccidental(
				0,
				new Vex.Flow.Accidental(curr_accd)
			);
		}
		
		vf_notes.push(vf_note);
	}
		
	// var tuplet = new VF.Tuplet(notesTreble.slice(0, 5), {beats_occupied:4, bracketed:true, ratioed:true});
	// var beam = new VF.Beam(notesTreble.slice(0, 5));
	
	var voiceTreble = new VF.Voice({num_beats:12, beat_value: 4, resolution:Vex.Flow.RESOLUTION});
	
	voiceTreble.setStrict(false);
	voiceTreble.addTickables(vf_notes);
	
	var formatter = new VF.Formatter();
	formatter.format([voiceTreble], 575);
	//formatter.joinVoices([voiceTreble, voiceTreble2]);
	//formatter.joinVoices([voiceBass]);
	//formatter.format([voiceTreble, voiceTreble2, voiceBass], 350);
	
	var max_x = treble.getNoteStartX();
	treble.setNoteStartX(max_x + 15);
	//bass.setNoteStartX(max_x);
	
	voiceTreble.draw(ctx, treble);
	//voiceTreble2.draw(ctx, treble);
	//voiceBass.draw(ctx, bass);
	
	// draw tuplets
	// tuplet.setContext(ctx).draw();
		
	// draw beams
	// beam.setContext(ctx).draw();
}

function midiToNote(midiNum) {
	switch(midiNum){
		case 60: 
		case 61:
			return "c/4";
		case 62: 
			return "d/4";
		case 63:
		case 64:
			return "e/4";
		case 65:
		case 66:
			return "f/4";
		case 67:
		case 68: 
			return "g/4";
		case 69:
			return "a/4";
		case 70:
		case 71:
			return "b/4";
		case 72:
		case 73:
			return "c/5";
		case 74:
			return "d/5";
		case 75:
		case 76:
			return "e/5";
		case 77:
		case 78: 
			return "f/5";
		case 79:
		case 80:
			return "g/5";
		default:
			return "b/4";
	}
}

function midiCorrect(mnnEst, instChange) {
	for(var i = 0; i < mnnEst.length; i ++) {
		if (mnnEst[i] != 0) {
			mnnEst[i] += instChange;
		}
	}
}

function accidCheck(midiVal) {
	switch(midiVal) {
		case 61:
		case 66:
		case 68:
		case 73:
		case 78:
		case 80:
			return "#";
		case 63:
		case 70:
		case 75:
			return "b";
		default:
			return null;
	}
}

function durCheck(midiVal) {
	if(midiVal == 0) {
		return "qr";
	}
	else {
		return "q";
	}
}

function timeSigFile(fileName) {
	switch(fileName) {
		case "7.wav":
			return "3/4";
		default:
			return "4/4";
	}
}

function timeSigTopFile(fileName) {
	switch(fileName) {
		case "7.wav":
			return 3;
		default:
			return 4;
	}
}
