pitch2MNN = function (pitch) {
	// Tom Collins 6/1/2016.
	// In
	// metadata Integer mandatory
	// Out String
	// This function converts a MIDI note number into a pitch class and octave.
	// It does so in a completely naive manner (no consideration of global or
	// local key), but this is handy for things like Tone.js playback, which tend
	// to prefer "C" to "B#", "C#" to "Db" (I think), and "G" to "F##".

	switch (pitch) {
		case "C4":
			var MNN = 60;
			break;
		case "C#4":
			var MNN = 61;
			break;
		case "D4":
			var MNN = 62;
			break;
		case "D#4":
			var MNN = 63;
			break;
		case "E4":
			var MNN = 64;
			break;
		case "F4":
			var MNN = 65;
			break;
		case "F#4":
			var MNN = 66;
			break;
		case "G4":
			var MNN = 67;
			break;
		case "G#4":
			var MNN = 68;
			break;
		case "A4":
			var MNN = 69;
			break;
		case "A#4":
			var MNN = 70;
			break;
		case "B4":
			var MNN = 71;
			break;
		case "C5":
			var MNN = 72;
			break;
		case "C#5":
			var MNN = 73;
			break;
		default:
			var MNN = 60;
	}
	return MNN;
}


var startQos = 0; // Start time at the beginning of each loop.
var endQos = 0; // End time at the end of each loop

Tone.Transport.schedule(function (time) {

	// Log the current time.
	console.log('time:', time);
	var startPos = Tone.Transport.toSeconds(Tone.Transport.Position);
	startQos = Tone.Transport.toSeconds(startPos + "@ 16n");
	console.log('startQos:', startQos);

	// Empty notesOns and noteOffs from last time.
	noteOns = [];
	noteOffs = [];
}, 0)


//Synth instrument defined
var synth = new Tone.PolySynth(12, Tone.MonoSynth);
synth.toMaster();
synth.volume.value = -20;


var noteInfo = [];
var inverseParams = [noteInfo];
var count = 0;

//This function is what actually plays the inverse sequence - based on Tone.Transport 
function playInverse() {
	console.log("Inverse sequence Started:");
	Tone.Transport.cancel(0);

	Tone.Transport.schedule(function (time) {
		console.log("Starting Storage");
		ivNoteOns.map(function (n) {
			Tone.Transport.schedule(function (time) {

				inverseParams.push([n[1], 0.2, time, 0.5]);
				synth.triggerAttackRelease(n[1], 0.2, time, 0.5);

				//Draws inverse of played notes
				if (matrix2.matrix[count][NoteToRow(n[1])] == null) {
					matrix2.matrix[count][NoteToRow(n[1])] = 1;
				}
				matrix2.draw();

				count += 1;
			}, n[0]);
		})
		console.log("Storage Completed");
	}, 0);
	Tone.Transport.start();
}

