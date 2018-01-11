// Set up the Transport.

// Tone.Transport.start();
Tone.Transport.loop = true;
Tone.Transport.setLoopPoints(0, "6m");
Tone.Transport.bpm.value = 80;

var startQos = 0; // Start time at the beginning of each loop.

Tone.Transport.schedule(function(time){
 
 // Log the current time.
 console.log('time:', time);
 // var startPos = Tone.Transport.toSeconds(Tone.Transport.Position);
 // startQos = Tone.Transport.toSeconds(startPos + "@ 16n");
 // console.log('startQos:', startQos);
 
 // Empty notesOns and noteOffs from last time.
 noteOns = [];
 noteOffs = [];
 
 // Prepare and submit information from the loop.
 // var loopInfo = {
 //  "subID": subID,
 //  "expSt": expSt,
 //  "stage": "training",
 //  "piece": pieceNames[pieceIdx],
 //  "barIdx": barIdx,
 //  "voiceChoice": voiceChoice,
 //  "tempo": Tone.Transport.bpm.value,
 //  "recall": recall,
 //  "noteOns": noteOns,
 //  "noteOffs": noteOffs,
 //  "targOns": targOns,
 //  "targOffs": targOffs
 // }
 // $.post("update.php",
 //        { yoyo: JSON.stringify(loopInfo) });
 //	// Empty the noteOns variable.
 //	noteOns = [];
 //	noteOffs = [];
 // Empty the compObj?

}, 0)


// Collect what's been played and analyze it.
Tone.Transport.schedule(function(time){
	console.log('Do some analysis here!');
 console.log('noteOns', noteOns);
 
 
 
 
 
}, 14*60/Tone.Transport.bpm.value)

var metronome2 = new Tone.NoiseSynth().toMaster();
var xcrptBeats = [0.001, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
for (bti = 0; bti < xcrptBeats.length; bti++){
	Tone.Transport.schedule(function(time){
		metronome2.triggerAttackRelease(
			.25,
			time,
			.6);
	}, xcrptBeats[bti]*60/Tone.Transport.bpm.value);
}
