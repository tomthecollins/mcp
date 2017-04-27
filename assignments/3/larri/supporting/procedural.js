// Set up the Transport.

// Tone.Transport.start();
Tone.Transport.loop = true;
Tone.Transport.setLoopPoints(0, "6m");
Tone.Transport.bpm.value = 120;

var startQos = 0; // Start time at the beginning of each loop.
var curr_time;

// Function to control playing post load of audio material.
function make_things_happen(){
	Tone.Transport.start();
	}





Tone.Transport.setTimeLine(function(time){
 
 // Log the current time.
 curr_time = time;
 console.log('curr_time:', curr_time);
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
 
	var count_bass = 0;
	var count_snare = 0;
	noteOns.map(function(a){
		if (a[1] == "G4"){ // Encodes snare hits.
			count_snare = count_snare + 1;
		}
		if (a[1] == "F4"){ // Encodes snare hits.
			count_bass = count_bass + 1;
		}
	});
	console.log('count_snare:', count_snare);
	console.log('count_bass:', count_bass);
	
	var snare_patterns = ["p1", "p2", "p4", "p5", "p7", "p9"];
	var bass_patterns = ["p3", "p5", "p6", "p8"];
	if (count_snare >= count_bass){
		var segment_to_play = bass_patterns[Math.floor(Math.random()*bass_patterns.length)];
	}
	else {
		var segment_to_play = snare_patterns[Math.floor(Math.random()*snare_patterns.length)];
	}
	console.log('segment_to_play', segment_to_play);
	
	// Decision about what to play beginning measure 5.
	var pattern = patterns[segment_to_play];
	console.log('pattern:', pattern);
	pattern.map(function(a){
		
		// This chunk of code attempts to use a sample-based instrument.
		kit.triggerAttackRelease(
			0.25,
			curr_time + ((a.ontime - 1)/4 + 16)*60/Tone.Transport.bpm.value,
			0.8);
		
		
		// This chunk of code works but uses a synth instrument.
		// metronome3.triggerAttackRelease(
		// 	0.25,
		//	//((a.ontime - 1)/4 + 16)*60/Tone.Transport.bpm.value,
		//	curr_time + ((a.ontime - 1)/4 + 16)*60/Tone.Transport.bpm.value,
		//	0.8);
	});
 
}, 14*60/Tone.Transport.bpm.value)




var metronome2 = new Tone.NoiseSynth().toMaster();
var metronome3 = new Tone.NoiseSynth().toMaster();
var xcrptBeats = [0.001, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
for (bti = 0; bti < xcrptBeats.length; bti++){
	Tone.Transport.schedule(function(time){
		metronome2.triggerAttackRelease(
			0.25,
			time,
			0.2);
	}, xcrptBeats[bti]*60/Tone.Transport.bpm.value);
}
