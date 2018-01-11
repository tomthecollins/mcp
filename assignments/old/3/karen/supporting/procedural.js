pitch2MNN = function(pitch){
  // Tom Collins 6/1/2016.
  // In
  // metadata Integer mandatory
  // Out String
  // This function converts a MIDI note number into a pitch class and octave.
  // It does so in a completely naive manner (no consideration of global or
  // local key), but this is handy for things like Tone.js playback, which tend
  // to prefer "C" to "B#", "C#" to "Db" (I think), and "G" to "F##".

  switch(pitch){
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


Array.prototype.equals = function(array){
  // Joe on Stack Overflow 27/12/2014.
  // In
  // array Array mandatory
  // Out Boolean
  // Returns true if two arrays are equal, and false otherwise.
  // http://stackoverflow.com/questions/7837456/comparing-two-arrays-in-javascript

  // If the other array is a falsy value, return.
  if (!array)
  return false;

  // Compare lengths.
  if (this.length != array.length)
  return false;

  for (var i = 0, l=this.length; i < l; i++){
    // Check if we have nested arrays.
    if (this[i] instanceof Array && array[i] instanceof Array){
      // Recurse into the nested arrays.
      if (!this[i].equals(array[i]))
      return false;
    }
    else if (this[i] != array[i]){
      // Warning - two different object instances will never be equal:
      // {x:20} != {x:20}.
      return false;
    }
  }
  return true;
}

// Set up the Transport.

// Tone.Transport.start();
Tone.Transport.loop = false;
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
	var diff_mtx = [];
	var diff_adj = [];
	for (var i = 0; i < noteOns.length; i++){
		diff_mtx[i] = [];
		for (var j = 0; j < noteOns.length; j++){
			if (j > i){
				diff_mtx[i][j] = noteOns[j][0] - noteOns[i][0];
			}
			if (j - i == 1){
				diff_adj.push(noteOns[j][0] - noteOns[i][0]);
			}
		}
	}
	console.log('diff_mtx:', diff_mtx);
	console.log('diff_adj:', diff_adj);
	
	// Setting up chordal analysis.
	var chord_lookup = [[57,58], [57,59], [59,60], [60, 61], [60, 62], [65, 66, 67], [61,62], [61,63], [62,63],[63,64], [62,64], [64,65], [65,66], [65,67], [66,67], [66,68], [67,69], [67,68], [68,70], [69,71], [69,70], [71,72], [72,73]];
	var chords = [[noteOns[0][1]]];
	// Tempo analysis.
	var tmp_arr = [];
	for (i = 0; i < diff_adj.length; i++){
		if (diff_adj[i] > 0.02){
			tmp_arr.push(diff_adj[i]);
			chords.push([noteOns[i + 1][1]]);
		}
		else {
			chords[chords.length - 1].push(noteOns[i + 1][1]);
		}
	}
	console.log('tmp_arr:', tmp_arr);
	console.log('chords:', chords);
	
	// A bit more chordal analysis.
	chords = chords.filter(function(c){
		return c.length > 1;
	});
	
	chords = chords.map(function(c){
		c = c.map(function(n){
			return pitch2MNN(n);
		});
		return c.sort(function(a, b){ return a - b; });
	});
	console.log('chords:', chords);
	
	var tonal_decision = 'tonal';
	chords.map(function(c){
		chord_lookup.map(function(l){
			if (c.equals(l)){
				tonal_decision = 'atonal';
			}
		});
	});
	console.log('tonal decision:', tonal_decision);
	
	var fast_count = 0;
	for (i = 0; i < tmp_arr.length; i++){
		if (tmp_arr[i] < 0.5){
			fast_count = fast_count + 1;
		//if (diff_adj[i] > 0.5){
		//	
		//}
		}
	}
	console.log('fast_count:', fast_count);
 
	// Density analysis.
	var dense_count = 0;
	for (i = 0; i < diff_adj.length; i++){
		if (diff_adj[i] < 0.02){
			dense_count = dense_count + 1;
		//if (diff_adj[i] > 0.02){
		//	=then classify as "low density"
		//}
		}
	}
	console.log('dense_count:', dense_count);
	
	var dense_thresh = 2;
	var fast_thresh = 3;
	var elem = document.createElement("img");
	elem.setAttribute("height", "768");
	elem.setAttribute("width", "1024");
	var elem2 = document.createElement("p");
	if (tonal_decision == 'tonal'){
		if (dense_count > dense_thresh){
			if (fast_count > fast_thresh){
				// Donut 1.
				console.log("I'm gonna show donut 1.");
				elem.src = './supporting/devils food donut (dense, quick, atonal).png';
				elem2.innerHTML = 'You are a DEVILS FOOD CAKE DONUT! Your music is fast, complex, and full of rich chords. Naughty yet oh so satisfying!' ;
			}
			else {
				// Donut 2.
				console.log("I'm gonna show donut 2.");
				elem.src = './supporting/boston cream pie (dense, slow atonal) .jpg';
				elem2.innerHTML = 'You are a BOSTON CREAM DONUT! Like this delicious flavor combination, your music is complex but with lots of complementary notes and deserves to be savored slowly.';
				
			}
		}
		else {
			if (fast_count > fast_thresh){
				// Donut 3.
				console.log("I'm gonna show donut 3.");
				elem.src = './supporting/frosted with sprinkles (low density, fast, tonal).jpg';
				elem2.innerHTML = 'You are a FROSTED DONUT WITH SPRINKLES! Like this childhood favorite donut, your music is simple, bright and fun.';
				
			}
			else {
				// Donut 4.
				console.log("I'm gonna show donut 4.");
				elem.src = './supporting/krispy creme (low density, slowly, tonal).jpg';
				elem2.innerHTML = 'You are a GLAZED KRISPY KREME DONUT! Like this old classic, your music is simple, light and airy.';
				
			}
			
		}
	}
	else {
		if (dense_count > dense_thresh){
			if (fast_count > fast_thresh){
				// Donut 1.
				console.log("I'm gonna show donut 5.");
				elem.src = './supporting/photo-4-1.jpg';
				elem2.innerHTML = 'You are a PICKLE DONUT! Like this bizarre creation, your music is busy, complex, and full of unusual combinations. Yes, people might not "get" you off the bat, but once they try you out they might be pleasantly surprised.';
			}
			else {
				// Donut 2.
				console.log("I'm gonna show donut 6.");
				elem.src = './supporting/sour cream donut (dense, slow, atonal).jpg';
				elem2.innerHTML = 'You are a SOUR CREAM DONUT! Like this rich delicacy, your deep and complex music deserves to be savored slowly.';
				
			}
		}
		else {
			if (fast_count > fast_thresh){
				// Donut 3.
				console.log("I'm gonna show donut 7.");
				elem.src = './supporting/apple cider donut (low density, quickly, atonal).jpg';
				elem2.innerHTML = 'You are an CINNAMON SUGAR CAKE DONUT! Like this autumn favorite, your music is simple yet messy. Listening to you can lead to a nostalgic aftertaste comparable to cinnamon sugar licked off of fingers.';
				
			}
			else {
				// Donut 4.
				console.log("I'm gonna show donut 8.");
				elem.src = './supporting/jelly donut (low density, slow, atonal).jpeg';
				elem2.innerHTML = 'You are a JELLY DONUT! Like the elderly population who most enjoys jelly donuts, your music requires patience and can be an acquired taste. Those who appreciate it, however, cannot get enough of your deliberate, sophisticated style.';
				
			}
			
		}
	}
	
 document.getElementById("placehere").appendChild(elem);
 document.getElementById("placehere").appendChild(elem2);
 
}, 14*60/Tone.Transport.bpm.value)

var metronome2 = new Tone.NoiseSynth().toMaster();
//var xcrptBeats = [0.001, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
//for (bti = 0; bti < xcrptBeats.length; bti++){
//	Tone.Transport.schedule(function(time){
//		metronome2.triggerAttackRelease(
//			.25,
//			time,
//			.6);
//	}, xcrptBeats[bti]*60/Tone.Transport.bpm.value);
//}
