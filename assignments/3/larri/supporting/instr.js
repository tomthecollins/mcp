var metronomeSamples = {
  "F2": "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/floor_tom_mf.wav",
  "F#2": "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hi-hat_closed_mf.wav",
};
var metronome = new Tone.PolySynth(1, Tone.Sampler, metronomeSamples, {
  "envelope": {
    "release": 0.2
  }
}).toMaster();
metronome.volume.value = 0;


var kit = new Tone.PolySynth(6, Tone.Sampler, {
          "60" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/bass_drum_f.wav",
          "C#2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/snare_drum_rim_mf.wav",
          "62" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/snare_mf.wav",
          "D#2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/clap_mf.wav",
          "E2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/snare_drum_short_mf.wav",
          "F2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/floor_tom_mf.wav",
          "61" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hi-hat_closed_mf.wav",
          "G2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hanging_tom_low_mf.wav",
          "G#2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hi-hat_pedal_mf.wav",
          "A2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hanging_tom_mid_mf.wav",
          "A#2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hi-hat_open_mf.wav",
          "B2" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/hanging_tom_high_mp.wav",
          "C3" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/cup_ride_cymbal_mf.wav",
          "C#3" : "https://dl.dropboxusercontent.com/u/11997856/samples/20150810/unpitched_percussion/drum_kits/drum_kit_1/crash_cymbal_mf.wav",
          
          
          
          //"C2" : "./data/bass_drum_mf.wav",
          //"D2" : "./data/snare_mf.wav",
          //"D#2" : "./data/clap_mf.wav",
          //"E2" : "./data/snare_drum_short_mf.wav",
          //"F#2" : "./data/hi-hat_closed_mf.wav",
          //"G#2" : "./data/hi-hat_pedal_mf.wav",
          //"A#2" : "./data/hi-hat_open_mf.wav"
        }).toMaster();
kit.volume = -10;

Tone.Note.route("hello", function(time, note, duration, velocity){
  kit.triggerAttackRelease(note, duration, time, velocity);
});
