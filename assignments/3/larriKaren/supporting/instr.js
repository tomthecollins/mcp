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
