// Plot of peaks.
var canvas = document.getElementById("fp");
canvas.width = 800;
canvas.height = 600;
var ctx = canvas.getContext("2d");

var backgroundImage = new Image();
backgroundImage.src = 'http://tomcollinsresearch.net/mcpc/spring2018/shazam/img/peaks.png';
backgroundImage.onload = function() {
  ctx.drawImage(backgroundImage, 0, 0);
}

// Define parameters for fingerprint construction.
var timeThresMin = 0.1;
var timeThresMax = 1;
var fIdxThresMin = 0;
var fIdxThresMax = 150;
var step = 1024;
var Fs = 44100;

// Locations of red crosses expressed as [x,y] pairs.
var V = [
  [3, 104],
  [3, 368],
  [4, 277],
  [4, 371],
  [4, 461],
  [5, 92],
  [6, 104],
  [6, 184],
  [12, 164],
  [12, 492],
  [14, 415],
  [15, 247],
  [15, 328],
  [15, 410],
  [17, 82],
  [18, 164],
  [25, 234],
  [25, 386],
  [26, 156],
  [26, 461],
  [27, 78],
  [27, 311],
  [27, 467],
  [35, 77],
  [36, 70],
  [36, 209],
  [36, 485],
  [37, 344],
  [37, 411],
  [38, 139],
  [38, 417],
  [39, 70],
  [39, 276],
  [44, 186],
  [45, 62],
  [47, 124],
  [47, 307],
  [47, 428],
  [48, 367],
  [49, 183],
  [55, 176],
  [56, 59],
  [57, 117],
  [57, 289],
  [57, 408],
  [58, 404],
  [66, 103],
  [68, 155],
  [68, 461],
  [69, 53],
  [69, 309],
  [69, 312],
  [71, 104],
  [77, 105],
  [79, 92],
  [79, 138],
  [80, 47],
  [80, 231],
  [90, 164],
  [91, 42],
  [91, 82],
  [91, 123],
  [91, 206]
];

// Create fingerprints.
function createFP(md_arr) {
  var fp = [];
  var npeak = md_arr.length;
  console.log('npeak:', npeak)
  // Iterate over the first peak of a pair.
  for (ii = 0; ii < npeak; ii++) {
    var ind1 = md_arr[ii];
    // Iterate over the second peak of a pair.
    var jj = ii + 1;
    while (jj < npeak) {
      var ind2 = md_arr[jj];
      var time_diff = (ind2[0] - ind1[0]) * step / Fs;
      var fidx_diff = Math.abs(ind2[1] - ind1[1]);
      // Decide whether to make a fingerprint.
      if (Math.random() > 0.9) {
        fp.push(md_arr[ii].concat(md_arr[jj]));
      }
      if (time_diff >= timeThresMax) {
        jj = npeak - 1;
      }
      jj++;
    }
  }
  var nfp = fp.length;
  console.log('nfp:', nfp);

  // Draw them.
  ctx.drawImage(backgroundImage, 0, 0);
  Tone.Transport.cancel(0);
  fp.map(function(ind12, idx) {
    Tone.Transport.schedule(function(time) {
      Tone.Draw.schedule(function() {
        //
        ctx.beginPath();
        var x1 = 100 + 6.2 * ind12[0];
        var y1 = 537 - 0.98 * ind12[1];
        var x2 = 100 + 6.2 * ind12[2];
        var y2 = 537 - 0.98 * ind12[3];
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.lineWidth = 0.25;
        ctx.strokeStyle = "blue";
        ctx.stroke();
      }, time)
    }, 0.01 * idx)
  });
  Tone.Transport.start();
  // Stop the Transport.
  Tone.Transport.schedule(function(time) {
    Tone.Transport.stop();
  }, 0.01 * fp.length + 1);

  console.log('fp:', fp);
  return fp;
}

function clearFP() {
  // Tone.Transport.cancel(0);
  ctx.drawImage(backgroundImage, 0, 0);
}

// Define play/pause buttons..
var playProbeBtn = document.getElementById('playProbe');
var stopProbeBtn = document.getElementById('stopProbe');
var playProbeSound = function(){
	audio.play();
};
playProbeBtn.addEventListener('click', playProbeSound, false);
stopProbeBtn.addEventListener('click', function(){ audio.pause() }, false);

var playProbe2Btn = document.getElementById('playProbe2');
var stopProbe2Btn = document.getElementById('stopProbe2');
var playProbe2Sound = function(){
	audio4.play();
};
playProbe2Btn.addEventListener('click', playProbe2Sound, false);
stopProbe2Btn.addEventListener('click', function(){ audio4.pause() }, false);

var playGuitarBtn = document.getElementById('playGuitar');
var stopGuitarBtn = document.getElementById('stopGuitar');
var playGuitarSound = function(){
	audio1.play();
};
playGuitarBtn.addEventListener('click', playGuitarSound, false);
stopGuitarBtn.addEventListener('click', function(){ audio1.pause() }, false);

var playPianoBtn = document.getElementById('playPiano');
var stopPianoBtn = document.getElementById('stopPiano');
var playPianoSound = function(){
	audio2.play();
};
playPianoBtn.addEventListener('click', playPianoSound, false);
stopPianoBtn.addEventListener('click', function(){ audio2.pause() }, false);

var playStringsBtn = document.getElementById('playStrings');
var stopStringsBtn = document.getElementById('stopStrings');
var playStringsSound = function(){
	audio3.play();
};
playStringsBtn.addEventListener('click', playStringsSound, false);
stopStringsBtn.addEventListener('click', function(){ audio3.pause() }, false);

