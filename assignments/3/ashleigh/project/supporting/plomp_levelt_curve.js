// This is an encoding of a curve from Plomp and Levelt's (1965) paper on sensory dissonance.

var knots = [0,0.029411764705882353,0.058823529411764705,0.088235294117647065,0.11764705882352941,0.14705882352941177,0.17647058823529413,0.20588235294117646,0.23529411764705882,0.25,0.26470588235294118,0.29411764705882354,0.3235294117647059,0.35294117647058826,0.38235294117647056,0.41176470588235292,0.44117647058823528,0.47058823529411764,0.5,0.52941176470588236,0.55882352941176472,0.58823529411764708,0.61764705882352944,0.6470588235294118,0.67647058823529416,0.70588235294117652,0.73529411764705888,0.76470588235294112,0.79411764705882348,0.82352941176470584,0.8529411764705882,0.88235294117647056,0.91176470588235292,0.94117647058823528,0.97058823529411764,1,1.0294117647058822,1.0588235294117647,1.088235294117647,1.1176470588235294,1.1470588235294117,1.1764705882352942,1.2];

var valus = [0,0.23460410557184752,0.47507331378299122,0.66275659824046917,0.7917888563049853,0.873900293255132,0.92668621700879761,0.97360703812316718,0.99120234604105573,1,0.99120234604105573,0.97360703812316718,0.93255131964809379,0.88563049853372433,0.82111436950146632,0.76246334310850439,0.70087976539589447,0.63343108504398826,0.58064516129032262,0.5161290322580645,0.46041055718475071,0.40469208211143692,0.35777126099706746,0.31085043988269795,0.26979472140762462,0.23460410557184752,0.19941348973607037,0.17008797653958943,0.1466275659824047,0.12316715542521994,0.10263929618768329,0.082111436950146624,0.064516129032258063,0.052785923753665691,0.041055718475073312,0.02932551319648094,0.023460410557184751,0.017595307917888565,0.011730205278592375,0.0087976539589442824,0.0058651026392961877,0.0029325513196480938,0];

pick_peaks = function(arr){
	var arr_out = [];
	for (i = 0; i < arr.length - 2; i++){
		if (arr[i + 1] > arr[i] && arr[i + 1] >= arr[i + 2]){
			arr_out.push([arr[i + 1], i + 1]);
		}
	}
	return arr_out;
}

sensory_dissonance = function(md_arr, freq_spacing){
	
	var diss_contrib = [];
	// Incorporate frequency spacing to calculate frequencies from indices.
	md_arr = md_arr.map(function(pk_n_idx){
		return [pk_n_idx[0], freq_spacing*pk_n_idx[1]];
	});
	var mdn = md_arr.length;
	// Nested loop over md_arr to calculate the dissonance contributions.
	for (i = 0; i < mdn; i++){
		var pknf1 = md_arr[i];
		for (j = i + 1; j < mdn; j++){
			var pknf2 = md_arr[j];
			var df = Math.abs(pknf1[1] - pknf2[1]); // Abs frequency difference.
			// Calculate the critical bandwidth. (The closer the two frequency
			// components, the greater the contribution to sensory dissonance,
			// subject to further processing by the Plomp and Levelt curve.)
			var cbw = 1.72*Math.pow(((pknf1[1] + pknf2[1])/2), 0.65);
			var mbw = 1.2*cbw; // Modified bandwidth.
			var dfdc = df/cbw;
				
			if (df < mbw){
				// console.log('pknf1:', pknf1);
				// console.log('pknf2:', pknf2);
				// console.log('df:', df);
				// console.log('cbw:', cbw);
				// console.log('mbw:', mbw);
				// console.log('dfdc:', dfdc);
				
				// This is old and I think wrong...
				// var h = Math.floor(dfdc/mbw*205); // This is giving an index into the curve.
				// var h = Math.floor(dfdc/mbw*205);
				var h = knots.findIndex(function(k){
					return dfdc < k;
				});
				// console.log('h:', h);
				var fac = valus[h - 1] + (valus[h] - valus[h - 1])*(dfdc/mbw - knots[h - 1])/(knots[h] - knots[h - 1]);
				if (!isNaN(fac)){
					diss_contrib.push({
						"pknf1": pknf1,
						"pknf2": pknf2,
						"fac": fac,
						"diss": fac*pknf1[0]*pknf2[0]
					});
				}
			}
		}
	}
	return diss_contrib;
}

