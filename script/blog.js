window.onload = function () {
	var abcElements = document.querySelectorAll('.abc');
	for(var i = 0; i < abcElements.length; i++){
	    var abc_text = abcElements[i].innerHTML;
		var visualObj = ABCJS.renderAbc(abcElements[i], abc_text, { add_classes: true, responsive: "resize"  });

		const controlDiv = document.createElement("div");
	    abcElements[i].insertAdjacentElement("afterend", controlDiv);

		var synthControl = new ABCJS.synth.SynthController();
		synthControl.load(controlDiv, null, {
            displayLoop: true, 
            displayRestart: true, 
            displayPlay: true, 
            displayProgress: true, 
            displayWarp: true
        });

	   	var createSynth = new ABCJS.synth.CreateSynth();
		var AUDIO_PARAMS = { chordsOff: true };

		createSynth.init({ visualObj: visualObj[0] }).then(function () {
			synthControl.setTune(visualObj[0], false, AUDIO_PARAMS).then(function () {
				console.log("Audio successfully loaded.")
			}).catch(function (error) {
				console.warn("Audio problem:", error);
			});
		}).catch(function (error) {
			console.warn("Audio problem:", error);
		});
	}
}