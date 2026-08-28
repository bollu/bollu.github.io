// === homepage filter: done/draft toggle ===
// article pages have no #filter-bar, so this is a no-op there.
document.addEventListener('DOMContentLoaded', function () {
	var bar = document.getElementById('filter-bar');
	if (!bar) return;

	var rows = document.querySelectorAll('#post-list .post-row');
	var statusPills = bar.querySelectorAll('[data-status-filter]');

	function applyFilter(activeStatus) {
		for (var i = 0; i < rows.length; i++) {
			var row = rows[i];
			row.classList.toggle('hidden', activeStatus !== 'all' &&
				row.getAttribute('data-status') !== activeStatus);
		}
		for (var i = 0; i < statusPills.length; i++) {
			statusPills[i].classList.toggle('is-active',
				statusPills[i].getAttribute('data-status-filter') === activeStatus);
		}
	}

	for (var i = 0; i < statusPills.length; i++) {
		statusPills[i].addEventListener('click', function () {
			applyFilter(this.getAttribute('data-status-filter'));
		});
	}
});

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