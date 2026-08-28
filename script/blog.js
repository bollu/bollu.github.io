// === homepage filter: topic bubbles + done/draft toggle ===
// article pages have no #filter-bar, so this is a no-op there.
document.addEventListener('DOMContentLoaded', function () {
	var bar = document.getElementById('filter-bar');
	if (!bar) return;

	var activeStatus = 'all';
	var activeTopic = null; // null = all topics
	var rows = document.querySelectorAll('#post-list .post-row');
	var statusPills = bar.querySelectorAll('[data-status-filter]');
	var topicPills = bar.querySelectorAll('[data-topic-filter]');

	function applyFilters() {
		for (var i = 0; i < rows.length; i++) {
			var row = rows[i];
			var okStatus = activeStatus === 'all' ||
				row.getAttribute('data-status') === activeStatus;
			var okTopic = activeTopic === null ||
				row.getAttribute('data-topics').indexOf(',' + activeTopic + ',') !== -1;
			row.classList.toggle('hidden', !(okStatus && okTopic));
		}
		for (var i = 0; i < statusPills.length; i++) {
			statusPills[i].classList.toggle('is-active',
				statusPills[i].getAttribute('data-status-filter') === activeStatus);
		}
		for (var i = 0; i < topicPills.length; i++) {
			topicPills[i].classList.toggle('is-active',
				topicPills[i].getAttribute('data-topic-filter') === activeTopic);
		}
	}

	for (var i = 0; i < statusPills.length; i++) {
		statusPills[i].addEventListener('click', function () {
			activeStatus = this.getAttribute('data-status-filter');
			applyFilters();
		});
	}
	for (var i = 0; i < topicPills.length; i++) {
		topicPills[i].addEventListener('click', function () {
			var topic = this.getAttribute('data-topic-filter');
			activeTopic = (activeTopic === topic) ? null : topic; // click again to clear
			applyFilters();
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