
function startup() {

    var root = "snd/";
    var audio = new Audio("snd/theme.mp3");
    audio.volume = 0.25;
    var click = new Audio("snd/click2.wav");
    click.volume = 0.50;

    var swap = new Audio("snd/woosh.wav");
    swap.volume = 0.25;

    var paused = true;
    var tetris = Elm.fullscreen(Elm.Tetris);

    tetris.recv('swap', function (event){
	if(event.value){
	    swap.play();
	}
    });

    tetris.recv('click', function (event){
	if(event.value){
	    click.play();
	}
    });

tetris.recv('play', function (event){
    
    if(!event.value) {
	audio.pause();
	paused = true;
	return;
    }
    if(!audio){
	audio = new Audio(root + "theme.mp3");
	audio.addEventListener('timeupdate', function () {
	    var sec = audio.currentTime;
	    if(sec > 37.6)
		audio.currentTime = 0.05;
	    }, false);
    }
    audio.play();

});
}

window.onload = startup;
