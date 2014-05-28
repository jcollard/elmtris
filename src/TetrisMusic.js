
function startup() {

    var root = "snd/";
    var audio = new Audio("snd/theme.mp3");
    audio.volume = 0.10;
    var click = new Audio("snd/click2.wav");
    click.volume = 0.50;

    var swap = new Audio("snd/woosh.wav");
    swap.volume = 0.50;

    var paused = true;
    var tetris = Elm.fullscreen(Elm.Tetris);

    tetris.ports.playSwap.subscribe(function (event){
	if(event){
	    swap.play();
	}
    });

    tetris.ports.playClick.subscribe(function (event){
	if(event){
	    click.play();
	}
    });

    tetris.ports.playTheme.subscribe(function (event){
    
        if(!event) {
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
