var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");

c.style.cursor = "none";

var width = 1900;
var height = 1055;

var floor_height = 50;

var playerSize = 40;
var playerXPos = 0;
var playerYPos = playerSize;

var jump = false;
var toJump = false;
var jump_timer = 0;
var falling = false;
var jump_height = 20;

var left = false;
var right = false;
var up = false;
var down = false;
var space = false;

var trace = [];

var moveX = 5;
var moveY = 10;

var level_index = 0;
var goal_x = width - 100;

var timer = [0];

var level_editor = false;
var playtest = false;

var mouse_x = 0;
var mouse_y = 0;

var start_x = 0;
var start_y = 0;

var end_x = 0;
var end_y = 0;

var add_block = false;
var mouse_down = false;

document.addEventListener('keydown', function(event) {
  if(event.keyCode == 32) {
      space = true;
  }
  if(event.keyCode == 37) {
      left = true;
  }
  if(event.keyCode == 38) {
      up = true;
  }
  if(event.keyCode == 39) {
      right = true;
  } 
  if(event.keyCode == 40) {
      down = true;
  }
  if(event.keyCode == 82) {
      if (!level_editor && !playtest) {
	  level_index = 0;
	  playerXPos = 0;
	  playerYPos = 0;
	  timer[level_index] = 0;
	  trace = [];
      }
  }
  if(event.keyCode == 80) {
      if  (playtest) {
	  playtest = false;
	  level_editor = true;
      }
      else if (level_editor) {
	  level_index = level.length-1;
	  playerXPos = 0;
	  playerYPos = 0;
	  level_editor = false;
	  playtest = true;
      }
  }
  if (event.keyCode == 85) {
      if (level_editor) {
	  level[level_index].pop();
      }
  }
  if (event.keyCode == 69) {
      playerXPos = 0;
      playerYPos = 0;
      timer[level_index] = 0;
      trace = [];
  }    
});

document.addEventListener('mousemove', function (event) {
  mouse_x = event.clientX-10;
  mouse_y = event.clientY-10;
});
document.addEventListener('mousedown', function (event) {
    mouse_down = true;
    start_x = mouse_x;
    start_y = mouse_y;
});
document.addEventListener('mouseup', function (event) {
    mouse_down = false;
    end_x = mouse_x;
    end_y = mouse_y;
    add_block = true;
});

document.addEventListener('keyup', function(event) {
  if(event.keyCode == 32) {
      space = false;
  }
  if(event.keyCode == 37) {
      left = false;
  }
  if(event.keyCode == 38) {
      up = false;
  }
  if(event.keyCode == 39) {
      right = false;
  } 
  if(event.keyCode == 40) {
      down = false;
  }
});

function col (a,b) {
    if ((b[0] <= a[0]+a[2] && a[0]+a[2] <= b[0] + b[2]) ||
	(b[0] <= a[0]      && a[0]      <= b[0] + b[2])) {
	if ((b[1] <= a[1]+a[3] && a[1]+a[3] <= b[1] + b[3]) ||
	    (b[1] <= a[1]      && a[1]      <= b[1] + b[3])) {
	    return true;
	}
    }
    return false;
}
    

function update () {    
    trace.push([playerXPos,playerYPos]);

    if (level_editor) return;
    
    timer[level_index] += 1;
    
    oldXPos = playerXPos;
    oldYPos = playerYPos;
    
    if (right) {
	playerXPos += moveX;
    }
    if (left) {
	playerXPos -= moveX;
    }
    if (space || up) {
	if (!jump && !falling) {
	    toJump = true;
	}
    }
    else {
	if (toJump) {
	    jump = true;
	    toJump = false;
	}
    }

    if (jump) {
	playerYPos += 2 * moveY;
	jump_timer += 1;
    }
    if (jump_timer >= jump_height) {
	jump = false;
	falling = true;
	jump_timer = 0;
    }    
    playerYPos -= moveY;
    
    for (var i = 0; i < level[level_index].length; i++) {
	if (col ([playerXPos,playerYPos,playerSize,playerSize],level[level_index][i])) {
	    if (col ([playerXPos,oldYPos,playerSize,playerSize],level[level_index][i])) {
		if (playerXPos < level[level_index][i][0])
		    playerXPos = level[level_index][i][0]-playerSize;
		else
		    playerXPos = level[level_index][i][0]+level[level_index][i][2];
	    }
	    else if (col ([oldXPos,playerYPos,playerSize,playerSize],level[level_index][i])) {
		if (playerYPos < level[level_index][i][1]) {
		    playerYPos = oldYPos;
		}
		else {
		    playerYPos = level[level_index][i][1] + level[level_index][i][3] + 1;
		    falling = false;
		}
	    }
	    else {
		playerXPos = oldXPos;
		playerYPos = oldYPos;
	    }
	}
    }

    if (playerYPos <= 0) {
	playerYPos = 0;
	falling = false;
    }

    if (playerXPos >= goal_x) {
	if (playtest) {
	    playtest = false;
	    level_index = 0;
	    playerXPos = 0;
	    playerYPos = 0;
	    timer[level_index] = 0;
	    return;
	}
	
	playerXPos = 0;
	playerYPos = 0;
	
	if (highscore[level_index] == -1 || timer[level_index] < highscore[level_index]) {
	    highscore[level_index] = timer[level_index];
	}

	var best = 10;
	for (var j = 9; j > 0; j--) {
	     if (ghosts[level_index][j].length == 0 || timer[level_index] < ghosts[level_index][j].length) {
		 best = j;
	     }
	}

	for (var i = 9; i >= best; i--) {
	    ghosts[level_index][i] = ghosts[level_index][i-1];
	}
	ghosts[level_index][best] = trace;
	
	trace = [];
	
	level_index += 1;

	if (timer.length <= level_index) {
	    timer.push(0);
	}

	timer[level_index] = 0;

	if (ghosts.length <= level_index) {
	    ghosts.push([[],[],[],[],[],[],[],[],[],[],[]]);
	}

	if (highscore.length <= level_index) {
	    highscore.push(-1);
	}
	
	if (level_index >= level.length) {
	    level_editor = true;
	    level.push([]);	    
	}
    }
}

function draw () {
    if (level_editor) {

	if (add_block) {
	    var tsx = start_x;
	    var tsy = height-floor_height-start_y;
	    var tex = end_x-start_x;
	    var tey = -(end_y-start_y);

	    if (tex < 0){
		tsx = tsx + tex;
		tex = -tex;
	    }
	    if (tey < 0){
		tsy = tsy + tey;
		tey = -tey;
	    }
	    
	    level[level_index].push([tsx,tsy,tex,tey]);
	    
	    add_block = false;
	}

	// Obsticales:
	ctx.fillStyle = "#00FF00";
	for (var i = 0; i < level[level_index].length; i++) {
	    ctx.fillRect(level[level_index][i][0],height-floor_height-level[level_index][i][1],level[level_index][i][2],-level[level_index][i][3]);
	}

	// Floor:
	ctx.fillStyle = "#FF0000";
	ctx.fillRect(0,height-floor_height,width,height);

	ctx.fillStyle = "#FF00FF";
	ctx.fillRect(mouse_x,mouse_y,10,10);

	if (mouse_down)
	    ctx.fillRect(start_x,start_y,mouse_x-start_x,mouse_y-start_y);
	
	return;
    }

    // Ghosts:
    for (var i = 0; i < ghosts[level_index].length ; i++) {
	if (ghosts[level_index][i].length > timer[level_index]) {
	    ctx.fillStyle = "#0000FF";
	    ctx.fillRect(ghosts[level_index][i][timer[level_index]][0],
			 height-floor_height-ghosts[level_index][i][timer[level_index]][1],
			 playerSize,-playerSize);
	}
    }
    
    // Player:
    ctx.fillStyle = "#0000FF";
    ctx.fillRect(playerXPos,height-floor_height-playerYPos,playerSize,-playerSize);
    
    // Obsticales:
    ctx.fillStyle = "#00FF00";
    for (var i = 0; i < level[level_index].length; i++) {
	ctx.fillRect(level[level_index][i][0],height-floor_height-level[level_index][i][1],level[level_index][i][2],-level[level_index][i][3]);
    }

    // Floor:
    ctx.fillStyle = "#FF0000";
    ctx.fillRect(0,height-floor_height,width,height);

    // Goal:
    ctx.fillStyle = "#00FFFF";
    ctx.fillRect(goal_x,0,50,height);
    
    ctx.fillStyle = "#000000";
    ctx.font = "40px Georgia";
    ctx.fillText(timer[level_index],10,40);

    if (highscore[level_index] == -1) {
	ctx.fillText("00000",width-140,40);
    }
    ctx.fillText(highscore[level_index],width-30-25*Math.log(highscore[level_index]+1)/Math.log(10),40);

    ctx.fillText("Level: " + level_index, width/2-80-25*Math.log(level_index+1)/Math.log(10), 40);
}
    
function loop() {
    ctx.clearRect(0,0,width,height);
    draw();
    update();
}

setInterval(loop,10);
