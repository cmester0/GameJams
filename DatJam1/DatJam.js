var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");

var width = 1900;
var height = 1055;

var playerSize = 40;
var playerXPos = 0;
var playerYPos = 0;

var left = false;
var right = false;
var up = false;
var down = false;
var space = false;

var stepSize = 10;

var playerColor = "#FF0000";

var shoots = [];
var snake = [];

var enemiesXpos = width-playerSize ;
var enemiesYpos = height-playerSize;

var enemiesLife = 100;
var playerLife = 100;
var points = 0;

function drawFigure (x, y, size, color) {
  ctx.fillStyle = color;
  ctx.fillRect(x,y,size,size);
}

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

function shoot() {
    if (!((!left && right) || (left && !right) || (up && !down) || (!up && down))) return;
    var velX = 0;
    var velY = 0;
    
    if (left)
	velX -= 10;
    
    if (down)
	velY += 10;
    
    if (right)
	velX += 10;
    
    if (up)
	velY -= 10;

    console.log(velX);
    console.log(velY);
    
    
    shoots.push([playerXPos,playerYPos,velX,velY,0]);
}

function update () {
    if (space) {
	shoot();
    }    
    if (left) {
	playerXPos -= stepSize;
    }
    if (down) {
	playerYPos += stepSize;
    }
    if (right) {
	playerXPos += stepSize;
    }
    if (up) {
	playerYPos -= stepSize
    }

    playerXPos = (width + playerXPos) % width;
    playerYPos = (height + playerYPos) % height;
    
    enemiesXpos = (width + enemiesXpos + (playerXPos - enemiesXpos) * 0.1 ) % width;
    enemiesYpos = (height + enemiesYpos  + (playerYPos - enemiesYpos) * 0.1) % height;
}

function draw () {
    drawFigure(playerXPos,playerYPos,playerSize,playerColor);
    for (var i = 0; i < shoots.length; i++) {
       drawFigure(shoots[i][0],shoots[i][1],10,"#FF00FF");
    }
    drawFigure(enemiesXpos,enemiesYpos,playerSize,"#0000FF");
    
    ctx.fillStyle = "#FF0000";
    ctx.fillRect(1,1,playerLife,10);
    ctx.fillStyle = "#0000FF";
    ctx.fillRect(width,1,-enemiesLife,10);
    ctx.fillStyle = "#FFFF00";
    ctx.fillRect(width/2 - points,1,points,10);    
}

function loop() {
    if (playerLife < 0) {
	ctx.clearRect(0,0,width,height);
	ctx.fillStyle = "#FFFF00";
	ctx.font = "30px Arial";
	ctx.fillText("Points:" + points,width/2-50,height / 2);
	// ctx.fillRect(width/2 - points,1,points,10);
	return;
    }
    
    ctx.clearRect(0,0,width,height);
    draw();
    update();
    for (var i = 0; i < shoots.length; i++) {
	shoots[i][0] = (width + shoots[i][0] + shoots[i][2]) % width;
	shoots[i][1] = (height +shoots[i][1] + shoots[i][3]) % height;
	shoots[i][4] = shoots[i][4] + 1;

	if (Math.abs(enemiesXpos - shoots[i][0]) <= playerSize) {
	    if (Math.abs(enemiesYpos - shoots[i][1]) <= playerSize) {
		enemiesLife -= 1;

		if (enemiesLife <= 0) {
		    points += 1;
		    enemiesLife = 100;
		    enemiesXpos = width - playerSize;
		    enemiesYpos = height - playerSize;
		}
	    }
	}

	if (shoots[i][4] > 100) {
	    shoots.splice(i,1);
	}
    }


    if (Math.abs(enemiesXpos - playerXPos) <= playerSize) {
	if (Math.abs(enemiesYpos - playerYPos) <= playerSize) {
	    playerLife -= 1;
	}
    }
}

setInterval(loop,20);

