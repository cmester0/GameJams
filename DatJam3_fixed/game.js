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

var level = [[[100,0,50,100],[300,0,50,200],[350,0,50,100]],[[295,4,89,94],[460,205,113,81],[474,115,10,99],[407,58,138,103],[1247,-33,129,59],[1273,-33,103,81],[1313,21,58,277],[1336,191,103,161],[695,306,108,66],[879,321,104,61],[1078,288,101,86],[1246,259,103,49]],[[357,6,117,135],[569,155,143,35],[753,204,22,174],[777,389,0,0],[619,273,32,40],[832,425,144,13],[1189,426,55,25],[1358,407,38,31],[1480,391,73,36],[1559,-11,75,395]],[[1634,-11,0,0],[363,-18,49,58],[443,-47,52,126],[555,-17,75,144],[671,-13,96,208],[828,-16,76,281],[974,-21,119,405],[646,321,147,98],[873,378,132,154],[779,297,32,43],[625,-21,435,69],[954,22,43,108]],[[1631,-12,82,376],[1617,333,52,265],[1589,582,50,263],[1407,-20,73,150],[1381,-11,51,62],[1456,120,23,186],[1444,210,16,23],[928,373,457,99],[1376,355,32,33],[1094,471,34,92],[1222,629,84,48],[1185,757,319,35]],[[1147,809,0,0],[500,-1,45,902],[500,1,45,112],[500,9,45,257],[500,8,45,439],[500,12,45,604],[500,13,45,797],[636,973,430,38],[1512,532,130,47],[1778,-7,50,262]],[[19,980,3,0],[0,969,1909,47],[1655,-1,76,218],[102,0,67,915],[46,109,62,38],[49,213,60,47],[41,349,64,48],[40,451,66,32],[47,534,66,33],[45,626,64,49],[43,742,63,32],[37,850,73,33],[1498,53,76,934],[158,855,1264,56],[249,741,1257,59],[131,649,1317,46],[227,542,1293,44],[142,449,1303,56],[220,328,1297,57],[139,200,1294,68],[228,106,1293,54]],[[228,-32,77,58],[290,-11,102,74],[384,-36,150,150],[510,-8,212,177],[710,-20,313,272],[978,-23,365,402],[1483,-63,124,582],[1076,453,180,88],[1237,446,156,38],[900,516,225,127],[729,590,198,130],[626,710,184,97],[466,803,202,60],[335,855,148,120],[570,987,1339,29],[1593,483,141,514]],[[1763,224,0,0],[1763,224,0,0],[1735,0,50,959],[69,58,1613,52],[30,72,38,924],[118,167,1630,73],[33,296,1656,88],[115,463,1618,44],[42,565,1636,53],[135,670,1618,79],[38,811,1645,71],[1732,963,58,47],[1738,972,43,24],[1728,967,53,37]],[[380,-7,94,280],[229,139,175,45],[0,259,129,39],[377,288,97,16],[379,323,89,16],[379,365,91,14],[380,273,90,16],[369,307,101,13],[384,347,85,15],[386,298,98,12],[396,312,78,34],[403,345,80,31],[371,414,156,4],[316,464,259,29],[623,96,72,457],[689,10,409,36],[930,-10,83,376],[848,319,191,211],[745,152,241,52],[653,373,121,17]],[[663,11,604,164],[856,152,232,380],[863,529,145,219],[863,482,205,286],[871,729,187,48],[885,758,157,36],[910,760,121,46],[932,787,64,30],[642,-3,650,147],[735,152,418,44],[955,845,17,59],[856,936,63,10],[739,899,19,37],[641,811,25,37],[534,696,54,66],[466,590,32,46],[364,473,52,54],[321,339,2,76],[291,323,89,90],[696,295,58,69],[1056,883,130,57],[1275,766,65,66],[1448,517,34,114],[1416,307,53,101],[1594,144,88,74],[1165,502,60,75],[172,200,54,43],[111,69,71,62],[57,-12,102,57],[629,788,65,29],[724,878,62,23],[628,-3,674,82],[652,104,636,53],[625,3,624,97],[1277,24,30,78],[608,27,48,86],[1274,75,38,38]],[[1065,161,0,0],[179,91,11,32],[262,148,12,30],[361,201,13,40],[465,276,15,53],[569,329,13,52],[656,391,12,58],[793,410,15,65],[930,447,11,72],[1029,-4,80,532]],[[96,79,26,60],[101,234,22,64],[98,367,28,67],[95,504,24,56],[93,630,22,53],[174,734,19,59],[308,830,14,52],[468,834,8,65],[610,836,12,72],[733,867,10,10],[849,846,12,11],[1316,-15,81,519],[1320,584,74,432],[1139,408,12,19],[866,148,10,13],[1004,251,9,11],[1013,251,0,0],[1358,375,0,0],[453,824,22,79]]]

var moveX = 5;
var moveY = 10;

var level_index = 0;
var goal_x = width - 100;

var timer = [0];
var highscore = [411,481,2029,1071,4199,4173,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1];

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
	(b[0] <= a[0]      && a[0]      <= b[0] + b[2]) ||
	(b[0] <= a[0] && a[0] + a[2] <= b[0] + b[2]) ||
	(a[0] <= b[0] && b[0] + b[2] <= a[0] + a[2])) {
	if ((b[1] <= a[1]+a[3] && a[1]+a[3] <= b[1] + b[3]) ||
	    (b[1] <= a[1]      && a[1]      <= b[1] + b[3]) ||
	    (b[1] <= a[1] && a[1] + a[3] <= b[1] + b[3]) ||
	    (a[1] <= b[1] && b[1] + b[3] <= a[1] + a[3])) {
	    return true;
	}
    }
    return false;
}
    

function update () {
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
	
	level_index += 1;

	if (timer.length <= level_index) {
	    timer.push(0);
	}

	timer[level_index] = 0;

	if (level_index >= level.length) {
	    level_editor = true;
	    level.push([]);
	    highscore.push(-1);
	    timer.push(0);
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
