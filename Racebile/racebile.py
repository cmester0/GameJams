from math import cos, sin, pi, floor, ceil
import colorsys

import random
from PIL import Image

import heapq

# Setup
width = 1000
height = 1000

# Helper functions

def raw_hax_coord(x, y):
    xs, ys = 3 * cos(pi/3), sin(pi/3)
    xi, yi = (xs * x, ys * (x + y * 2))
    return xi, yi

def hex_coord(x,y,cx,cy,scale):
    xi,yi = raw_hax_coord(x,y)
    xi = width // 2 + (xi-cx) * scale
    yi = height // 2 + (yi-cy) * scale
    return xi, yi

def draw_hex(m, xi, yi, scale, color):
    for k in range(round(-scale*cos(pi/3)), round(scale*cos(pi/3))+1):
        m[round(xi+k)][round(yi+scale*sin(pi/3))] = color
        m[round(xi+k)][round(yi-scale*sin(pi/3))] = color

    for k in range(0, scale + 1):
        m[round(xi-scale*cos(pi/3)-k*cos(pi/3))][round(yi-scale*sin(pi/3)+k*sin(pi/3))] = color
        m[round(xi-scale*cos(pi/3)-k*cos(pi/3))][round(yi+scale*sin(pi/3)-k*sin(pi/3))] = color

        m[round(xi+scale*cos(pi/3)+k*cos(pi/3))][round(yi-scale*sin(pi/3)+k*sin(pi/3))] = color
        m[round(xi+scale*cos(pi/3)+k*cos(pi/3))][round(yi+scale*sin(pi/3)-k*sin(pi/3))] = color

def draw_filled_hex(m, xi, yi, scale, color):
    for k in range(1, scale + 1):
        for i in range(round(xi-scale*cos(pi/3)-k*cos(pi/3))+1, round(xi+scale*cos(pi/3)+k*cos(pi/3))-1 +1):
            m[i][round(yi-scale*sin(pi/3)+k*sin(pi/3))] = color
            m[i][round(yi+scale*sin(pi/3)-k*sin(pi/3))] = color

def draw_hex_dir(m, xi, yi, d, scale, color):
    for k in range(int(scale*3/4)):
        rx,ry = cos(d*pi/3+pi/6)*k, sin(d*pi/3+pi/6)*k
        m[int(xi+rx)][int(yi+ry)] = color

def step_dir(x,y,d):
    if d == 0:
        return (x+1, y+0)
    elif d == 1:
        return (x+0, y+1)
    elif d == 2:
        return (x-1, y+1)
    elif d == 3:
        return (x-1, y+0)
    elif d == 4:
        return (x+0, y-1)
    elif d == 5:
        return (x+1, y-1)

def weigh_direction_towards_start(x,y,d):
    (nx, ny) = step_dir(0,0,(d-1)%6)
    w1 = ((x - nx) ** 2 + (y - ny) ** 2)
    (nx, ny) = step_dir(0,0,(d+0)%6)
    w2 = ((x - nx) ** 2 + (y - ny) ** 2)
    (nx, ny) = step_dir(0,0,(d+1)%6)
    w3 = ((x - nx) ** 2 + (y - ny) ** 2)
    r = random.randint(0,w1+w2+w3-1)
    if 0 <= r <= w1:
        return -1
    elif w1 <= r <= w1+w2:
        return 0
    else:
        return 1

def gen_map_from_dirs(dirs):
    x = 0
    y = 0
    gm = []
    for d in dirs:
        gm.append((x,y,d))
        x,y = step_dir(x,y,d)
    return gm

def bfs(x0,y0,d0,x1,y1,d1,exclude=set()):
    stk = [(0,x0,y0,d0,[])]
    visited = set()
    while len(stk) > 0:
        s,x,y,d,r = heapq.heappop(stk)

        if len(stk) > 0 and (x,y) in exclude or (x,y) in r:
            continue

        if not (-100 <= x <= 100 or -100 <= y <= 100):
            continue

        if not s < 40:
            continue

        if (x,y) == (x1,y1) and (d - d1)%6 == 0:
            print ("success")
            return r[:-1] + [(x1,y1,d1)]
        if (x,y,d) in visited:
            continue
        visited.add((x,y,d))

        nx, ny = step_dir(x,y,d)
        heapq.heappush(stk,(s+1, nx, ny, (d+1)%6, r + [(nx, ny, (d+1)%6)]))
        heapq.heappush(stk,(s+1, nx, ny, d, r + [(nx, ny, d)]))
        heapq.heappush(stk,(s+1, nx, ny, (d-1)%6, r + [(nx, ny, (d-1)%6)]))

    return []

def starting_platform():
    return [(0,0,1),
            (-1,0,0),
            (0,-1,1),
            (-1,-1,0),
            (0,-2,1),
            (-1,-2,0),
            (0,-3,1),
            (-1,-3,0)]

def gen_map():
    x,y,d = 0,0,1
    l = starting_platform()
    steps = [(x,y,d)]

    count = 0

    total_steps = 4

    while len(steps) <= total_steps:
        if len(steps) == total_steps:
            nx, ny, nd = (0,-4,1)
        else:
            nx = random.randint(-10,10)
            ny = random.randint(-10,10)
            nd = random.randint(0,5)

        count += 1
        if count > 4:
            x,y,d = 0,0,1
            l = starting_platform()
            steps = [(x,y,d)]
            count = 0
            print ("backtrack")
            continue

        nl = bfs(x,y,d,nx,ny,nd,set(map(lambda x: (x[0], x[1]),l)))
        if nl == []:
            continue

        steps += [(nx, ny, nd)]
        l += nl
        x,y,d = nx, ny, nd
        count = 0

    return l

def draw_map(m, game_map, players, player_steps, cx, cy, scale):
    # Grid
    for i in range(-40,40):
        for j in range(-40,40):
            xi, yi = hex_coord(i, j, cx, cy, scale)

            if not (scale <= xi < width-scale and scale <= yi < width-scale):
                continue

            color = (255, 255, 255)
            draw_hex(m, xi, yi, scale, color)

    # Map
    for iters,(i,j,d) in enumerate(game_map):
        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi < width-scale and scale <= yi < width-scale):
            continue

        # color = (255,255,0)
        color = (0, int(iters / len(game_map) * 100), 100)

        draw_filled_hex(m, xi, yi, scale, color)

        if iters == 0:
            draw_hex_dir(m, xi, yi+3, -0.5, scale, (255,0,0))
            draw_hex_dir(m, xi, yi+3, 2.5, scale, (255,0,0))

        draw_hex_dir(m, xi, yi, d, scale, (255,255,0))

    pl, player_steps = player_steps
    for ps,(i,j,d) in enumerate(player_steps):
        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi < width-scale and scale <= yi < width-scale):
            continue

        h = pl / len(players)
        s = ((ps+1) / (len(player_steps)+1))
        v = ((ps+1) / (len(player_steps)+1))
        r, g, b = colorsys.hsv_to_rgb(h,s,v)

        color = (int(r * 255), int(g * 255), int(b * 255))

        draw_hex(m, xi, yi, scale // 2-pl-1, color)
        draw_hex(m, xi, yi, scale // 2+pl+1, color)
        draw_hex(m, xi, yi, scale // 2-pl, color)
        draw_hex(m, xi, yi, scale // 2+pl, color)
        draw_hex_dir(m, xi, yi, d, scale, color)

    # Players
    for pl, (i,j,d,_) in enumerate(players):
        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi < width-scale and scale <= yi < width-scale):
            continue

        h = pl / len(players)
        s = 1.0
        v = 1.0
        r, g, b = colorsys.hsv_to_rgb(h,s,v)

        color = (int(r * 255), int(g * 255), int(b * 255))

        draw_hex(m, xi, yi, scale // 2-pl-1, color)
        draw_hex(m, xi, yi, scale // 2+pl+1, color)
        draw_hex(m, xi, yi, scale // 2-pl, color)
        draw_hex(m, xi, yi, scale // 2+pl, color)
        draw_hex_dir(m, xi, yi, d, scale, color)


game_map = gen_map()

raw_coords = [raw_hax_coord(xi, yi) for xi, yi, _ in game_map]
xm = list(map(lambda x: x[0], raw_coords))
x_max, x_min = (max(xm), min(xm))

ym = list(map(lambda x: x[1], raw_coords))
y_max, y_min = (max(ym), min(ym))

cx = (x_max + x_min) // 2
cy = (y_max + y_min) // 2

scale = int(min(width / 2 / (x_max+2-cx), height / 2 / (y_max+2-cy)))

players = list(map(lambda x: (*x,0), starting_platform())) # (-1,1,1,0), (-1,0,1,0), (0,-1,1,0)]

def save_map(filename, game_map, players, player_steps, scale):
    m = [[(0, 0, 0) for j in range(height)] for i in range(width)]
    draw_map(m, game_map, players, player_steps, cx, cy, scale)

    flat_m = [m[i][j] for j in reversed(range(height)) for i in range(width)]

    img = Image.new('RGB', (width, height)) # width, height
    img.putdata(flat_m)
    img.save(filename)

    return img

def lookup_in_map(x,y):
    l = list(filter(lambda v: v[0] == x and v[1] == y, game_map))
    if len(l) == 0:
        return None
    else:
        return l[0]

def player_block(x,y):
    l = list(filter(lambda v: v[0] == x and v[1] == y, players))
    return len(l) >= 2

def off_map(x,y):
    l = list(filter(lambda v: v[0] == x and v[1] == y, game_map))
    return len(l) == 0

def step_dir_update_dir(x,y,d):
    nx, ny = step_dir(x,y,d)
    if player_block(nx,ny):
        return x,y, lookup_in_map(x,y)[2], False, True

    nd = lookup_in_map(nx,ny)[2]
    return nx, ny, nd, d == nd, False

def step_dir_n(x,y,d,n, sips):
    player_steps = []
    player_steps.append((x,y,d))

    if off_map(x,y):
        x,y = step_dir(x,y,(d+3)%6)
        d = lookup_in_map(x,y)[2]
        player_steps.append((x,y,d))

    if n <= 9:
        for ni in range(n):
            x,y,d,t,bonk = step_dir_update_dir(x,y,d)

            if bonk:
                sips["bonk"] += n - ni
                break

            if n >= 7:
                sips["turn"] += 1

            player_steps.append((x,y,d))

        return x,y,d, player_steps
    else:
        for ni in range(n):
            nx,ny = step_dir(x,y,d)
            if player_block(nx,ny):
                sips["bonk"] += n - ni
                d = lookup_in_map(x,y)[2]
                break

            x, y = nx, ny
            if off_map(x,y):
                sips["off_map"] += 1
                break

            player_steps.append((x,y,d))

        return x,y,d, player_steps

def step_player(pl,x,y,d,g):
    ng = g + 1 if (g < 2 if pl == 0 else g < 3) else g
    sips = {"turn": 0, "off_map": 0, "gas": 0, "bonk": 0, "gear_box": 0}
    steps = []
    for i in range(ng):
        r = random.randint(1,6)
        while r >= 5:
            r = random.randint(1,6)
            sips["gas"] += 1

        steps.append(r)
    if steps == [1,1,1]: # Destroy gear box
        sips["gear_box"] += 1
        ng = 0
        ret_val = (x,y,d,ng)
        player_steps = []
    else:
        px,py,pd,player_steps = step_dir_n(x, y, d, sum(steps), sips)
        ret_val = (px, py, pd, ng if not sips["off_map"] else 0)
    print (sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0), sips, sum(steps), (pl,x,y))
    return ret_val, player_steps

print ("simulate dice")
sim_dice_res = {i: 0 for i in range(3,12+1)}
for _ in range(100000):
    dice = []
    for _ in range(3):
        r = random.randint(1,6)
        while r >= 5:
            r = random.randint(1,6)
        dice.append(r)
    sim_dice_res[sum(dice)] += 1
print (sim_dice_res)
print ({i: sim_dice_res[i] / sum(sim_dice_res[i] for i in sim_dice_res) for i in sim_dice_res})

frames = []
frames.append(save_map(f'Maps/000_map.png', game_map, players, (0, []), scale))
for i in range(1,30):
    for pl,(x,y,d,g) in enumerate(players):
        players[pl], players_steps = step_player(pl,x,y,d,g)
        filename = f'Maps/{i:03d}_{pl:02d}_a_map.png'
        frames.append(save_map(filename, game_map, players, (pl, players_steps), scale))
        filename = f'Maps/{i:03d}_{pl:02d}_b_map.png'
        frames.append(save_map(filename, game_map, players, (pl, []), scale))
    print("\nframe")

frames[0].save(f'Maps/map.gif', format='GIF', append_images=frames[1:], save_all=True, duration=120, loop=0)
