from math import cos, sin, pi, floor, ceil

import random
from PIL import Image

import heapq

# Setup
width = 3000
height = 3000

# Helper functions

def hex_coord(x,y,scale):
    xs, ys = 3 * cos(pi/3), sin(pi/3)
    xi, yi = (xs * x, ys * (x + y * 2))
    xi = width // 2 + xi * scale
    yi = height // 2 + yi * scale
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

# def extend_dirs_randomly(d):
#     dirs = list(d)
#     index = random.randint(0,len(dirs)-1)

#     if index+2 < len(dirs) and dirs[index] == dirs[index+1] and dirs[index+1] == dirs[index+2]:
#         if random.randint(0,1) == 0:
#             dirs = dirs[:index] + ([(dirs[index]-1)%6, (dirs[index])%6, (dirs[index])%6, (dirs[index]+1)%6]) + dirs[index+3:]
#         else:
#             dirs = dirs[:index] + ([(dirs[index]+1)%6, (dirs[index])%6, (dirs[index])%6, (dirs[index]-1)%6]) + dirs[index+3:]
#     elif (dirs[index]+3)%6 in dirs:
#         dirs = dirs[:index] + [dirs[index]] * 2 + dirs[index+1:]
#         other = list(filter(lambda x: x[1] == (dirs[index]+3)%6, enumerate(dirs)))
#         index = random.randint(0,len(other)-1)
#         index, value = other[index]
#         dirs = dirs[:index] + [value]*2 + dirs[index+1:]

#     return dirs

def gen_map():
    x,y,d = 0,0,1
    l = [(x,y,d)]
    steps = [(x,y,d)]

    count = 0

    total_steps = 4

    while len(steps) <= total_steps:
        if len(steps) == total_steps:
            nx, ny, nd = 0,-1,1
        else:
            nx = random.randint(-10,10)
            ny = random.randint(-10,10)
            nd = random.randint(0,5)

        count += 1
        if count > 4:
            # steps.pop()
            # while l[-1] != steps[-1]:
            #     l.pop()
            # x,y,d = l[-1]
            x,y,d = 0,0,1
            l = [(x,y,d)]
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

def draw_map(m, game_map, players):
    scale = 20

    # Grid
    for i in range(-20,20):
        for j in range(-30,30):
            xi, yi = hex_coord(i, j, scale)

            if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
                continue

            color = (255, 255, 255)
            draw_hex(m, xi, yi, scale, color)

    # Map
    for iters,(i,j,d) in enumerate(game_map):
        xi, yi = hex_coord(i, j, scale)

        if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
            continue

        # color = (255,255,0)
        color = (0, int(iters / len(game_map) * 200), 255)
        if iters == 0:
            color = (255, 0, 0)

        draw_filled_hex(m, xi, yi, scale, color)

        draw_hex_dir(m, xi, yi, d, scale, (255,255,0))

    # Players
    for (i,j,d,_) in players:
        xi, yi = hex_coord(i, j, scale)

        if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
            continue

        # color = (255,255,0)
        color = (0, 255, 0)

        draw_hex(m, xi, yi, scale // 2-1, color)
        draw_hex(m, xi, yi, scale // 2+1, color)
        draw_hex_dir(m, xi, yi, d, scale, color)

game_map = gen_map()
players = [(0,0,1,1)]

def save_map(filename, game_map, players):
    m = [[(0, 0, 0) for j in range(height)] for i in range(width)]

    draw_map(m, game_map, players)

    flat_m = [m[i][j] for j in reversed(range(height)) for i in range(width)]

    img = Image.new('RGB', (width, height)) # width, height
    img.putdata(flat_m)
    img.save(filename)

def lookup_in_map(x,y):
    l = list(filter(lambda v: v[0] == x and v[1] == y, game_map))
    if len(l) == 0:
        return None
    else:
        return l[0]

def step_dir_update_dir(x,y,d):
    nx, ny = step_dir(x,y,d)
    nd = lookup_in_map(nx,ny)[2]
    return nx, ny, nd, d == nd

def step_dir_n(x,y,d,n, sips):
    if lookup_in_map(x,y) is None:
        x,y = step_dir(x,y,(d+3)%6)
        d = lookup_in_map(x,y)[2]

    if n <= 9:
        for _ in "*"*n:
            x,y,d,t = step_dir_update_dir(x,y,d)
            if n >= 7:
                sips["turn"] += 1

        return x,y,d
    else:
        for _ in "*"*n:
            x,y = step_dir(x,y,d)
            if lookup_in_map(x,y) is None:
                sips["off_map"] += 1
                break
        return x,y,d

def step_players(players):
    new_players = []
    for x,y,d,g in players:
        ng = g + 1 if g < 3 else g
        sips = {"turn": 0, "off_map": 0, "gas": 0}
        steps = []
        for i in range(ng):
            r = random.randint(1,6)
            while r >= 5:
                r = random.randint(1,6)
                sips["gas"] += 1

            steps.append(r)

        if sips["off_map"]:
            2

        new_players.append((*step_dir_n(x, y, d, sum(steps), sips), ng if sips["off_map"] else 0))
        print ("sips", sips, sum(steps))
    players[:] = new_players

for i in range(30):
    filename = f'Maps/{i:03d}_map.png'
    save_map(filename, game_map, players)
    step_players(players)
