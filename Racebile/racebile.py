from math import cos, sin, pi

import random
from PIL import Image

import heapq

# Setup
width = 2000
height = 2000

m = [[(0, 0, 0) for j in range(height)] for i in range(width)]

# Helper functions

def hex_coord(x,y):
    return (x * cos(pi/6), x * sin(pi/6) + y)

def draw_hex(m, xi, yi, scale, color):
    for k in range(-scale, scale+1):
        m[int(xi)+k][int(yi)+2*scale] = color
        m[int(xi)+k][int(yi)-2*scale] = color

    for k in range(-scale, scale+1+3):
        m[round(xi-scale-(k+scale)*cos(pi/3))][round(yi-2*scale+(k+scale)*sin(pi/3))] = color
        m[round(xi-scale-(k+scale)*cos(pi/3))][round(yi+2*scale-(k+scale)*sin(pi/3))] = color

        m[round(xi+scale+(k+scale)*cos(pi/3))][round(yi-2*scale+(k+scale)*sin(pi/3))] = color
        m[round(xi+scale+(k+scale)*cos(pi/3))][round(yi+2*scale-(k+scale)*sin(pi/3))] = color

def draw_filled_hex(m, xi, yi, scale, color):
    for k in range(-scale, scale+3):
        for i in range(round(xi-scale-(k+scale)*cos(pi/3)), round(xi+scale+(k+scale)*cos(pi/3))):
            m[i][round(yi-2*scale+(k+scale)*sin(pi/3)+1)] = color
            m[i][round(yi+2*scale-(k+scale)*sin(pi/3)-1)] = color

def draw_hex_dir(m, xi, yi, d, scale, color):
    for k in range(int(scale * 1.5)):
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
    l = []

    success = 0
    while success < 3:
        nx = random.randint(-10,10)
        ny = random.randint(-10,10)
        nd = random.randint(0,5)

        nl = bfs(x,y,d,nx,ny,nd,set(map(lambda x: (x[0], x[1]),l)))
        if nl == []:
            continue

        l += nl
        x,y,d = nx, ny, nd

        success += 1

    l += bfs(x,y,d,0,0,1,set(map(lambda x: (x[0], x[1]),l)))

    return l


game_map = gen_map()

# Game

for i in range(-10,10):
    for j in range(-10,10):
        scale = 10
        xi, yi = hex_coord(i, j)
        xi = width // 2 + xi * (scale * 4)
        yi = height // 2 + yi * (scale * 4)

        if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
            continue

        color = (255, 255, 255)
        draw_hex(m, xi, yi, scale, color)

for iters,(i,j,d) in enumerate(game_map):
    scale = 10
    xi, yi = hex_coord(i, j)
    xi = width // 2 + xi * (scale * 4)
    yi = height // 2 + yi * (scale * 4)

    if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
        continue

    # color = (255,255,0)
    color = (0, int(iters / len(game_map) * 200), 255)

    draw_filled_hex(m, xi, yi, scale, color)

    draw_hex_dir(m, xi, yi, d, scale, (255,255,0))

# Print map

flat_m = [m[i][j] for j in reversed(range(height)) for i in range(width)]

img = Image.new('RGB', (width, height)) # width, height
img.putdata(flat_m)
img.save('map.png')
