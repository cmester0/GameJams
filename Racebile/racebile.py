from math import cos, sin, pi, floor, ceil
import colorsys

import random

from PIL import Image
from PIL import GifImagePlugin
GifImagePlugin.LOADING_STRATEGY = GifImagePlugin.LoadingStrategy.RGB_AFTER_DIFFERENT_PALETTE_ONLY

import heapq

# Setup
width = 1500 # 1500
height = 1500

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

        for i in range(2):
            m[int(xi+rx+i)][int(yi+ry)] = color
            m[int(xi+rx-i)][int(yi+ry)] = color

            m[int(xi+rx)][int(yi+ry+i)] = color
            m[int(xi+rx)][int(yi+ry-i)] = color

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

def draw_circle(m, xi, yi, radius, color):
    for i in range(360):
        m[round(xi + radius * cos(i / 360 * 2 * pi))][round(yi + radius * sin(i / 360 * 2 * pi))] = color


def draw_map(m, game_map, players, player_steps, cx, cy, scale):
    raw_coords = [(xi, yi) for xi, yi in game_map]
    xm = list(map(lambda x: x[0], raw_coords))
    x_max, x_min = (max(xm), min(xm))

    ym = list(map(lambda x: x[1], raw_coords))
    y_max, y_min = (max(ym), min(ym))

    # Grid
    for i in range(x_min-1, x_max+1+1):
        for j in range(y_min-1, y_max+1+1):
            xi, yi = hex_coord(i, j, cx, cy, scale)

            if not (scale <= xi < width-scale and scale <= yi < width-scale):
                continue

            color = (255, 255, 255)
            draw_hex(m, xi, yi, scale, color)

    # Map
    for (i,j) in game_map:
        dirs,t = game_map[(i,j)]

        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi <= width-scale and scale <= yi <= height-scale):
            continue

        h = 210.6/360 if 2 in t else 51.1/360
        s = 1.0
        v = 0.5 if 1 in t else 0.75
        r, g, b = colorsys.hsv_to_rgb(h,s,v)

        color = (int(r * 255), int(g * 255), int(b * 255))

        draw_filled_hex(m, xi, yi, scale, color)

        if 3 in t:
            h = 1.9/360
            s = 1.0
            v = 0.75
            r, g, b = colorsys.hsv_to_rgb(h,s,v)
            color = (int(r * 255), int(g * 255), int(b * 255))
            draw_filled_hex(m, xi, yi, scale // 2, color)

        for d in dirs:
            if 4 in t or 5 in t:
                draw_hex_dir(m, xi, yi, d, scale, (0,0,0))
            else:
                draw_hex_dir(m, xi, yi, d, scale, (255,255,255))

    pl, player_steps = player_steps
    for ps,(i,j,d) in enumerate(player_steps):
        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi <= width-scale and scale <= yi <= width-scale):
            continue

        h = 5 * pl / 8 # len(players)
        s = ((ps+1) / (len(player_steps)+1))
        v = ((ps+1) / (len(player_steps)+1))
        r, g, b = colorsys.hsv_to_rgb(h,s,v)

        color = (int(r * 255), int(g * 255), int(b * 255))

        draw_circle(m, xi, yi, scale // 3 + 2 * pl, color)
        draw_circle(m, xi, yi, scale // 3 - 2 * pl, color)

        draw_circle(m, xi, yi, scale // 3 + 2 * pl + 1, color)
        draw_circle(m, xi, yi, scale // 3 - 2 * pl - 1, color)

        draw_hex_dir(m, xi, yi, d, scale, color)

    # Players
    for pl, (i,j,d,_,_) in enumerate(players):
        xi, yi = hex_coord(i, j, cx, cy, scale)

        if not (scale <= xi <= width-scale and scale <= yi <= width-scale):
            continue

        h = 5 * pl / 8 # len(players)
        s = 1.0
        v = 1.0
        r, g, b = colorsys.hsv_to_rgb(h,s,v)

        color = (int(r * 255), int(g * 255), int(b * 255))

        draw_circle(m, xi, yi, scale // 3 + 2 * pl, color)
        draw_circle(m, xi, yi, scale // 3 - 2 * pl, color)

        draw_circle(m, xi, yi, scale // 3 + 2 * pl + 1, color)
        draw_circle(m, xi, yi, scale // 3 - 2 * pl - 1, color)

        draw_hex_dir(m, xi, yi, d, scale, color)


def compute_scale_and_center():
    extra_x = 1
    extra_y = 0

    raw_coords = [raw_hax_coord(xi, yi) for xi, yi in game_map]
    xm = list(map(lambda x: x[0], raw_coords))
    x_max, x_min = (max(xm)+extra_x, min(xm))

    ym = list(map(lambda x: x[1], raw_coords))
    y_max, y_min = (max(ym)+extra_y, min(ym))

    cx = (x_max + x_min) // 2
    cy = (y_max + y_min) // 2

    scale = int(min(width / 2 / (x_max+2-cx), height / 2 / (y_max+2-cy)))

    return scale, (cx, cy)

def save_map(filename, game_map, players, player_steps, scale):
    m = [[(0, 0, 0) for j in range(height)] for i in range(width)]
    draw_map(m, game_map, players, player_steps, cx, cy, scale)

    flat_m = [m[i][j] for j in reversed(range(height)) for i in range(width)]

    img = Image.new('RGB', (width, height)) # width, height
    img.putdata(flat_m)
    img.save(filename)

    return img

def lookup_in_map(x,y):
    if not (x,y) in game_map:
        return None
    else:
        return game_map[(x,y)]

def player_block(x,y):
    l = list(filter(lambda v: v[0] == x and v[1] == y, players))
    cm = lookup_in_map(x,y)
    return len(l) >= 2 or (not cm is None and 3 in cm[1] and len(l) >= 1)

def off_map(x,y):
    return not (x,y) in game_map

def get_map_dirs(x, y, player_state, update=True):
    cm = lookup_in_map(x,y)
    if cm is None:
        d = None
    elif 5 in cm[1]:
        d = cm[0][player_state[(x,y)]]
        if update:
            player_state[(x,y)] = 1 - player_state[(x,y)]
    else:
        _, d = max((position_distance[step_dir(x,y,nd)], nd) for nd in cm[0])
    return d, player_state

def step_dir_update_dir(x,y,d, player_state):
    nx, ny = step_dir(x,y,d)
    if player_block(nx,ny):
        md, player_state = get_map_dirs(x, y, player_state)
        return x,y, md, False, True, player_state

    nd, player_state = get_map_dirs(nx, ny, player_state)
    return nx, ny, nd, d == nd, False, player_state

def step_dir_n(x,y,d,n, sips, player_state, fell_off_map):
    player_steps = []
    player_steps.append((x,y,d))

    if fell_off_map:
        x,y = step_dir(x,y,(d+3)%6)
        d, player_state = get_map_dirs(x, y, player_state, False)
        player_steps.append((x,y,d))

    if n <= 9:
        for ni in range(n):
            x,y,d,t,bonk,player_state = step_dir_update_dir(x,y,d,player_state)

            if bonk:
                sips["bonk"] += n - ni
                break

            if n >= 7:
                sips["turn"] += 1

            player_steps.append((x,y,d))

        return x,y,d, player_steps, player_state
    else:
        next_off_map = False
        for ni in range(n):
            nx,ny = step_dir(x,y,d)

            if player_block(nx,ny):
                sips["bonk"] += n - ni
                break

            x, y = nx, ny
            nd, player_state = get_map_dirs(x, y, player_state)

            if off_map(x,y) or next_off_map:
                sips["off_map"] += 1
                break

            if not d in lookup_in_map(x,y)[0]:
                next_off_map = True

            player_steps.append((x,y,d))

        return x,y,d, player_steps, player_state

def step_player(pl,x,y,d,g,player_state,fell_off_map):
    if not lookup_in_map(x,y) is None and 2 in lookup_in_map(x,y)[1]:
        ng = max(g-1,1)
    else:
        ng = g + 1 if (g < 2 if pl == 0 else g < 3) else g

    sips = {"turn": 0, "off_map": 0, "gas": 0, "bonk": 0, "gear_box": 0, "start_last": 0, "end_first": 0, "halfway_cheer": 0, "goal_cheer": 0}
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
        ret_val = (x,y,d,ng,player_state)
        player_steps = []
    else:
        px,py,pd,player_steps, player_state = step_dir_n(x, y, d, sum(steps), sips, player_state, fell_off_map[pl])
        ng = ng if not sips["off_map"] else 0
        ret_val = (px, py, pd, ng, player_state)
    print (sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0), sips, sum(steps), (pl,x,y))
    fell_off_map[pl] = sips["off_map"]
    return ret_val, player_steps

def rtfm_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        # Start Setup
        ( 0, 0): ([4,5],[1]),
        (-1, 0): ([0,5],[1]),
        (-1, 1): ([4,5],[1]),
        (-2, 1): ([0,5],[1]),
        (-2, 2): ([4,5],[1]),
        (-3, 2): ([0,5],[1]),
        (-3, 3): ([4,5],[1]),
        (-4, 3): ([0,5],[1]),

        # Around map
        ( 0,-1): ([0],  [0]),
        ( 1,-1): ([5],  [0]),
        ( 2,-2): ([0],  [2]),
        ( 3,-2): ([0],  [0]),
        ( 4,-2): ([0],  [0]),
        ( 5,-2): ([1],  [0]),
        ( 5,-1): ([1],  [0]),
        ( 5, 0): ([0,2],[4]),

        # split branch left
        ( 6, 0): ([1],  [0]),
        ( 6, 1): ([2],  [0]),
        # split branch right
        ( 4, 1): ([1],  [0]),
        ( 4, 2): ([0],  [0]),

        # Back to merge
        ( 5, 2): ([1],  [3]),
        ( 5, 3): ([1],  [0]),
        ( 5, 4): ([1],  [0]),
        ( 5, 5): ([2],  [0]),
        ( 4, 6): ([3],  [3]),
        ( 3, 6): ([2,3],[2]),
        ( 2, 6): ([2],  [2]),
        ( 2, 7): ([3],  [2]),
        ( 1, 7): ([3],  [0]),
        ( 0, 7): ([3,4],[0]),
        ( 0, 6): ([2,3],[2]),
        (-1, 7): ([3,4],[0]),
        (-1, 6): ([2],  [2]),

        # Midpoint
        (-2, 7): ([2],  [3]),
        (-3, 8): ([2],  [0]),
        (-4, 9): ([2],  [4]),

        (-5,10): ([2,3],[5]), # Crossover point

        (-6,11): ([2],  [3]),
        (-7,12): ([1],  [0]),
        (-7,13): ([0],  [2]),
        (-6,13): ([5],  [2]),
        (-5,12): ([4],  [2]),
        (-5,11): ([4],  [4]),
        # ( .. )          # Crossover point
        (-6,10): ([4],  [3]),
        (-6, 9): ([3],  [0]),
        (-7, 9): ([4],  [0]),
        (-7, 8): ([3,4],[0]),
        (-8, 8): ([4,5],[2]),
        (-7, 7): ([3,4],[0]),
        (-8, 7): ([5],  [2]),
        (-7, 6): ([5],  [3]),
        (-6, 5): ([5],  [0]),
        (-5, 4): ([5],  [0]),
    }
    start_line = []
    players = [
        # Start Setup
        ( 0, 0, 5, 0, {(-5,10): 0}),
        (-1, 0, 5, 0, {(-5,10): 0}),
        (-1, 1, 5, 0, {(-5,10): 0}),
        (-2, 1, 5, 0, {(-5,10): 0}),
        (-2, 2, 5, 0, {(-5,10): 0}),
        (-3, 2, 5, 0, {(-5,10): 0}),
        (-3, 3, 5, 0, {(-5,10): 0}),
        (-4, 3, 5, 0, {(-5,10): 0}),
    ]
    return game_map, players

def loop_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        # Start Setup
        ( 1,8): ([3,4],[1]),
        ( 0,8): ([4,5],[1]),
        ( 1,7): ([3,4],[1]),
        ( 0,7): ([4,5],[1]),
        ( 1,6): ([3,4],[1]),
        ( 0,6): ([4,5],[1]),
        ( 1,5): ([3,4],[1]),
        ( 0,5): ([4,5],[1]),
        ( 1,4): ([3,4],[1]),
        ( 0,4): ([4,5],[1]),
        ( 1,3): ([3,4],[1]),
        ( 0,3): ([4,5],[1]),

        # Map
        (1, 2): ([4,5],[0]),
        (0, 2): ([0,5],[0]),
        (2, 1): ([0,5],[0]),
        (1, 1): ([0,5],[0]),
        (2, 0): ([0],[0]),
        (3, 0): ([1],[0]),
        (3, 1): ([0],[0]),
        (4, 1): ([0,5],[0]),
        (5, 1): ([5],[2]),
        (5, 0): ([0],[0]),
        (6, 0): ([0],[3]),
        (7, 0): ([0],[0]),
        (8, 0): ([0,1],[0]),
        (9, 0): ([1],[0]),
        (8, 1): ([0],[3]),
        (9, 1): ([0],[0]),
        (10, 1): ([0,2],[5,3]),
        (11, 1): ([0],[0]),
        (12, 1): ([1],[0]),
        (12, 2): ([1,0],[4]),

        # left way
        (12, 3): ([1],[0]),
        (12, 4): ([0],[0]),
        (13, 4): ([5],[2]),
        (14, 3): ([0],[3]),

        # right way
        (13, 2): ([5],[0]),
        (14, 1): ([0],[3]),
        (15, 1): ([1],[2]),
        (15, 2): ([1],[0]),

        # Combine
        (15, 3): ([0],[0]),
        (16, 3): ([5],[0]),
        (17, 2): ([4],[0]),
        (17, 1): ([4,5],[0]),
        (18, 0): ([3,4],[0]),
        (17, 0): ([4,5],[0]),
        (18,-1): ([3,4],[0]),
        (17,-1): ([4,5],[3]),
        (18,-2): ([3,4],[2]),
        (17,-2): ([4,5],[0]),
        (18,-3): ([3,4],[0]),
        (17,-3): ([5],[0]),
        (18,-4): ([4],[0]),
        (18,-5): ([4],[0]),
        (18,-6): ([3,4],[0]),
        (18,-7): ([2,3],[0]),
        (17,-6): ([2,3],[2]),
        (17,-7): ([2],[0]),
        (16,-6): ([1,2],[0]),
        (16,-5): ([2,3],[0]),
        (15,-5): ([1],[0]),
        (15,-4): ([2],[0]),
        (14,-3): ([2],[3]),
        (13,-2): ([2],[0]),
        (12,-1): ([2],[0]),
        (11, 0): ([2],[0]),
        # (..)             # Crossover Point
        ( 9, 2): ([2],[0]),
        ( 8, 3): ([2],[0]),
        ( 7, 4): ([2],[0]),
        ( 6, 5): ([2],[3]),
        ( 5, 6): ([2],[0]),
        ( 4, 7): ([2],[0]),
        ( 3, 8): ([2,3],[0]),
        ( 2, 9): ([2,3],[2,3]),
        ( 2, 8): ([3],[3]),
        ( 1,10): ([3],[2]),
        ( 1, 9): ([3,4],[0]),
        ( 0,10): ([4],[2]),
        ( 0,9): ([4,5],[0]),

    }
    start_line = [(0,3), (1,3)]
    mid_point = [(17, 0), (18,-1)]
    players = [
        ( 0,3, 4, 0, {(10, 1): 0}),
        ( 1,3, 4, 0, {(10, 1): 0}),
        ( 0,4, 4, 0, {(10, 1): 0}),
        ( 1,4, 4, 0, {(10, 1): 0}),
        ( 0,5, 4, 0, {(10, 1): 0}),
        ( 1,5, 4, 0, {(10, 1): 0}),
        ( 0,6, 4, 0, {(10, 1): 0}),
        ( 1,6, 4, 0, {(10, 1): 0}),
        ( 0,7, 4, 0, {(10, 1): 0}),
        ( 1,7, 4, 0, {(10, 1): 0}),
        ( 0,8, 4, 0, {(10, 1): 0}),
        ( 1,8, 4, 0, {(10, 1): 0}),
    ]
    return game_map, players, start_line, mid_point

def clover_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        ####################
        # Start
        (0,1): ([2], [1,3]),

        # Up to round
        (-1,2): ([2,4], [5]),

        # around (-2,4)
        (-2,3): ([2], [0]),
        (-3,4): ([1], [0]),
        (-3,5): ([0], [2]),
        (-2,5): ([5], [2]),
        (-1,4): ([4], [0]),
        (-1,3): ([4], [0]),

        ####################
        # Continue
        (-1,1): ([3], [1,3]),

        # Next round-about
        (-2,1): ([3,5], [5]),

        # around (-4, 2)
        (-3,1): ([3], [0]),
        (-4,1): ([2], [0]),
        (-5,2): ([1], [2]),
        (-5,3): ([0], [2]),
        (-4,3): ([5], [0]),
        (-3,2): ([5], [0]),
        ####################
        # Continue
        (-1,0): ([4], [1,3]),

        # Next round-about
        (-1,-1): ([4,0], [5]),

        # around (-2,-2)
        (-1,-2): ([4], [0]),
        (-1,-3): ([3], [0]),
        (-2,-3): ([2], [2]),
        (-3,-2): ([1], [2]),
        (-3,-1): ([0], [0]),
        (-2,-1): ([0], [0]),

        ####################
        # Continue
        (0,-1): ([5], [1,3]),

        # Next round-about
        (1,-2): ([5,1], [5]),

        # around (2,-4)
        (2,-3): ([5], [0]),
        (3,-4): ([4], [0]),
        (3,-5): ([3], [2]),
        (2,-5): ([2], [2]),
        (1,-4): ([1], [0]),
        (1,-3): ([1], [0]),

        ####################
        # Continue
        (1,-1): ([0], [1,3]),

        # Next round-about
        (2,-1): ([0,2], [5]),

        # around (4,-2)
        (3,-1): ([0], [0]),
        (4,-1): ([5], [0]),
        (5,-2): ([4], [2]),
        (5,-3): ([3], [2]),
        (4,-3): ([2], [0]),
        (3,-2): ([2], [0]),

        ####################
        # Continue
        (1,0): ([1], [1,3]),

        # Next round-about
        (1,1): ([1,3], [5]),

        # around (2,2)
        (1,2): ([1], [0]),
        (1,3): ([0], [0]),
        (2,3): ([5], [2]),
        (3,2): ([4], [2]),
        (3,1): ([3], [0]),
        (2,1): ([3], [0]),
    }
    start_line = []
    players = [
        (0,1, 2, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (-1,1, 3, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (-1,0, 4, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (0,-1, 5, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (1,-1, 0, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (1,0, 1, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
    ]
    return game_map, players

def bfs_distance(game_map, start_line):
    position_distance = {}
    stk = [(0,d,x,y) for x,y in start_line for d in game_map[(x,y)][0]]
    visited = set()
    while len(stk) > 0:
        dist,d,x,y = stk.pop()

        # if not (x,y) in game_map:
        #     print ("NEVER")
        #     continue

        if (d,x,y) in visited:
            continue
        visited.add((d,x,y))

        # TODO: handle forced directions!
        if not (x,y) in position_distance or dist < position_distance[(x,y)]:
            position_distance[(x,y)] = dist

        x,y = step_dir(x,y,d)
        dirs, t = game_map[(x,y)]
        for d in dirs:
            stk.append((dist+1,d,x,y))

    assert (len(position_distance) == len(game_map))
    return position_distance

# game_map, players = rtfm_map()
game_map, players, start_line, mid_point = loop_map()
# game_map, players = clover_map()

position_distance = bfs_distance(game_map, start_line)

scale, (cx, cy) = compute_scale_and_center()


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

fell_off_map = [False for p in players]

i = 0
while (i < 15):
    i += 1

    print(f'\nframe {i:03d}.png')
    for pl,(x,y,d,g,player_state) in enumerate(players):
        players[pl], players_steps = step_player(pl,x,y,d,g,player_state, fell_off_map)
        filename = f'Maps/{i:03d}_{pl:02d}_a_map.png'
        frames.append(save_map(filename, game_map, players, (pl, players_steps), scale))
        filename = f'Maps/{i:03d}_{pl:02d}_b_map.png'
        frames.append(save_map(filename, game_map, players, (pl, []), scale))

frames[0].save(f'Maps/map.gif', append_images=frames[1:], save_all=True, duration=120, loop=0)
# frames[0].save(f'Maps/map.gif', format='GIF', append_images=frames[1:], save_all=True, duration=120, loop=0)
