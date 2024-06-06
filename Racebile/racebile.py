from math import cos, sin, pi, floor, ceil
import colorsys

import random

from PIL import Image
from PIL import GifImagePlugin
GifImagePlugin.LOADING_STRATEGY = GifImagePlugin.LoadingStrategy.RGB_AFTER_DIFFERENT_PALETTE_ONLY

import heapq
import itertools
import numpy as np
from threading import Thread

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

def pre_draw(m, game_map, cx, cy, scale):
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

def draw_map(m, players, player_steps, cx, cy, scale, fell_off_map):
    # pre_draw(m,game_map, cx, cy, scale)
    
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

    # Players
    fell_off_total = max([fell_off_map[(x,y)] for x, y in fell_off_map] + [1])
    for (x,y) in fell_off_map:
        h = 0
        s = 1
        v = fell_off_map[(x,y)]/(fell_off_total)
        r, g, b = colorsys.hsv_to_rgb(h,s,v)
        color = (int(r * 255), int(g * 255), int(b * 255))
        xi, yi = hex_coord(x, y, cx, cy, scale)
        draw_filled_hex(m, xi, yi, scale // 2, color)


def compute_scale_and_center():
    extra_x = 1
    extra_y = 0

    raw_coords = [raw_hax_coord(xi, yi) for xi, yi in game_map]
    xm = list(map(lambda x: x[0], raw_coords))
    x_max, x_min = (max(xm)+extra_x, min(xm)-extra_x)

    ym = list(map(lambda x: x[1], raw_coords))
    y_max, y_min = (max(ym)+extra_y, min(ym))

    cx = (x_max + x_min) // 2
    cy = (y_max + y_min) // 2

    scale = int(min(width / 2 / (x_max+2-cx), height / 2 / (y_max+2-cy)))

    return scale, (cx, cy)

def save_map(m, filename, players, player_steps, scale, fell_off_map):
    draw_map(m, players, player_steps, cx, cy, scale, fell_off_map)

    # flat_m = (itertools.chain.from_iterable(m)) # [m[i][j] for j in reversed(range(height)) for i in range(width)]

    # img = Image.new('RGB', (width, height)) # width, height
    # img.putdata(flat_m)

    # arr = np.array(m,dtype=np.uint8)
    img = Image.fromarray(m, "RGB")
    img = img.rotate(90)

    t = Thread(target=lambda i,f: i.save(f), args=[img, filename])
    t.start()
    
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

def get_map_dirs(x, y, cd, player_state, update):
    cm = lookup_in_map(x,y)
    if cm is None:
        d = None
    elif 5 in cm[1]:
        if update:
            player_state[(x,y)] = (1 + player_state[(x,y)]) % len(cm[0])
        d = cm[0][player_state[(x,y)]]
    else:
        # _, d = max((position_distance[step_dir(x,y,nd)], nd) for nd in cm[0])
        _, d = max((len(game_map) + v if position_distance[(x,y)] > v else v, nd)
                   for nd in cm[0]
                   for nd2 in lookup_in_map(*step_dir(x,y,nd))[0]
                   for nd3 in lookup_in_map(*step_dir(*step_dir(x,y,nd), nd2))[0]
                   for v in [position_distance[step_dir(*step_dir(*step_dir(x,y,nd), nd2), nd3)]]
                   if (cd == -1 or (cd-nd)%6 in [5,0,1]) and (nd-nd2)%6 in [5,0,1] and (nd2-nd3)%6 in [5,0,1]
                   )

        # TODO: Get optimal racing line here!

        # d = cm[0][0]
    return d, player_state

def step_dir_update_dir(x,y,d, player_state,fell_off_map):
    # if d is None:
    #     return x,y,d,False,False,player_state

    nx, ny = step_dir(x,y,d)
    if player_block(nx,ny):
        md, player_state = get_map_dirs(x, y, d, player_state, update=False)
        md = d if md is None else md
        return x,y, md, False, True, player_state

    nd, player_state = get_map_dirs(nx, ny, d, player_state, update=True)
    nd = d if nd is None else nd
    return nx, ny, nd, d == nd, False, player_state

def step_dir_n(x,y,d,n, sips, player_state, fell_off_map):
    player_steps = []
    player_steps.append((x,y,d))

    if fell_off_map:
        x,y = step_dir(x,y,(d+3)%6)
        d, player_state = get_map_dirs(x, y, -1, player_state, False)
        player_steps.append((x,y,d))

    if n <= 9:
        next_off_map = False
        for ni in range(n):
            x,y,d,t,bonk,player_state = step_dir_update_dir(x,y,d,player_state, fell_off_map)

            if bonk:
                sips["bonk"] += n - ni
                break

            if n >= 7:
                sips["turn"] += 1

            if off_map(x,y) or next_off_map:
                sips["off_map"] += 1
                break

            if not d in lookup_in_map(x,y)[0]:
                next_off_map = True

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
            nd, player_state = get_map_dirs(x, y, -1, player_state, True)

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
        ng = g + 1 if g < 3 else g # TODO: Strategy

    sips = {"turn": 0, "off_map": 0, "gas": 0, "bonk": 0, "gear_box": 0, "start_last": 0, "end_first": 0, "halfway_cheer": 0, "goal_cheer": 0, "koblingsfejl": 0, "no_sips": 0}
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
        sips["koblingsfejl"] = len(list(filter(lambda x: x == 1, steps)))
        px,py,pd,player_steps, player_state = step_dir_n(x, y, d, sum(steps), sips, player_state, fell_off_map[pl])
        ng = ng if not sips["off_map"] else 0
        ret_val = (px, py, pd, ng, player_state)
    fell_off_map[pl] = sips["off_map"]
    return ret_val, player_steps, sips, steps

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

        (-5,10): ([3,2],[5]), # Crossover point

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
    start_line = [( 0, 0), ( 0, -1)]
    mid_point = []
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
    return game_map, players, start_line, mid_point

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
        (10, 1): ([2,0],[5,3]),
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
        # ( 0,5, 4, 0, {(10, 1): 0}),
        # ( 1,5, 4, 0, {(10, 1): 0}),
        # ( 0,6, 4, 0, {(10, 1): 0}),
        # ( 1,6, 4, 0, {(10, 1): 0}),
        # ( 0,7, 4, 0, {(10, 1): 0}),
        # ( 1,7, 4, 0, {(10, 1): 0}),
        # ( 0,8, 4, 0, {(10, 1): 0}),
        # ( 1,8, 4, 0, {(10, 1): 0}),
    ]
    return game_map, players, start_line, mid_point

def clover_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        ####################
        # Start
        (0,1): ([2], [1,3]),

        # Up to round
        (-1,2): ([4,2], [5]),

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
        (-2,1): ([5,3], [5]),

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
        (-1,-1): ([0,4], [5]),

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
        (1,-2): ([1,5], [5]),

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
        (2,-1): ([2,0], [5]),

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
        (1,1): ([3,1], [5]),

        # around (2,2)
        (1,2): ([1], [0]),
        (1,3): ([0], [0]),
        (2,3): ([5], [2]),
        (3,2): ([4], [2]),
        (3,1): ([3], [0]),
        (2,1): ([3], [0]),
    }
    start_line = [(0,1)]
    mid_point = []
    players = [
        (0,1, 2, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (-1,1, 3, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (-1,0, 4, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        (0,-1, 5, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        # (1,-1, 0, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
        # (1,0, 1, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}),
    ]
    return game_map, players, start_line, mid_point

def tight_clover_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        ####################
        # around (-2,4)
        ( 0,-1): ([4,0,0], [5]), # ,3
        (-1, 0): ([3,5,5], [5]), # ,3
        (-1, 1): ([2,4,4], [5]), # ,3
        ( 0, 1): ([1,3,3], [5]), # ,3
        ( 1, 0): ([0,2,2], [5]), # ,3
        ( 1,-1): ([5,1,1], [5]), # ,3

        # around (-2,4)
        (-2, 2): ([1,5], [5, 2]),
        (-2, 3): ([0],   [1, 3]),
        (-1, 3): ([5],   [1, 3]),
        ( 0, 2): ([0,4], [5, 2]),

        # around (-2,4)
        (-2, 0): ([2,0], [5, 2]),
        (-3, 1): ([1],   [1, 3]),
        (-3, 2): ([0],   [1, 3]),

        # around (-2,4)
        (-1,-2): ([2],   [1, 3]),
        (-2,-1): ([1],   [1, 3]),
        ( 0,-2): ([3,1], [5, 2]),

        # around (-2,4)
        ( 1,-3): ([2],   [1, 3]),
        ( 2,-2): ([2,4], [5, 2]),
        ( 2,-3): ([3],   [1, 3]),

        # around (-2,4)
        ( 2, 0): ([3,5], [5, 2]),
        ( 3,-1): ([4],   [1, 3]),
        ( 3,-2): ([3],   [1, 3]),

        # around (-2,4)
        ( 1, 2): ([5],   [1, 3]),
        ( 2, 1): ([4],   [1, 3]),
    }
    start_line = [(0,1)]
    mid_point = []
    players = [
        (3,-2, 3, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 0-1,
            ( 0, 1): 0-1,
            ( 1, 0): 1-1,
            ( 1,-1): 2-1,
            (-2, 2): 0-1,
            ( 0, 2): 0-1,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 0-1,
            ( 2, 0): 0-1,
        }),
        (3,-1, 4, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 0-1,
            ( 0, 1): 0-1,
            ( 1, 0): 1-1,
            ( 1,-1): 2-1,
            (-2, 2): 0-1,
            ( 0, 2): 0-1,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 0-1,
            ( 2, 0): 0-1,
        }),
        (2,1, 4, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 0-1,
            ( 0, 1): 1-1,
            ( 1, 0): 1,
            ( 1,-1): 0-1,
            (-2, 2): 0-1,
            ( 0, 2): 0,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 0,
            ( 2, 0): 0-1,
        }),
        (1,2, 5, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 0-1,
            ( 0, 1): 1-1,
            ( 1, 0): 1,
            ( 1,-1): 0-1,
            (-2, 2): 0-1,
            ( 0, 2): 0,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 0,
            ( 2, 0): 0-1,
        }),
        (-1,3, 5, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 1-1,
            ( 0, 1): 2-1,
            ( 1, 0): 0-1,
            ( 1,-1): 0-1,
            (-2, 2): 1-1,
            ( 0, 2): 1-1,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 1-1,
            ( 2, 0): 1-1,
        }),
        (-2,3, 0, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 0-1,
            (-1, 1): 1-1,
            ( 0, 1): 2-1,
            ( 1, 0): 0-1,
            ( 1,-1): 0-1,
            (-2, 2): 1-1,
            ( 0, 2): 1-1,
            (-2, 0): 0-1,
            ( 0,-2): 0-1,
            ( 2,-2): 1-1,
            ( 2, 0): 1-1,
        }),
        (-3,2, 0, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 1-1,
            (-1, 1): 2-1,
            ( 0, 1): 0-1,
            ( 1, 0): 0-1,
            ( 1,-1): 0-1,
            (-2, 2): 1-1,
            ( 0, 2): 0-1,
            (-2, 0): 1-1,
            ( 0,-2): 0-1,
            ( 2,-2): 1-1,
            ( 2, 0): 1-1,
        }),
        (-3,1, 1, 0, {
            ( 0,-1): 0-1,
            (-1, 0): 1-1,
            (-1, 1): 2-1,
            ( 0, 1): 0-1,
            ( 1, 0): 0-1,
            ( 1,-1): 0-1,
            (-2, 2): 1-1,
            ( 0, 2): 0-1,
            (-2, 0): 1-1,
            ( 0,-2): 0-1,
            ( 2,-2): 1-1,
            ( 2, 0): 1-1,
        }),
    ]
    return game_map, players, start_line, mid_point

def pod_racing_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs, death wall
    game_map = {
        # Start
        (29, 0): ([1,2], [1]),
        (28, 0): ([0,1], [1]),
        (29,-1): ([1,2], [1]),
        (28,-1): ([0,1], [1]),
        (29,-2): ([1,2], [1]),
        (28,-2): ([0,1], [1]),
        (29,-3): ([1,2], [1]),
        (28,-3): ([0,1], [1]),
        (29,-4): ([1,2], [1]),

        # Map start
        (29, 1): ([2], [0]),
        (28, 1): ([1,2], [0]),
        (28, 2): ([2,3], [0]),
        (27, 2): ([1,2], [0]),
        (27, 3): ([2,3], [0]),
        (26, 3): ([1,2,3], [0]),
        (26, 4): ([2,3], [0]),
        (25, 4): ([1,2], [0]),
        (25, 5): ([2,3], [0]),
        (24, 5): ([1,2], [0]),
        (24, 6): ([2,3], [0]),
        (23, 6): ([1,3], [0]),
        (23, 7): ([1], [0]),

        # Path up
        (23, 8): ([2], [0]),
        (22, 9): ([3], [0]),
        (21, 9): ([3], [0]),
        (20, 9): ([4], [0]),
        (20, 8): ([3,4], [0]),

        # Path down (and blocade)
        (25, 3): ([3,4], [0]),
        (25, 2): ([4], [0]),
        (25, 1): ([3], [0]),

        (24, 3): ([2,3], [0]),
        (24, 1): ([2], [0]),

        (23, 4): ([2], [0]),
        (23, 3): ([1,4], [0]),
        (23, 2): ([1,3], [0]),

        (22, 6): ([4], [0]), # 0
        (22, 5): ([3], [0]), # 1
        (22, 2): ([2], [0]),

        (21, 5): ([2], [0]),
        (21, 4): ([1], [0]),
        (21, 3): ([1], [0]),

        # (24, 4): ([], [2]), # Should be wall around
        # (24, 2): ([], [2]), # Should be wall around
        # (23, 5): ([], [2]), # Should be wall around
        # (22, 4): ([], [2]), # Should be wall around
        # (22, 3): ([], [2]), # Should be wall around
        # (21, 6): ([], [2]), # Should be wall around
        # (20, 5): ([], [2]), # Should be wall around

        # Open area with holes
        (20, 7): ([2,4], [0]),
        (20, 6): ([1,3], [0]),

        (19, 6): ([2,3], [0]),
        (19, 8): ([2,3], [0]),

        (18, 9): ([2,3], [0]),
        (18, 8): ([2,3], [0]),
        (18, 7): ([2], [0]),
        (18, 6): ([3], [0]),
        (18, 5): ([2], [0]), # ?? respawn?

        (17, 10): ([2,3], [0]),
        (17, 9): ([2], [0]),
        (17, 8): ([3], [0]),
        (17, 6): ([2], [0]),

        (16, 11): ([3], [0]),
        (16, 10): ([2,3], [0]),
        (16, 8): ([3], [0]),
        (16, 7): ([2], [0]),

        (15, 11): ([2], [0]),
        (15, 10): ([3], [0]),
        (15, 8): ([2,3], [0]),

        (14, 12): ([3], [0]),
        (14, 10): ([2,4], [0]),
        (14, 9): ([1,3], [0]),
        (14, 8): ([2], [0]),

        (13, 12): ([4], [0]),
        (13, 11): ([3], [0]),
        (13, 9): ([2], [0]),
        
        (12, 11): ([3], [0]),
        (12, 10): ([2], [0]),

        # Ramp
        (11, 11): ([3], [0]),
        (10, 11): ([3], [0]),
        (9, 11): ([3,4], [0]),
        (8, 11): ([3], [0]),
        (7, 11): ([3], [0]),
        (6, 11): ([3], [0]),
        (5, 11): ([3], [0]),
        (4, 11): ([3], [0]),

        (3, 11): ([3,4], [0]),
        (3, 10): ([3,4], [0]),
        (3, 9): ([4,5], [0]),
        (3, 8): ([0,5], [0]),

        (2, 11): ([3,4], [0]),
        (2, 10): ([4,5], [0]),
        (2, 9): ([0,5], [0]),

        (1, 11): ([4], [0]),
        (1, 10): ([5], [0]),

        (4, 8): ([4,5], [0]),
        (4, 7): ([0], [0]),

        (5, 7): ([5], [0]),
        (6, 6): ([5], [0]),
        (7, 5): ([4,5], [0]),

        # Slow path
        (9, 10): ([4], [0]),
        (9, 9): ([3], [0]),
        (8, 9): ([3], [0]),
        (7, 9): ([4], [0]),
        (7, 8): ([5], [0]),
        (8, 7): ([0], [0]),
        (9, 7): ([5], [0]),
        (10, 6): ([5], [0]),
        (11, 5): ([4], [0]),
        (11, 4): ([3], [0]),

        (10, 4): ([2,4], [0]),
        (9, 5): ([3], [0]),
        (8, 5): ([3,4], [0]),
        (8, 4): ([3,4,5], [0]),
        (9, 3): ([3,4], [0]),
        (10, 3): ([3], [0]),

        (7, 4): ([4,5], [0]),
        (8, 3): ([3,4,5], [0]),
        (9, 2): ([3,4], [0]),

        (7, 3): ([4,5], [0]),
        (8, 2): ([3,4,5], [0]),
        (9, 1): ([3,4], [0]),

        (7, 2): ([4,5], [0]),
        (8, 1): ([3,4,5], [0]),
        (9, 0): ([3,4], [0]),

        (7, 1): ([4,5], [0]),
        (8, 0): ([3,4,5], [0]),
        (9,-1): ([3], [0]),

        (7, 0): ([5], [0]),

        (8,-1): ([4], [0]),

        # Cave
        (8,-2): ([4,5], [0]),
        (8,-3): ([0,5], [0]),

        (9,-3): ([4,5], [0]),
        (9,-4): ([0,5], [0]),

        (10,-4): ([0,4,5], [0]),
        (10,-5): ([0,4,5], [0]),
        (10,-6): ([0,5], [0]),

        (11,-4): ([0,4,5], [0]),
        (11,-5): ([0,1,4], [0]),
        (11,-6): ([4,5], [0]),
        (11,-7): ([0,5], [0]),

        (12,-4): ([5], [0]),
        (12,-5): ([0,5], [0]),
        (12,-7): ([0,5], [0]),
        (12,-8): ([0], [0]),

        (13,-5): ([4,5], [0]),
        (13,-6): ([0,4,5], [0]),
        (13,-7): ([0,1,5], [0]),
        (13,-8): ([0,1], [0]),

        (14,-6): ([4,5], [0]),
        (14,-7): ([0,5], [0]),
        (14,-8): ([0,1], [0]),

        (15,-7): ([0], [0]),
        (15,-8): ([0,1], [0]),

        (16,-6): ([0], [0]),
        (16,-7): ([0,1], [0]),
        (16,-8): ([1], [0]),

        (17,-6): ([0], [0]),
        (17,-7): ([0,1], [0]),

        (18,-3): ([3], [0]),
        (18,-4): ([2], [0]),
        (18,-5): ([0,1], [0]),
        (18,-6): ([0,1], [0]),
        (18,-7): ([1], [0]),

        (19,-4): ([2], [0]),
        (19,-5): ([2,1], [0]),
        (19,-6): ([2,1], [0]),

        # Cave ends shots
        (17,-3): ([2,3], [0]),
        (16,-3): ([2], [0]),
        (16,-2): ([1,2], [0]),
        (15,-2): ([1], [0]),
        
        (16,-1): ([1,2], [0]),
        (15,-1): ([0,1], [0]),

        (16,0): ([0,1], [0]),
        (15,0): ([0,1], [0]),

        (16,1): ([0,5], [0]),
        (15,1): ([0], [0]),

        (17,0): ([0], [0]),
        (17,1): ([0,5], [0]),

        # End race
        (18,0): ([0,1,5], [0]),
        (18,1): ([0,5], [0]),

        (19,-1): ([0,5], [0]),
        (19,0): ([0,4,5], [0]),
        (19,1): ([5], [0]),

        (20,-2): ([0,5], [0]),
        (20,-1): ([0,4,5], [0]),
        (20,0): ([4,5], [0]),

        (21,-3): ([0,5], [0]),
        (21,-2): ([0,4,5], [0]),
        (21,-1): ([4,5], [0]),

        (22,-4): ([0,5], [0]),
        (22,-3): ([0,4,5], [0]),
        (22,-2): ([4,5], [0]),

        (23,-5): ([0,5], [0]),
        (23,-4): ([0,4,5], [0]),
        (23,-3): ([4,5], [0]),

        (24,-6): ([0,5], [0]),
        (24,-5): ([0,4,5], [0]),
        (24,-4): ([4,5], [0]),

        (25,-7): ([0,5], [0]),
        (25,-6): ([0,4,5], [0]),
        (25,-5): ([4,5], [0]),

        (26,-8): ([0,5], [0]),
        (26,-7): ([0,4,5], [0]),
        (26,-6): ([0,4,5], [0]),

        (27,-9): ([0,5], [0]),
        (27,-8): ([0,4,5], [0]),
        (27,-7): ([0,1,4,5], [0]),

        (28,-10): ([0], [0]),
        (28,-9): ([0], [0]),
        (28,-8): ([0,1,2], [0]),

        (29,-10): ([1], [0]),
        (29,-9): ([1,2], [0]),

        (27,-6): ([0,1], [0]),
        (28,-7): ([0,1,2], [0]),
        (29,-8): ([1,2], [0]),

        (27,-5): ([0,1], [0]),
        (28,-6): ([0,1,2], [0]),
        (29,-7): ([1,2], [0]),

        (27,-4): ([0,1], [0]),
        (28,-5): ([0,1,2], [0]),
        (29,-6): ([1,2], [0]),

        (27,-3): ([0,1], [0]),
        (28,-4): ([0,1,2], [0]),
        (29,-5): ([1,2], [0]),

        (27,-2): ([0], [0]),
    }
    start_line = [(29, 0)]
    mid_point = []
    players = [
        (29, 0, 1, 0, {}),
        (28, 0, 1, 0, {}),
        (29,-1, 1, 0, {}),
        # (28,-1, 1, 0, {}),
        # (29,-2, 1, 0, {}),
        # (28,-2, 1, 0, {}),
        # (29,-3, 1, 0, {}),
        # (28,-3, 1, 0, {}),
        # (29,-4, 1, 0, {}),
    ]
    return game_map, players, start_line, mid_point

def bfs_distance(game_map, start_line):
    position_distance = {}
    stk = [(0,d,x,y) for x,y in start_line for d in game_map[(x,y)][0]]
    visited = set()
    while len(stk) > 0:
        dist,d,x,y = heapq.heappop(stk)

        if (d,x,y) in visited and ((x,y) in position_distance and dist >= position_distance[(x,y)]):
            continue
        visited.add((d,x,y))

        # TODO: handle forced directions!
        if not (x,y) in position_distance or dist < position_distance[(x,y)]:
            position_distance[(x,y)] = dist

        x,y = step_dir(x,y,d)
        dirs, t = game_map[(x,y)]
        for d in dirs:
            heapq.heappush(stk,(dist+1,d,x,y))

    # assert (len(position_distance) == len(game_map))
    return position_distance

# game_map, players, start_line, mid_point = rtfm_map()
game_map, players, start_line, mid_point = loop_map()
# game_map, players, start_line, mid_point = clover_map()
# game_map, players, start_line, mid_point = tight_clover_map()
# game_map, players, start_line, mid_point = pod_racing_map()

position_distance = bfs_distance(game_map, start_line)

scale, (cx, cy) = compute_scale_and_center()

fell_off_map = [False for p in players]
out_of_map_counter = {}

frames = []
m = [[(0, 0, 0) for j in range(width)] for i in range(height)]
m = np.array(m,dtype=np.uint8)

pre_draw(m,game_map, cx, cy, scale)
frame = save_map(np.array(m,dtype=np.uint8), f'Maps/000_map.png', players, (0, []), scale, out_of_map_counter)
frames.append(frame)

drinking = [0 for p in players]
moves = [0 for p in players]

i = 0
rounds = 1000
while (i < rounds):
    i += 1

    print(f'\nframe {i:03d}.png')
    for pl,(x,y,d,g,player_state) in enumerate(players):
        players[pl], players_steps, sips, steps = step_player(pl,x,y,d,g,player_state, fell_off_map)
        if fell_off_map[pl]:
            ox,oy = (players[pl][0],players[pl][1])
            if not (ox,oy) in out_of_map_counter:
                out_of_map_counter[(ox,oy)] = 0
            out_of_map_counter[(ox,oy)] += 1
        total_sips = sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0) + 5.5 * sips["off_map"] + sips["bonk"] + 11 * sips["gear_box"] + sips["start_last"] + sips["end_first"] + sips["halfway_cheer"] + sips["goal_cheer"] + sips["koblingsfejl"]
        if total_sips == 0:
            sips["no_sip"] = 1
            total_sips = 1

        drinking[pl] += total_sips
        moves[pl] += 1

        if i % 1 == 0:
            filename = f'Maps/{i:03d}_{pl:02d}_a_map.png'
            frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, players_steps), scale,out_of_map_counter)
            if i < 10: frames.append(frame)
            filename = f'Maps/{i:03d}_{pl:02d}_b_map.png'
            frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale,out_of_map_counter)
            if i < 10: frames.append(frame)
            print (out_of_map_counter)

print (out_of_map_counter)

filename = f'Maps/result_map.png'
frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale, out_of_map_counter)
frames.append(frame)

print ("Average:", sum(drinking) / sum(moves))
print ("Total:", sum(drinking))
print ("Per player", drinking)

frames[0].save(f'Maps/map.gif', append_images=frames[1:], save_all=True, duration=120, loop=0)
# frames[0].save(f'Maps/map.gif', format='GIF', append_images=frames[1:], save_all=True, duration=120, loop=0)
