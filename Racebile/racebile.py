from math import cos, sin, pi, floor, ceil, inf
import colorsys

import random

from PIL import Image
from PIL import GifImagePlugin
GifImagePlugin.LOADING_STRATEGY = GifImagePlugin.LoadingStrategy.RGB_AFTER_DIFFERENT_PALETTE_ONLY

import heapq
import itertools
import numpy as np
from threading import Thread

from scipy.stats import poisson
from scipy.stats import geom
from scipy.stats import binom

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
    for pl, ((i,j),d,_,_,_) in enumerate(players):
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
        if cd == -1:
            pos1 = [(position_distance_goal[(x,y,d)], 0 if d == cd else 1, d) for d in range(6) if (x,y,d) in position_distance_goal]
            pos2 = [(position_distance_midpoint[(x,y,d)], 0 if d == cd else 1, d) for d in range(6) if (x,y,d) in position_distance_midpoint]
        else:
            pos1 = [(position_distance_goal[(x,y,d)], 0 if d == cd else 1, d) for d in range(6) if (x,y,d) in position_distance_goal]
            pos2 = [(position_distance_midpoint[(x,y,d)], 0 if d == cd else 1, d) for d in range(6) if (x,y,d) in position_distance_midpoint]
        # d = min(pos, key=lambda x:x[:2])[2]
        d = min(pos2)[2] if min(pos1)[2] < 12 else min(pos1)[2]

        # print (min())
        # _, d = min((position_distance[(*step_dir(x,y,nd),(nd+i)%6)], nd) for nd in cm[0] for i in [-1,0,1] if (*step_dir(x,y,nd),(nd+i)%6) in position_distance)
        # _, d = min((len(game_map) + v if position_distance[(x,y,cd)] > v else v, nd)
        #            for nd in cm[0]
        #            )

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

def get_position(x,y,d):
    if (x,y,d) in position_distance_goal:
        return position_distance_goal[(x,y,d)]
    else:
        ox, oy = step_dir(x,y,(d+3)%6)
        return min((position_distance_goal[(ox,oy,d)],d) for d in range(6) if (ox,oy,d) in position_distance_goal)[1]

blocked = set()

def step_player(pl,players,fell_off_map):
    (x,y),d,g,player_state,rounds = players[pl]

    if not lookup_in_map(x,y) is None and 2 in lookup_in_map(x,y)[1]:
        ng = max(g-1,1)
    else:
        ng = g + 1 if g < 3 else g # TODO: Strategy

    sips = {"turn": 0, "off_map": 0, "gas": 0, "bonk": 0, "gear_box": 0, "start_last": 0, "end_first": 0, "halfway_cheer": 0, "goal_cheer": 0, "koblingsfejl": 0, "no_sips": 0}

    steps = [random.randint(1,4) for i in range(ng)]
    sips["gas"] = sum(geom.rvs(2/3,size=ng))-ng

    player_steps = []

    if fell_off_map[pl]:
        player_steps.append((x,y,d))
        x,y = step_dir(x,y,(d+3)%6)
        d, player_state = get_map_dirs(x, y, -1, player_state, False)
        player_steps.append((x,y,d))

    sips["start_last"] = int(min((((r,position_distance_goal[(x,y,d)] if (x,y,d) in position_distance_goal else inf),pl) for pl,((x,y),d,_,_,r) in enumerate(players)), key=lambda x: x[0])[1] == pl)

    if steps == [1,1,1]: # Destroy gear box
        sips["gear_box"] += 1
        ng = 0
        ret_val = ((x,y),d,ng,player_state,rounds)
    else:

        if (x,y) in blocked:
            blocked.remove((x,y))

        sips["koblingsfejl"] = len(list(filter(lambda x: x == 1, steps)))

        # Greedy
        lookahead = sum(steps)

        def get_racing_line(lookahead):
            l = []
            use_goal = True
            for (nx,ny,nd) in go_to_paths[lookahead][(x,y,d)]:
                for p in go_to_paths[lookahead][(x,y,d)][nx,ny,nd]:
                    if any(b in map(lambda x: (x[0],x[1]), [*p]) for b in blocked):
                        continue

                    if (nx,ny,nd) in position_distance_goal:
                        l.append(((position_distance_goal[(nx,ny,nd)], position_distance_midpoint[(nx,ny,nd)]), (nx,ny,nd), p))
                        if position_distance_goal[(nx,ny,nd)] < 12:
                            use_goal = False
                    else:
                        l.append(((inf, inf), (nx,ny,nd), p)) # Todo: calculate actual distance when falling off!

            l = sorted(map(lambda x: (x[0][0] if use_goal else x[0][1],x[1],x[2]), l))

            # Strategy!
            racing_line = list(filter(lambda x: x[0] == min(l)[0], l))
            return racing_line

        racing_line = get_racing_line(lookahead)
        if len(racing_line) == 0 and lookahead >= 10:
            # TODO: Handle 10-12 when blocked!
            def handle_10_to_12_blocked():
                assert (len(go_to_paths[lookahead][(x,y,d)]) == 1)
                for (nx,ny,nd) in go_to_paths[lookahead][(x,y,d)]:
                    assert (len(go_to_paths[lookahead][(x,y,d)][(nx,ny,nd)]) == 1)
                    for p in go_to_paths[lookahead][(x,y,d)][(nx,ny,nd)]:
                        pl = [*p]
                        while any(b in map(lambda x: (x[0],x[1]), pl) for b in blocked):
                            pl = pl[:-1]
                            return [(None,(nx,ny,nd),pl)]
                assert (False)
            racing_line = handle_10_to_12_blocked()
            sips["bonk"] += lookahead - len(racing_line)
            print ("!!! HAPPEND HERE !!!")
        else:
            while len(racing_line) == 0:
                lookahead -= 1
                sips["bonk"] += 1
                racing_line = get_racing_line(lookahead)

        px,py,pd = racing_line[0][1]
        player_steps = player_steps + [*racing_line[0][2]]
        # End of strategy

        sips["turn"] = sum([0 if (d1 == d2) else 1 for (_,_,d1), (_,_,d2) in zip([*racing_line[0][2]], [*racing_line[0][2]][1:])]) if sum(steps) >= 7 else 0

        if len([*racing_line[0][2]]) < sum(steps):
            blockers = list(filter(lambda x: x, (opl != pl and (x,y) == step_dir(px,py,pd) for opl,((x,y),d,_,_,r) in enumerate(players))))
            if len(blockers) == 1 and (not lookup_in_map(*step_dir(px,py,pd)) is None and 3 in lookup_in_map(*step_dir(px,py,pd))[1]):
                blocked.remove(step_dir(px,py,pd)) # Unflip player

        sips["off_map"] = int(not ((px, py, pd) in legal_positions or (px, py, pd) in next_outside_map))

        ng = ng if not sips["off_map"] else 0

        # TODO: Handle this better!
        start_line_pos = [(x,y,d) for x,y in start_line for d in game_map[(x,y)][0]][0]
        sips["goal_cheer"] = int(get_position(x,y,d) <= get_position(*start_line_pos) < get_position(px,py,pd))
        midpoint_pos = [(x,y,d) for x,y in mid_point for d in game_map[(x,y)][0]][0]
        sips["halfway_cheer"] = int(get_position(x,y,d) <= get_position(*midpoint_pos) < get_position(px,py,pd))

        nrounds = rounds + int(get_position(x,y,d) < get_position(px,py,pd))
        # print ("ROUND:", nrounds)
        ret_val = ((px, py), pd, ng, player_state, nrounds)

    players[pl] = ret_val

    sips["end_first"] = int(max((((r,position_distance_goal[(x,y,d)] if (x,y,d) in position_distance_goal else inf),pl) for pl,((x,y),d,_,_,r) in enumerate(players)), key=lambda x: x[0])[1] == pl)

    if (not lookup_in_map(*ret_val[0]) is None and 3 in lookup_in_map(*ret_val[0])[1]) or any(opl != pl and (x,y) == ret_val[0] for opl,((x,y),d,_,_,r) in enumerate(players)):
        blocked.add(ret_val[0])

    fell_off_map[pl] = sips["off_map"]

    return player_steps, sips, steps

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
        (( 0, 0), 5, 0, {(-5,10): 0},0),
        ((-1, 0), 5, 0, {(-5,10): 0},0),
        ((-1, 1), 5, 0, {(-5,10): 0},0),
        ((-2, 1), 5, 0, {(-5,10): 0},0),
        ((-2, 2), 5, 0, {(-5,10): 0},0),
        ((-3, 2), 5, 0, {(-5,10): 0},0),
        ((-3, 3), 5, 0, {(-5,10): 0},0),
        ((-4, 3), 5, 0, {(-5,10): 0},0),
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
        (0, 2): ([5],[0]),
        (2, 1): ([0],[0]),
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
        (18,-3): ([4],[0]),
        (17,-3): ([5],[0]),
        (18,-4): ([4],[0]),
        (18,-5): ([4],[0]),
        (18,-6): ([3,4],[0]),
        (18,-7): ([3],[0]),
        (17,-6): ([2,3],[2]),
        (17,-7): ([2],[0]),
        (16,-6): ([1,2],[0]),
        (16,-5): ([2],[0]),
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
        ( (0,3), 4, 0, {(10, 1): 0}, 0),
        ( (1,3), 4, 0, {(10, 1): 0}, 0),
        ( (0,4), 4, 0, {(10, 1): 0}, 0),
        ( (1,4), 4, 0, {(10, 1): 0}, 0),
        ( (0,5), 4, 0, {(10, 1): 0}, 0),
        ( (1,5), 4, 0, {(10, 1): 0}, 0),
        ( (0,6), 4, 0, {(10, 1): 0}, 0),
        ( (1,6), 4, 0, {(10, 1): 0}, 0),
        ( (0,7), 4, 0, {(10, 1): 0}, 0),
        ( (1,7), 4, 0, {(10, 1): 0}, 0),
        ( (0,8), 4, 0, {(10, 1): 0}, 0),
        ( (1,8), 4, 0, {(10, 1): 0}, 0),
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
        ((0,1), 2, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
        ((-1,1), 3, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
        ((-1,0), 4, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
        ((0,-1), 5, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
        ((1,-1), 0, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
        ((1,0), 1, 0, {(-1,2): 0, (-2,1): 0, (-1,-1): 0, (1,-2): 0, (2,-1): 0, (1,1): 0}, 0),
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
        ((3,-2), 3, 0, {
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
        }, 0),
        ((3,-1), 4, 0, {
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
        }, 0),
        ((2,1), 4, 0, {
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
        }, 0),
        ((1,2), 5, 0, {
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
        }, 0),
        ((-1,3), 5, 0, {
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
        }, 0),
        ((-2,3), 0, 0, {
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
        }, 0),
        ((-3,2), 0, 0, {
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
        }, 0),
        ((-3,1), 1, 0, {
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
        }, 0),
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
        ((29, 0), 1, 0, {},0),
        ((28, 0), 1, 0, {},0),
        ((29,-1), 1, 0, {},0),
        ((28,-1), 1, 0, {},0),
        ((29,-2), 1, 0, {},0),
        ((28,-2), 1, 0, {},0),
        ((29,-3), 1, 0, {},0),
        ((28,-3), 1, 0, {},0),
        ((29,-4), 1, 0, {},0),
    ]
    return game_map, players, start_line, mid_point

# Distance to goal:
def bfs_distance(game_map, start_line, comes_from):
    position_distance = {}
    stk = [(0,x,y,d) for x,y in start_line for d in game_map[(x,y)][0]]
    # visited = set()
    while len(stk) > 0:
        dist,x,y,d = heapq.heappop(stk)

        if (x,y,d) in position_distance and dist >= position_distance[(x,y,d)]:
            continue

        if not (x,y,d) in comes_from[1]: # Unreachable??
            continue

        # TODO: handle forced directions!
        if not (x,y,d) in position_distance or dist < position_distance[(x,y,d)]:
            position_distance[(x,y,d)] = dist

        assert (d in game_map[(x,y)][0])
        # print ((x,y,d), comes_from[(x,y,d)], dist)

        for nx,ny,nd in comes_from[1][(x,y,d)]:
            heapq.heappush(stk,(dist+1,nx,ny,nd))

    return position_distance

def compute_goto_path(game_map):
    legal_positions = set()
    for (x,y) in game_map:
        dirs,t = game_map[(x,y)]
        for d in dirs:
            legal_positions.add((x,y,d))


    next_outside_map = set()
    outside_map = set()
    for (x,y,d) in legal_positions: # Take up to 12 steps anywhere
        nx,ny = x,y
        for _ in range(12+1):
            nx, ny = step_dir(nx,ny,d)
            if not (nx,ny,d) in legal_positions:
                if (nx,ny) in game_map:
                    next_outside_map.add((nx,ny,d))
                    nx, ny = step_dir(nx,ny,d)
                outside_map.add((nx,ny,d))
                break

    positions = next_outside_map.union(legal_positions)
    go_to_paths = [{(x,y,d): {} for (x,y,d) in positions} for i in range(12+1)]
    for (x,y,d) in go_to_paths[0]:
        if not (x,y,d) in go_to_paths[0][(x,y,d)]:
            go_to_paths[0][(x,y,d)][(x,y,d)] = set()
        go_to_paths[0][(x,y,d)][(x,y,d)].add(((x,y,d),))

    for steps in range(0,9):
        for (x,y,d) in go_to_paths[steps]:
            for (cx,cy,cd) in go_to_paths[steps][(x,y,d)]:
                if (cx, cy, cd) in next_outside_map:
                    continue

                nx,ny = step_dir(cx,cy,cd)
                for nd in [(cd-1)%6,cd,(cd+1)%6]:
                    if not ((nx, ny, nd) in legal_positions or (nx, ny, nd) in next_outside_map):
                        continue
                    if not (nx,ny,nd) in go_to_paths[steps+1][(x,y,d)]:
                        go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)] = set()
                    # Add step to end of each path
                    for p in go_to_paths[steps][(x,y,d)][(cx,cy,cd)]:
                        go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)].add((*p,(nx,ny,nd)))

        print ("Explosion!", steps)
    for steps in range(9,12):
        for (x,y,d) in go_to_paths[steps]:
            for (cx,cy,cd) in go_to_paths[steps][(x,y,d)]:
                if (cx, cy, cd) in next_outside_map:
                    continue

                straight_paths = list(filter(lambda x: len(set(map(lambda x: x[2], list(x)))) == 1, go_to_paths[steps][(x,y,d)][(cx,cy,cd)]))
                if len(straight_paths) == 0:
                    continue

                nx,ny = step_dir(cx,cy,cd)
                nd = cd
                if not ((nx, ny, nd) in legal_positions or (nx, ny, nd) in next_outside_map):
                    continue
                if not (nx,ny,nd) in go_to_paths[steps+1][(x,y,d)]:
                    go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)] = set()
                # Add step to end of each path
                for p in straight_paths:
                    go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)].add((*p,(nx,ny,nd)))

        print ("Explosion!", steps)

    for (x,y,d) in outside_map:
        nx, ny = x,y
        p = ((nx,ny,d),)

        nx, ny = step_dir(nx,ny,(d+3)%6)
        if not ((nx, ny, d) in legal_positions or (nx, ny, d) in next_outside_map):
            break
        p = ((nx,ny,d),*p)

        for steps in range(1,12+1):
            for exp in range(max(10,steps),12+1):
                if not (nx,ny,d) in go_to_paths[exp]:
                    go_to_paths[exp][(nx,ny,d)] = {}
                if not (x,y,d) in go_to_paths[exp][(nx,ny,d)]:
                    go_to_paths[exp][(nx,ny,d)][(x,y,d)] = set()
                go_to_paths[exp][(nx,ny,d)][(x,y,d)].add(p) # Step off map in any amount of steps

            nx, ny = step_dir(nx,ny,(d+3)%6)
            if not ((nx, ny, d) in legal_positions or (nx, ny, d) in next_outside_map):
                break
            p = ((nx,ny,d),*p)

    for (x,y,d) in next_outside_map:
        nx, ny = step_dir(x,y,d)
        for steps in range(1,12+1):
            if not (nx,ny,d) in go_to_paths[steps][(x,y,d)]:
                go_to_paths[steps][(x,y,d)][(nx,ny,d)] = set()
            go_to_paths[steps][(x,y,d)][(nx,ny,d)].add(((x,y,d),(nx,ny,d))) # Step off map in any amount of steps

    # for i in range(len(go_to)):
    #     for (x,y,d) in go_to[i]:
    #         for (nx,ny,nd) in go_to[i][(x,y,d)]:
    #             if not (nx,ny,nd) in go_to_paths[i][(x,y,d)]:
    #                 print (((i, (x,y,d), (nx,ny,nd))))
    #                 print ((nx,ny,nd) in next_outside_map, (nx,ny,nd) in outside_map, (nx,ny,nd) in legal_positions)
    #                 print ((x,y,d) in next_outside_map, (x,y,d) in outside_map, (x,y,d) in legal_positions)
    #                 assert ((nx,ny,nd) in go_to_paths[i][(x,y,d)])

    return legal_positions, outside_map, next_outside_map, go_to_paths


# def racing_line(game_map, position_distance, ):
#     lookahead = 2
#     from_to = [((x,y,d),sorted(map(lambda x: (position_distance[x],x), filter(lambda x: x in position_distance, go_to[lookahead][(x,y,d)])))) for (x,y,d) in go_to[lookahead]]

#     # Exclude large things from list
#     from_to_new = [((x,y,d), list((((v + 1 + max(l)[0] if v < lookahead else v),o) for v,o in l) if len(l) > 0 and max(l)[0] - min(l)[0] > lookahead else l)) for (x,y,d),l in from_to]

#     racing_line = {(x,y,d): list(filter(lambda x: x[0] == min(l)[0], l)) for (x,y,d), l in from_to_new}

#     return go_to

def compute_comes_from(go_to_paths):
    comes_from = [{} for i in range(12+1)]
    for steps in range(len(go_to_paths)):
        for (x,y,d) in go_to_paths[steps]:
            for (nx,ny,nd) in go_to_paths[steps][(x,y,d)]:
                if not (nx,ny,nd) in comes_from[steps]:
                    comes_from[steps][(nx,ny,nd)] = set()
                comes_from[steps][(nx,ny,nd)].add((x,y,d))
    return comes_from

# game_map, players, start_line, mid_point = rtfm_map()
game_map, players, start_line, mid_point = loop_map()
# game_map, players, start_line, mid_point = clover_map()
# game_map, players, start_line, mid_point = tight_clover_map()
# game_map, players, start_line, mid_point = pod_racing_map()

players = players[:8]

legal_positions, outside_map, next_outside_map, go_to_paths = compute_goto_path(game_map)
comes_from = compute_comes_from(go_to_paths)
position_distance_goal     = bfs_distance(game_map, start_line, comes_from)
position_distance_midpoint = bfs_distance(game_map,  mid_point, comes_from)
# go_to = racing_line(game_map, comes_from, position_distance, valid_next_outside_map, legal_positions)

scale, (cx, cy) = compute_scale_and_center()

fell_off_map = [False for p in players]
out_of_map_counter = {}

frames = []
m = [[(0, 0, 0) for j in range(width)] for i in range(height)]
m = np.array(m,dtype=np.uint8)

pre_draw(m,game_map, cx, cy, scale)

for i,j,d in {(2, 0, 0)}:
    xi, yi = hex_coord(i, j, cx, cy, scale)
    for xj in range(-3,3+1):
        for yj in range(-3,3+1):
            draw_hex_dir(m, xi+xj, yi+yj, d, scale, (200,0,0))

# for i,j,d in {(1,2,5)}:
#     xi, yi = hex_coord(i, j, cx, cy, scale)
#     for xj in range(-3,3+1):
#         for yj in range(-3,3+1):
#             draw_hex_dir(m, xi+xj, yi+yj, d, scale, (0,200,0))

# for i,j,d in {(0,3,4),(1,2,4)}:
#     xi, yi = hex_coord(i, j, cx, cy, scale)
#     for xj in range(-3,3+1):
#         for yj in range(-3,3+1):
#             draw_hex_dir(m, xi+xj, yi+yj, d, scale, (0,0,200))

frame = save_map(np.array(m,dtype=np.uint8), f'Maps/000_map.png', [], (0, []), scale, out_of_map_counter) # players
frames.append(frame)

# exit()

drinking = [0 for p in players]
moves = [0 for p in players]

iters = 0
total_rounds = 10000
while (iters < total_rounds):
    iters += 1

    print(f'\nframe {iters:03d}.png')
    for pl,((x,y),d,g,player_state,rounds) in enumerate(players):
        players_steps, sips, steps = step_player(pl,players,fell_off_map)

        if fell_off_map[pl]:
            ox,oy = players[pl][0]
            if not (ox,oy) in out_of_map_counter:
                out_of_map_counter[(ox,oy)] = 0
            out_of_map_counter[(ox,oy)] += 1
        total_sips = sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0) + 5.5 * sips["off_map"] + sips["bonk"] + 11 * sips["gear_box"] + sips["start_last"] + sips["end_first"] + sips["halfway_cheer"] + sips["goal_cheer"] + sips["koblingsfejl"]
        if total_sips == 0:
            sips["no_sips"] = 1
            total_sips = 1

        print (sips)

        drinking[pl] += total_sips
        for opl in range(len(players)):
            if opl != pl: drinking[opl] += sips["halfway_cheer"] + sips["goal_cheer"] # Everyone drinks on cheers!

        moves[pl] += 1

        if iters < 40:
            filename = f'Maps/{iters:03d}_{pl:02d}_a_map.png'
            frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, players_steps), scale,out_of_map_counter)
            if iters < 10: frames.append(frame)
            filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
            frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale,out_of_map_counter)
            if iters < 10: frames.append(frame)

print (out_of_map_counter)

filename = f'Maps/result_map.png'
frame = save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale, out_of_map_counter)
frames.append(frame)

print ("Average:", sum(drinking) / sum(moves))
print ("Total:", sum(drinking))
print ("Per player", drinking)

frames[0].save(f'Maps/map.gif', append_images=frames[1:], save_all=True, duration=120, loop=0)
# frames[0].save(f'Maps/map.gif', format='GIF', append_images=frames[1:], save_all=True, duration=120, loop=0)

# Procedural generated racebile with powerups affecting map-gen
