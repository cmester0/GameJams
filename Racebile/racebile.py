from math import cos, sin, pi, floor, ceil, inf

import itertools

from maps import *
from draw_hex_map import *
from run_game import *

# game_map, players, start_line, mid_point = rtfm_map()
game_map, players, start_line, mid_point = loop_map()
# game_map, players, start_line, mid_point = clover_map()
# game_map, players, start_line, mid_point = tight_clover_map()
# game_map, players, start_line, mid_point = pod_racing_map()

players = players[:8]

drawing = DrawHexMap(2000, 2000, game_map)

logic = GameLogic(game_map, start_line, mid_point)



fell_off_map = [False for p in players]
out_of_map_counter = {}

# frames = []
# m = [[(0, 0, 0) for j in range(drawing.width)] for i in range(drawing.height)]
# m = np.array(m,dtype=np.uint8)

# drawing.pre_draw(m,game_map, cx, cy, scale)

# for i,j,d in {(3, 0, 5), (15, 2, 5), (9, 2, 1)}:
#     xi, yi = hex_coord(i, j, cx, cy, scale)
#     for xj in range(-3,3+1):
#         for yj in range(-3,3+1):
#             draw_hex_dir(m, xi+xj, yi+yj, d, scale, (200,0,0))

drawing.save_map( f'Maps/000_map.png', [], (0, []), out_of_map_counter) # players

drinking = [0 for p in players]
moves = [0 for p in players]

verbose = False
iters = 0
total_rounds = 1_000_000
while (iters < total_rounds):
    iters += 1

    if verbose or iters % 5000 == 0:
        print(f'\nframe {iters:03d}.png')
    for pl,((x,y),d,g,player_state,rounds) in enumerate(players):
        players_steps, sips, steps = logic.step_player(pl,players,fell_off_map)

        if fell_off_map[pl]:
            ox,oy = players[pl][0]
            if not (ox,oy) in out_of_map_counter:
                out_of_map_counter[(ox,oy)] = 0
            out_of_map_counter[(ox,oy)] += 1
        total_sips = sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0) + 5.5 * sips["off_map"] + sips["bonk"] + 11 * sips["gear_box"] + sips["start_last"] + sips["end_first"] + sips["halfway_cheer"] + sips["goal_cheer"] + sips["koblingsfejl"]
        if total_sips == 0:
            sips["no_sips"] = 1
            total_sips = 1

        if verbose:
            print (sips)

        drinking[pl] += total_sips
        for opl in range(len(players)):
            if opl != pl: drinking[opl] += sips["halfway_cheer"] + sips["goal_cheer"] # Everyone drinks on cheers!

        moves[pl] += 1

        if iters < 0:
            filename = f'Maps/{iters:03d}_{pl:02d}_a_map.png'
            drawing.save_map(np.array(m,dtype=np.uint8),filename, players, (pl, players_steps), scale,cx, cy,out_of_map_counter)
            filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
            drawing.save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale,cx, cy,out_of_map_counter)

print (out_of_map_counter)

filename = f'Maps/result_map.png'
drawing.save_map(np.array(m,dtype=np.uint8),filename, players, (pl, []), scale, cx, cy, out_of_map_counter)

print ("Average:", sum(drinking) / sum(moves))
print ("Total:", sum(drinking))
print ("Per player", drinking)

drawing.frames[0].save(f'Maps/map.gif', append_images=drawing.frames[1:], save_all=True, duration=120, loop=0)
# frames[0].save(f'Maps/map.gif', format='GIF', append_images=frames[1:], save_all=True, duration=120, loop=0)

# Procedural generated racebile with powerups affecting map-gen
