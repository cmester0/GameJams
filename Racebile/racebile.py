from math import cos, sin, pi, floor, ceil, inf

import itertools

from maps import *
from draw_hex_map import *
from run_game import *

# game_map, players, start_line, mid_point = rtfm_map()
game_map, orig_players, start_line, mid_point = loop_map()
# game_map, players, start_line, mid_point = clover_map()
# game_map, players, start_line, mid_point = tight_clover_map()
# game_map, players, start_line, mid_point = pod_racing_map()

players = list(orig_players[:7])

drawing = DrawHexMap(2000, 2000)
drawing.set_map(game_map)

logic = GameLogic(game_map, start_line, mid_point)

fell_off_map = [False for p in players]
out_of_map_counter = {}

# for i,j,d in {(3, 0, 5), (15, 2, 5), (9, 2, 1)}:
#     xi, yi = hex_coord(i, j, cx, cy, scale)
#     for xj in range(-3,3+1):
#         for yj in range(-3,3+1):
#             draw_hex_dir(m, xi+xj, yi+yj, d, scale, (200,0,0))

drawing.save_map( f'Maps/000_map.png', [], (0, []), out_of_map_counter) # players

# Running logic!
blocked = set()
drinking = [0 for p in players]
moves = [0 for p in players]

average_rounds = []

verbose = False
iters = 0
total_rounds = 100_000
while (iters < total_rounds):
    iters += 1

    if verbose or iters % 5000 == 0:
        print(f'\nframe {iters:03d}.png')
        filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
        drawing.save_map(filename, players, (0, []),out_of_map_counter)
    for pl,((x,y),d,g,player_state,rounds) in enumerate(players):
        players_steps, sips, steps, total_sips = logic.step_player(pl,players,fell_off_map,blocked)

        if sips["off_map"]:
            ox,oy = players[pl][0]
            if not (ox,oy) in out_of_map_counter:
                out_of_map_counter[(ox,oy)] = 0
            out_of_map_counter[(ox,oy)] += 1

        drinking[pl] += total_sips
        for opl in range(len(players)):
            if opl != pl: drinking[opl] += sips["halfway_cheer"] + sips["goal_cheer"] # Everyone drinks on cheers!

        moves[pl] += 1

        if verbose:
            print (sips)

        if iters < 0:
            filename = f'Maps/{iters:03d}_{pl:02d}_a_map.png'
            drawing.save_map(filename, players, (pl, players_steps),out_of_map_counter)
            filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
            drawing.save_map(filename, players, (pl, []),out_of_map_counter)

        if all(rounds > 1 for _,_,_,_,rounds in players): # Any , All
            players = list(orig_players[:len(players)])
            # logic = GameLogic(game_map, start_line, mid_point)
            fell_off_map = [False for p in players]
            blocked = set()

            # drinking = [0 for p in players]
            # moves = [0 for p in players]

            average_rounds.append(iters)
            print (len(average_rounds), iters, min(average_rounds), max(average_rounds), sum(average_rounds)/len(average_rounds))
            iters = 0
            break

    if len(average_rounds) > 10000:
        break

print (out_of_map_counter)

filename = f'Maps/result_map.png'
drawing.save_map(filename, players, (pl, []), out_of_map_counter)

print ("Average:", sum(drinking) / sum(moves))
print ("Total:", sum(drinking))
print ("Per player", drinking)

drawing.frames[0].save(f'Maps/map.gif', append_images=drawing.frames[1:], save_all=True, duration=120, loop=0)

# Procedural generated racebile with powerups affecting map-gen
