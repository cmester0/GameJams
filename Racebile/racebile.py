from math import cos, sin, pi, floor, ceil, inf

import itertools

from maps import *
from draw_hex_map import *
from run_game import *

# game_map, orig_players, start_line, mid_point, player_state_start, player_state_mid = chikane_map()
game_map, orig_players, start_line, mid_point, player_state_start, player_state_mid = rtfm_map()
# game_map, orig_players, start_line, mid_point, player_state_start, player_state_mid = loop_map()

# game_map, orig_players, start_line, mid_point = clover_map()
# game_map, orig_players, start_line, mid_point = tight_clover_map()
# game_map, orig_players, start_line, mid_point = pod_racing_map()

players = list(orig_players[:8])
out_of_map_counter = {}

drawing = DrawHexMap(2000, 2000)
drawing.set_map(game_map)

for i,j,d in {(-5, 11, 4), (-5, 10, 4), (-5, 9, 4)}: # , (31, -19, 5)
    xi, yi = drawing.hex_coord(i, j)
    for xj in range(-3,3+1):
        for yj in range(-3,3+1):
            drawing.draw_hex_dir(xi+xj, yi+yj, d, (200,0,0))

drawing.update_init()
drawing.save_map( f'Maps/000_map.png', [], (0, []), out_of_map_counter) # players
print ("saved")



logic = GameLogic(game_map, start_line, mid_point, player_state_start, player_state_mid)

fell_off_map = [False for p in players]


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

    if verbose:
        print(f'\nframe {iters:03d}.png')
        filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
        drawing.save_map(filename, players, (0, []),out_of_map_counter)
    for pl,((x,y),d,g,player_state,rounds) in enumerate(players):
        player_steps, sips, steps, total_sips = logic.step_player(pl,players,fell_off_map,blocked)

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

        if iters < 0 and len(average_rounds) == 0:
            print (f"save {iters:03d}, player {pl:02d}", rounds)
            filename = f'Maps/{iters:03d}_{pl:02d}_a_map.png'
            drawing.save_map(filename, players, (pl, player_steps),out_of_map_counter)
            filename = f'Maps/{iters:03d}_{pl:02d}_b_map.png'
            drawing.save_map(filename, players, (pl, []),out_of_map_counter)

        if all(rounds > 1 for _,_,_,_,rounds in players): # Any , All
            # if iters < 10:
            #     print (len(average_rounds), iters)
            #     print (players)
            #     filename = f'Maps/bug.png'
            #     drawing.save_map(filename, players, (pl, player_steps),out_of_map_counter)
            #     exit()

            players = list(orig_players[:len(players)])
            # logic = GameLogic(game_map, start_line, mid_point)
            fell_off_map = [False for p in players]
            blocked = set()

            # drinking = [0 for p in players]
            # moves = [0 for p in players]

            average_rounds.append(iters)
            if (len(average_rounds) % 100) == 0:
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
