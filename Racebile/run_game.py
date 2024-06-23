from math import inf
import heapq
import random

from scipy.stats import poisson
from scipy.stats import geom
from scipy.stats import binom

# Helper functions

class GameLogic:
    def __init__(self, game_map, start_line, mid_point, player_state_start, player_state_mid):
        self.game_map = game_map
        self.start_line = [(x,y,d) for x,y in start_line for d in self.game_map[(x,y)][0]]
        self.mid_point  = [(x,y,d) for x,y in  mid_point for d in self.game_map[(x,y)][0]]

        self.player_state_start = player_state_start
        self.player_state_mid   = player_state_mid

        self.bfs_to_dict = {}
        
        self.legal_positions, self.outside_map, self.next_outside_map, self.go_to_paths = None, None, None, None
        self.compute_goto_path()

        self.comes_from = None
        self.compute_comes_from()

    def step_dir(self,x,y,d):
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

    def lookup_in_map(self,x,y):
        if not (x,y) in self.game_map:
            return None
        else:
            return self.game_map[(x,y)]

    # TODO: compute directions when falling off map in step instead
    def get_map_dirs(self, x, y, player_state):
        cm = self.lookup_in_map(x,y)
        if cm is None:
            d = None
        else:
            if 5 in cm[1]:
                d = cm[0][player_state[(x,y)]]
            else:
                d = cm[0][0]
        return d, player_state

    def step_player(self,pl,players,fell_off_map,blocked):
        (x,y),d,g,player_state,rounds = players[pl]

        o_player_state = dict(player_state)

        if not self.lookup_in_map(x,y) is None and 2 in self.lookup_in_map(x,y)[1]:
            ng = max(g-1,1)
        else:
            ng = g + 1 if g < 3 else g # TODO: Strategy

        sips = {
            "turn": 0,
            "off_map": 0,
            "gas": 0,
            "bonk": 0,
            "gear_box": 0,
            "start_last": 0,
            "end_first": 0,
            "halfway_cheer": 0,
            "goal_cheer": 0,
            "koblingsfejl": 0,
            "no_sips": 0
        }

        steps = [random.randint(1,4) for i in range(ng)]
        sips["gas"] = sum(geom.rvs(2/3,size=ng))-ng

        player_steps = []

        if fell_off_map[pl]:
            # TODO: Drink?
            if self.step_dir(x,y,(d+3)%6) in blocked:
                sips["off_map"] = 1
            else:
                player_steps.append((x,y,d))
                x,y = self.step_dir(x,y,(d+3)%6)
                d, player_state = self.get_map_dirs(x, y, player_state)
                player_steps.append((x,y,d))

        # TODO: Better!
        # sips["start_last"] = int(min((((r,self.position_distance_goal[(x,y,d,tuple([player_state[(x,y)] for (x,y) in player_state]))] if (x,y,d,tuple([player_state[(x,y)] for (x,y) in player_state])) in self.position_distance_goal else inf),pl) for pl,((x,y),d,_,_,r) in enumerate(players)), key=lambda x: x[0])[1] == pl)

        if sips["off_map"] == 1:
            ng = 0
            ret_val = ((x,y),d,ng,player_state,rounds)
        elif steps == [1,1,1]: # Destroy gear box
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
                for (nx,ny,nd) in self.go_to_paths[lookahead][(x,y,d)]:
                    # TODO: Handle double loops?
                    for p in self.go_to_paths[lookahead][(x,y,d)][nx,ny,nd]:
                        valid_forced_dir = True
                        temp_player_state = dict(player_state)
                        for (fd_x,fd_y,fd_d) in [*p][1:]:
                            if (fd_x, fd_y) in temp_player_state:
                                cm = self.lookup_in_map(fd_x,fd_y)
                                if cm[0][temp_player_state[(fd_x,fd_y)]] == fd_d:
                                    temp_player_state[(fd_x,fd_y)] = (1 + temp_player_state[(fd_x,fd_y)]) % len(cm[0])
                                else:
                                    valid_forced_dir = False
                                    break

                        if not valid_forced_dir:
                            continue

                        if any(b in map(lambda x: (x[0],x[1]), [*p]) for b in blocked):
                            continue

                        goal_dist = self.bfs_to_pos([(nx,ny,nd)], self.start_line, temp_player_state)
                        mid_dist = self.bfs_to_pos([(nx,ny,nd)], self.mid_point , temp_player_state)
                        l.append((
                            max(
                                0 if goal_dist == inf else goal_dist, # start line dist
                                0 if mid_dist == inf else mid_dist # mid point dist
                            ),
                            (nx,ny,nd),
                            p))
                        # if tps_tuple in self.position_distance_goal and self.position_distance_goal[tps_tuple] < 12:
                        #     use_goal = False

                l = sorted(map(lambda x: (x[0],x[1],x[2]), l))

                # Strategy!
                racing_line = list(filter(lambda x: x[0] == min(l)[0], l))
                return racing_line

            racing_line = get_racing_line(lookahead)
            if len(racing_line) == 0 and lookahead >= 10:
                # TODO: Handle 10-12 when blocked!
                def handle_10_to_12_blocked():
                    assert (len(self.go_to_paths[lookahead][(x,y,d)]) == 1)
                    for (nx,ny,nd) in self.go_to_paths[lookahead][(x,y,d)]:
                        assert (len(self.go_to_paths[lookahead][(x,y,d)][(nx,ny,nd)]) == 1)
                        for p in self.go_to_paths[lookahead][(x,y,d)][(nx,ny,nd)]:
                            pl = [*p]
                            while any(b in map(lambda x: (x[0],x[1]), pl) for b in blocked):
                                pl = pl[:-1]
                                return [(None,(nx,ny,nd),pl)]
                    assert (False)
                racing_line = handle_10_to_12_blocked()
                sips["bonk"] += lookahead - len(racing_line)
                # TODO: print ("!!! HAPPEND HERE !!!")
            else:
                while len(racing_line) == 0:
                    lookahead -= 1
                    sips["bonk"] += 1
                    racing_line = get_racing_line(lookahead)

            px,py,pd = racing_line[0][1]
            player_steps = player_steps + [*racing_line[0][2]]

            for (fd_x,fd_y,fd_d) in [*racing_line[0][2]][1:]:
                if (fd_x, fd_y) in player_state:
                    cm = self.lookup_in_map(fd_x,fd_y)
                    if cm[0][player_state[(fd_x,fd_y)]] == fd_d:
                        player_state[(fd_x,fd_y)] = (1 + player_state[(fd_x,fd_y)]) % len(cm[0])

            # End of strategy

            sips["turn"] = sum([0 if (d1 == d2) else 1 for (_,_,d1), (_,_,d2) in zip([*racing_line[0][2]], [*racing_line[0][2]][1:])]) if sum(steps) >= 7 else 0

            if len([*racing_line[0][2]]) < sum(steps):
                blockers = list(filter(lambda x: x, (opl != pl and (x,y) == self.step_dir(px,py,pd) for opl,((x,y),d,_,_,r) in enumerate(players))))
                if len(blockers) == 1 and (not self.lookup_in_map(*self.step_dir(px,py,pd)) is None and 3 in self.lookup_in_map(*self.step_dir(px,py,pd))[1]):
                    if self.step_dir(px,py,pd) in blocked:
                        blocked.remove(self.step_dir(px,py,pd)) # Unflip player

            sips["off_map"] = int(not ((px, py, pd) in self.legal_positions or (px, py, pd) in self.next_outside_map))

            ng = ng if not sips["off_map"] else 0

            # # TODO: Handle this better!
            # start_line_pos = [(x,y,d) for x,y in self.start_line for d in self.game_map[(x,y)][0]][0]
            # sips["goal_cheer"] = int(
            #     self.get_position_mid(x,y,d,o_player_state) <=
            #     self.get_position_mid(*start_line_pos,self.player_state_start) <
            #     self.get_position_mid(px,py,pd,player_state))
            # midpoint_pos = [(x,y,d) for x,y in self.mid_point for d in self.game_map[(x,y)][0]][0]
            # sips["halfway_cheer"] = int(
            #     self.get_position_goal(x,y,d,o_player_state) <=
            #     self.get_position_goal(*midpoint_pos,self.player_state_mid) <
            #     self.get_position_goal(px,py,pd,player_state))

            # print ((( x, y), ( x, y) in self.game_map, ( x, y, d,tuple([o_player_state[(x,y)] for (x,y) in o_player_state])) in self.position_distance_goal),
            #        ((px,py), (px,py) in self.game_map, (px,py,pd,tuple([  player_state[(x,y)] for (x,y) in   player_state])) in self.position_distance_goal))

            # if (   self.bfs_to_pos([( x, y, d)],self.start_line,o_player_state)
            #        <= len([*racing_line[0][2]][1:])):
            #     print (self.bfs_to_pos([( x, y, d)],self.start_line,o_player_state) + self.bfs_to_pos(self.start_line,(px,py,pd),self.player_state_start),
            #            len([*racing_line[0][2]][1:]),
            #            self.bfs_to_pos([( x, y, d)],self.start_line,o_player_state)
            #            <= len([*racing_line[0][2]][1:]))

            nrounds = rounds + int(
                self.bfs_to_pos([( x, y, d)],self.start_line,o_player_state) <= len([*racing_line[0][2]][1:])
                # self.bfs_to_pos([( x, y, d)],[(px,py,pd)],o_player_state)
            )
            # print ("ROUND:", nrounds)
            ret_val = ((px, py), pd, ng, player_state, nrounds)

        players[pl] = ret_val

        # sips["end_first"] = int(max(
        #     (((r,
        #        self.position_distance_goal[(x,y,d,tuple([player_state[(x,y)] for (x,y) in player_state]))]
        #        if (x,y,d,tuple([player_state[(x,y)] for (x,y) in player_state])) in self.position_distance_goal else
        #        inf),
        #       pl) for pl,((x,y),d,_,_,r) in enumerate(players)),
        #     key=lambda x: x[0])[1] == pl)

        if ((not self.lookup_in_map(*ret_val[0]) is None and 3 in self.lookup_in_map(*ret_val[0])[1]) or
            any(opl != pl and (x,y) == ret_val[0] for opl,((x,y),d,_,_,r) in enumerate(players))):
            if not (*ret_val[0],ret_val[1]) in self.outside_map:
                blocked.add(ret_val[0])

        fell_off_map[pl] = sips["off_map"]
        total_sips = sips["turn"] + (sips["gas"]-2 if sips["gas"] > 2 else 0) + 5.5 * sips["off_map"] + sips["bonk"] + 11 * sips["gear_box"] + sips["start_last"] + sips["end_first"] + sips["halfway_cheer"] + sips["goal_cheer"] + sips["koblingsfejl"]
        if total_sips == 0:
            sips["no_sips"] = 1
            total_sips = 1

        return player_steps, sips, steps, total_sips


    def bfs_to_pos(self, start, goal, player_state):
        # print ("from ", start, " to ", goal)
        # print (f"len {len(self.bfs_to_dict)} vs {len([(x,y,d) for x,y in self.game_map for d in self.game_map[(x,y)][0]])}")

        position_distance = {}
        stk = [(0,x,y,d,dict(player_state)) for x,y,d in start]

        if (tuple(start), tuple(goal)) in self.bfs_to_dict:
            return self.bfs_to_dict[(tuple(start), tuple(goal))]

        while len(stk) > 0:
            dist,x,y,d,ps = heapq.heappop(stk)

            # Handle forced paths correctly!
            if (x, y) in ps:
                cm = self.lookup_in_map(x,y)
                if cm[0][ps[(x,y)]] == d:
                    ps[(x,y)] = (1 + ps[(x,y)]) % len(cm[0])
                else:
                    # print ("invalid!", (x,y,d), tuple([ps[(x,y)] for (x,y) in ps]))
                    continue

            ps_tuple = tuple([ps[(x,y)] for (x,y) in ps])

            if (x,y,d,ps_tuple) in position_distance:
                continue

            if not (x,y) in self.game_map or not d in self.game_map[(x,y)][0]: # Unreachable??
                nx,ny = self.step_dir(x,y,(d+3)%6)
                for nd in self.game_map[(nx,ny)][0]:
                    heapq.heappush(stk,(dist+1,nx,ny,nd,dict(ps)))
                continue

            if not (x,y,d,ps_tuple) in position_distance:
                position_distance[(x,y,d,ps_tuple)] = dist

            assert (d in self.game_map[(x,y)][0])

            if (x,y,d) in goal: # TODO: include d?
                self.bfs_to_dict[(tuple(start), tuple(goal))] = dist
                return dist

            for nx,ny,nd in self.go_to_paths[1][(x,y,d)]:
                heapq.heappush(stk,(dist+1,nx,ny,nd,dict(ps)))

        # assert False, f"({start}, {goal})"
        # exit() # Should always be a distance
        return inf

    def compute_goto_path(self):
        self.legal_positions = set()
        for (x,y) in self.game_map:
            dirs,t = self.game_map[(x,y)]
            for d in dirs:
                self.legal_positions.add((x,y,d))


        self.next_outside_map = set()
        self.outside_map = set()
        for (x,y,d) in self.legal_positions: # Take up to 12 steps anywhere
            nx,ny = x,y
            for _ in range(12+1):
                nx, ny = self.step_dir(nx,ny,d)
                if not (nx,ny,d) in self.legal_positions:
                    if (nx,ny) in self.game_map:
                        self.next_outside_map.add((nx,ny,d))
                        nx, ny = self.step_dir(nx,ny,d)
                    if (nx,ny) in self.game_map:
                        print ("TODO:", (nx,ny,d))
                    self.outside_map.add((nx,ny,d))
                    break

        positions = self.next_outside_map.union(self.legal_positions)
        self.go_to_paths = [{(x,y,d): {} for (x,y,d) in positions} for i in range(12+1)]
        for (x,y,d) in self.go_to_paths[0]:
            if not (x,y,d) in self.go_to_paths[0][(x,y,d)]:
                self.go_to_paths[0][(x,y,d)][(x,y,d)] = set()
            self.go_to_paths[0][(x,y,d)][(x,y,d)].add(((x,y,d),))

        # Should be done in 1 step function, and enfored by others?
        # TODO: Forced turns!
        # TODO: Directed turns!

        for steps in range(0,9):
            for (x,y,d) in self.go_to_paths[steps]:
                for (cx,cy,cd) in self.go_to_paths[steps][(x,y,d)]:
                    if (cx, cy, cd) in self.next_outside_map:
                        continue

                    nx,ny = self.step_dir(cx,cy,cd)
                    for nd in [(cd-1)%6,cd,(cd+1)%6]:
                        if not ((nx, ny, nd) in self.legal_positions or (nx, ny, nd) in self.next_outside_map):
                            continue
                        if not (nx,ny,nd) in self.go_to_paths[steps+1][(x,y,d)]:
                            self.go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)] = set()
                        # Add step to end of each path
                        for p in self.go_to_paths[steps][(x,y,d)][(cx,cy,cd)]:
                            self.go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)].add((*p,(nx,ny,nd)))

            print ("Explosion!", steps)
        for steps in range(9,12):
            for (x,y,d) in self.go_to_paths[steps]:
                for (cx,cy,cd) in self.go_to_paths[steps][(x,y,d)]:
                    if (cx, cy, cd) in self.next_outside_map:
                        continue

                    straight_paths = list(filter(lambda x: len(set(map(lambda x: x[2], list(x)))) == 1, self.go_to_paths[steps][(x,y,d)][(cx,cy,cd)]))
                    if len(straight_paths) == 0:
                        continue

                    nx,ny = self.step_dir(cx,cy,cd)
                    nd = cd
                    if not ((nx, ny, nd) in self.legal_positions or (nx, ny, nd) in self.next_outside_map):
                        continue
                    if not (nx,ny,nd) in self.go_to_paths[steps+1][(x,y,d)]:
                        self.go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)] = set()
                    # Add step to end of each path
                    for p in straight_paths:
                        self.go_to_paths[steps+1][(x,y,d)][(nx,ny,nd)].add((*p,(nx,ny,nd)))

            print ("Explosion!", steps)

        for (x,y,d) in self.outside_map:
            nx, ny = x,y
            p = ((nx,ny,d),)

            nx, ny = self.step_dir(nx,ny,(d+3)%6)
            if not ((nx, ny, d) in self.legal_positions or (nx, ny, d) in self.next_outside_map):
                break
            p = ((nx,ny,d),*p)

            for steps in range(1,12+1):
                for exp in range(max(10,steps),12+1):
                    if not (nx,ny,d) in self.go_to_paths[exp]:
                        self.go_to_paths[exp][(nx,ny,d)] = {}
                    if not (x,y,d) in self.go_to_paths[exp][(nx,ny,d)]:
                        self.go_to_paths[exp][(nx,ny,d)][(x,y,d)] = set()
                    self.go_to_paths[exp][(nx,ny,d)][(x,y,d)].add(p) # Step off map in any amount of steps

                nx, ny = self.step_dir(nx,ny,(d+3)%6)
                if not ((nx, ny, d) in self.legal_positions or (nx, ny, d) in self.next_outside_map):
                    break
                p = ((nx,ny,d),*p)

        # TODO: Keep stepping (not just 1) until you fall out of map, or hit wall!
        for (x,y,d) in self.next_outside_map:
            nx, ny = self.step_dir(x,y,d)
            for steps in range(1,12+1):
                if not (nx,ny,d) in self.go_to_paths[steps][(x,y,d)]:
                    self.go_to_paths[steps][(x,y,d)][(nx,ny,d)] = set()
                self.go_to_paths[steps][(x,y,d)][(nx,ny,d)].add(((x,y,d),(nx,ny,d))) # Step off map in any amount of steps


    def compute_comes_from(self):
        self.comes_from = [{} for i in range(12+1)]
        for steps in range(len(self.go_to_paths)):
            for (x,y,d) in self.go_to_paths[steps]:
                for (nx,ny,nd) in self.go_to_paths[steps][(x,y,d)]:
                    if not (nx,ny,nd) in self.comes_from[steps]:
                        self.comes_from[steps][(nx,ny,nd)] = set()
                    self.comes_from[steps][(nx,ny,nd)].add((x,y,d))
