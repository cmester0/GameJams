def rtfm_map():
    # 0 standard, 1 start fields, 2 blue, 3 star, 4 choice direction, 5 forced dirs
    game_map = {
        # Start Setup
        ( 0, 0): ([  5],[1]),
        (-1, 0): ([0,5],[1]),
        (-1, 1): ([4,5],[1]),
        (-2, 1): ([0,5],[1]),
        (-2, 2): ([4,5],[1]),
        (-3, 2): ([0,5],[1]),
        (-3, 3): ([5],[1]),
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
        ( 0, 6): ([3],[2]),
        (-1, 7): ([3],[0]),
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
        (-8, 8): ([4],[2]),
        (-7, 7): ([4],[0]),
        (-8, 7): ([5],  [2]),
        (-7, 6): ([5],  [3]),
        (-6, 5): ([5],  [0]),
        (-5, 4): ([5],  [0]),
    }
    start_line = [( 0, 0), ( 0, -1)]
    mid_point = [(-1, 6), (-1, 7)]

    player_state_start = {(-5,10): -1}
    player_state_mid =   {(-5,10): -1}

    players = [
        # Start Setup
        (( 0, 0), 5, 0, dict(player_state_start),0),
        ((-1, 0), 5, 0, dict(player_state_start),0),
        ((-1, 1), 5, 0, dict(player_state_start),0),
        ((-2, 1), 5, 0, dict(player_state_start),0),
        ((-2, 2), 5, 0, dict(player_state_start),0),
        ((-3, 2), 5, 0, dict(player_state_start),0),
        ((-3, 3), 5, 0, dict(player_state_start),0),
        ((-4, 3), 5, 0, dict(player_state_start),0),
    ]
    return game_map, players, start_line, mid_point, player_state_start, player_state_mid

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

    player_state_start = {(10, 1): 0}
    player_state_mid =   {(10, 1): 0}

    players = [
        ( (0,3), 4, 0, dict(player_state_start), 0),
        ( (1,3), 4, 0, dict(player_state_start), 0),
        ( (0,4), 4, 0, dict(player_state_start), 0),
        ( (1,4), 4, 0, dict(player_state_start), 0),
        ( (0,5), 4, 0, dict(player_state_start), 0),
        ( (1,5), 4, 0, dict(player_state_start), 0),
        ( (0,6), 4, 0, dict(player_state_start), 0),
        ( (1,6), 4, 0, dict(player_state_start), 0),
        ( (0,7), 4, 0, dict(player_state_start), 0),
        ( (1,7), 4, 0, dict(player_state_start), 0),
        ( (0,8), 4, 0, dict(player_state_start), 0),
        ( (1,8), 4, 0, dict(player_state_start), 0),
    ]
    return game_map, players, start_line, mid_point, player_state_start, player_state_mid

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

def chikane_map():
    game_map = {
        (16, -8) : ([1], []) ,
        (16, -7) : ([1], []) ,
        (16, -6) : ([0,1], [3]) ,
        (16, -5) : ([0], []) ,

        (17, -9) : ([2], []) ,
        (17, -6) : ([0,1], []) ,
        (17, -5) : ([0,5], []) ,

        (18, -13) : ([1], []) ,
        (18, -12) : ([1], []) ,
        (18, -11) : ([0], [3]) ,
        (18, -9) : ([3], []) ,
        (18, -6) : ([0], []) ,
        (18, -5) : ([5], []) ,

        (19, -14) : ([1,2], []) ,
        (19, -13) : ([2], []) ,
        (19, -11) : ([1], [3]) ,
        (19, -10) : ([2], [2]) ,
        (19, -6) : ([5], [2]) ,

        (20, -15) : ([1,2], []) ,
        (20, -14) : ([2], []) ,
        (20, -11) : ([2], [3]) ,
        (20, -8) : ([5], [3]) ,
        (20, -7) : ([4,5], [2]) ,

        (21, -19) : ([1], []) ,
        (21, -18) : ([1], [2, 3]) ,
        (21, -17) : ([1], []) ,
        (21, -16) : ([0,2], [2,4]) ,
        (21, -15) : ([], []) , # ( Should not happen? )
        (21, -12) : ([2], []) ,
        (21, -11) : ([3], [3]) ,
        (21, -9) : ([0,5], [3]) ,
        (21, -8) : ([4,5], [3]) ,

        (22, -20) : ([1,2], []) ,
        (22, -19) : ([2], []) ,
        (22, -16) : ([1], []) ,
        (22, -15) : ([1], []) ,
        (22, -14) : ([1], [3]) ,
        (22, -13) : ([1,2], []) ,
        (22, -12) : ([2], []) ,
        (22, -10) : ([0,5], []) ,
        (22, -9) : ([4,5], [3]) ,

        (23, -21) : ([2], []) ,
        (23, -20) : ([2,3], []) ,
        (23, -11) : ([0,5], []) ,
        (23, -10) : ([4,5], []) ,

        (24, -21) : ([2,3], []) ,
        (24, -12) : ([0,5], [1]) ,
        (24, -11) : ([4,5], [1]) ,

        (25, -21) : ([3], []) ,
        (25, -13) : ([0,5], [1]) ,
        (25, -12) : ([4,5], [1]) ,

        (26, -21) : ([3], [2]) ,
        (26, -14) : ([0,5], [1]) ,
        (26, -13) : ([4,5], [1]) ,

        (27, -22) : ([2], []) ,
        (27, -15) : ([0,5], [1]) ,
        (27, -14) : ([4,5], [1]) ,

        (28, -22) : ([3], []) ,
        (28, -21) : ([4], []) ,
        (28, -16) : ([0,5], [1]) ,
        (28, -15) : ([4,5], [1]) ,
        (28, -6) : ([5], [3]) ,
        (28, -5) : ([4], [3]) ,

        (29, -21) : ([3], []) ,
        (29, -17) : ([0,5], [1]) ,
        (29, -16) : ([4,5], [1]) ,
        (29, -10) : ([1], []) ,
        (29, -9) : ([1], []) ,
        (29, -8) : ([1], []) ,
        (29, -7) : ([0], [2]) ,
        (29, -5) : ([3], []) ,

        (30, -21) : ([3], [3]) ,
        (30, -18) : ([0,5], []) ,
        (30, -17) : ([4,5], []) ,
        (30, -13) : ([1], []) ,
        (30, -12) : ([1], [3]) ,
        (30, -11) : ([1,2], [2]) ,
        (30, -10) : ([2], []) ,
        (30, -7) : ([1,5], [2,5]) ,
        (30, -6) : ([2], []) ,

        (31, -21) : ([3], []) ,
        (31, -19) : ([0], []) ,
        (31, -18) : ([0,5], []) ,
        (31, -15) : ([1], []) ,
        (31, -14) : ([1,2], []) ,
        (31, -13) : ([2], []) ,
        (31, -8) : ([0,5], []) ,

        (32, -21) : ([3], []) ,
        (32, -19) : ([0,1], []) ,
        (32, -18) : ([0,1], []) ,
        (32, -17) : ([1], []) ,
        (32, -16) : ([1,2], [2]) ,
        (32, -15) : ([2], []) ,
        (32, -9) : ([0], []) ,
        (32, -8) : ([5], []) ,
        (32, -6) : ([5], []) ,
        (32, -5) : ([4], []) ,

        (33, -21) : ([3], []) ,
        (33, -19) : ([1], []) ,
        (33, -18) : ([1,2], []) ,
        (33, -17) : ([2], []) ,
        (33, -9) : ([0], []) ,
        (33, -7) : ([5], [3]) ,
        (33, -5) : ([3], []) ,

        (34, -21) : ([3], [2]) ,
        (34, -14) : ([5], []) ,
        (34, -13) : ([4], []) ,
        (34, -12) : ([4], []) ,
        (34, -9) : ([1], []) ,
        (34, -8) : ([0], [2]) ,
        (34, -5) : ([3], []) ,

        (35, -21) : ([3], [3]) ,
        (35, -16) : ([5], []) ,
        (35, -15) : ([0,4], [2,4]) ,
        (35, -12) : ([3], []) ,
        (35, -8) : ([1,5], [2,5]) ,
        (35, -7) : ([1], []) ,
        (35, -6) : ([2], [3]) ,

        (36, -21) : ([3], []) ,
        (36, -17) : ([0], [3]) ,
        (36, -15) : ([5], []) ,
        (36, -13) : ([2], [3]) ,
        (36, -12) : ([3], [2]) ,
        (36, -10) : ([5], []) ,
        (36, -9) : ([4,5], []) ,

        (37, -21) : ([3], [3]) ,
        (37, -17) : ([5], [2]) ,
        (37, -16) : ([4], [3]) ,
        (37, -13) : ([3], [3]) ,
        (37, -12) : ([3,4], []) ,
        (37, -11) : ([4], []) ,
        (37, -10) : ([4], []) ,

        (38, -21) : ([3], []) ,
        (38, -18) : ([0,5], []) ,

        (39, -21) : ([3], []) ,
        (39, -19) : ([0], []) ,
        (39, -18) : ([5], []) ,

        (40, -21) : ([3], [3]) ,
        (40, -19) : ([0], []) ,

        (41, -21) : ([3], []) ,
        (41, -19) : ([5], [3]) ,

        (42, -21) : ([3], [2]) ,
        (42, -20) : ([4], [2]) ,
    }
    start_line = [(29, -17), (29, -16)]
    mid_point = [(39, -19), (39, -18)]

    forced_dir = {(30, -7): 0, (35, -8): 0} # Start from -1... weird interaction (could be autogened, assuming order correct)

    players = [
        ((29, -16), 5, 0, dict(forced_dir), 0),
        ((29, -17), 5, 0, dict(forced_dir), 0),
        ((28, -15), 5, 0, dict(forced_dir), 0),
        ((28, -16), 5, 0, dict(forced_dir), 0),
        ((27, -14), 5, 0, dict(forced_dir), 0),
        ((27, -15), 5, 0, dict(forced_dir), 0),
        ((26, -13), 5, 0, dict(forced_dir), 0),
        ((26, -14), 5, 0, dict(forced_dir), 0),
        ((25, -12), 5, 0, dict(forced_dir), 0),
        ((25, -13), 5, 0, dict(forced_dir), 0),
        ((24, -11), 5, 0, dict(forced_dir), 0),
        ((24, -12), 5, 0, dict(forced_dir), 0),
    ]

    player_state_start = {(30, -7): 0, (35, -8): 0} # dict(forced_dir)
    player_state_mid = {(30, -7): 0, (35, -8): 0} # dict(forced_dir)

    return game_map, players, start_line, mid_point, player_state_start, player_state_mid
