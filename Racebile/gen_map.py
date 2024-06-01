
import opensimplex
import uuid
def gen_map():
    seed_x = uuid.uuid1().int >> 64
    seed_y = uuid.uuid1().int >> 64

    l = []
    sz = 100000000
    osc = 10  # Circularity
    dim = 30

    opensimplex.seed(seed_x)

    x_old, y_old = None, None
    for i in range(10000000): # sz
        r = 40

        ri = opensimplex.noise2(x=osc * cos(i / sz * 2 * pi), y=osc * sin(i / sz * 2 * pi))
        r = 100+int((ri+1)/2*20)
        x, y = (round(r * cos((i / sz) * 2 * pi)), round(r * sin((i / sz) * 2 * pi)))

        if (x_old != x or y_old != y):
            l.append((x,y)) # Zero is dummy val

        x_old, y_old = x,y
        print (i,"/",sz, x,y)

    l_res = list(l)
    l = []
    for (x_old,y_old), (x,y) in zip(l_res, l_res[1:]):
        for d in range(6):
            if step_dir(x_old, y_old, d) == (x, y):
                l.append((x_old,y_old,d)) # Zero is dummy val
                break
        else:
            print ("fail direction")
            l.append((x_old,y_old,0))

    l = starting_platform(l[0][0], l[0][1]) + l

    return l

def starting_platform(xi,yi):
    return [(xi-0,yi+0,1),
            (xi-1,yi+0,0),
            (xi+0,yi-1,1),
            (xi-1,yi-1,0),
            (xi+0,yi-2,1),
            (xi-1,yi-2,0),
            (xi+0,yi-3,1),
            (xi-1,yi-3,0)]
