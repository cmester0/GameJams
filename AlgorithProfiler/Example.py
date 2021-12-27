from collections import deque
from sys import stdin
import cProfile

def highest_label_selection_rule():
    n,m,s,t = map(int, input().split())

    adjecency = [[] for _ in range(n)]
    h = [0 for _ in range(n)]
    e = [0 for _ in range(n)]
    c = [{} for _ in range(n)]
    f = [{} for _ in range(n)]
    current = [0 for _ in range(n)]

    def initialize ():
        lines = stdin.readlines()

        for line in lines:
            u,v,w = map(int, line.split())

            adjecency[u].append(v)
            adjecency[v].append(u)

            c[u][v] = w
            c[v][u] = 0

            f[u][v] = 0
            f[v][u] = 0
        
        h[s] = n

        for v in adjecency[s]:
            csv = c[s][v]

            f[s][v] += csv
            f[v][s] -= csv

            e[v] += csv
            e[s] -= csv
    
    initialize ()
    buckets = [set() for i in range(n)]

    for v in range(n):
        buckets[0].add(v)
        buckets[0].remove(s)

    def set_h_u(u,hu):
        if h[u] < n:
            if u in buckets[h[u]]:
                buckets[h[u]].remove(u)
                h[u] = hu
        if h[u] < n:
            buckets[h[u]].add(u)

    def cf(e):
        (u,v) = e
        return c[u][v] - f[u][v]

    def bfs_height(u):
        S = []
        discovered = [False for _ in range(n)]

        uindex = 0

        S.append(u)
        while not len(S) == 0:
            u = S.pop()

            if discovered[u]:
                continue

            set_h_u(u, uindex)
            uindex += 1

            discovered[u] = True

            for v in adjecency[u]:
                if cf((v,u)) > 0:
                    S.append(v)

    def push(uv):
        (u,v) = uv

        df_u_v = min(e[u], cf((u,v)))

        f[u][v] += df_u_v
        f[v][u] -= df_u_v

        e[u] -= df_u_v
        e[v] += df_u_v

    def relabel(u):
        set_h_u(u, 1 + min(list(map(lambda x: h[x], filter(lambda v: cf((u,v)) > 0, adjecency[u])))))

    def discharge(u):
        while e[u] > 0:
            if len(adjecency[u]) == current[u]:
                relabel(u)
                current[u] = 0
            else:
                v = adjecency[u][current[u]]

                if cf((u,v)) > 0 and h[u] == h[v] + 1:
                    push((u,v))
                else:
                    current[u] += 1

    def highest_label_selection_rule_algo():
        iters = 0

        active = set()
        for i in range(n):
            if i == s or i == t:
                continue
            active.add(i)
            inactive = set()

        while not (len(active) == 0 or sum(map(len, buckets)) == 0):
            if iters % 25000 == 0:
                bfs_height(t)
                iters += 1

            bi = 0
            for i in reversed(range(len(buckets))):
                if len(buckets[i]) > 0:
                    bi = i
                    break

            val, u = max([(h[i],i) for i in active])

            old_height = h[u]
            discharge(u)

            if h[u] > old_height:
                for v in inactive:
                    if h[v] < n:
                        buckets[h[v]].add(v)
                        active = active.union(inactive)
                        inactive = set()

            active.remove(u)
            if h[u] < n:
                buckets[h[u]].remove(u)

            if h[u] < n:
                inactive.add(u)


    highest_label_selection_rule_algo()

    res = []
    for i, fi in enumerate(f):
        for j in fi.keys():
            if fi[j] > 0:
                res.append((i,j,fi[j]))


    print (n, e[t], len(res))
    for (i,j,v) in res:
        print (i,j,v)

highest_label_selection_rule()
