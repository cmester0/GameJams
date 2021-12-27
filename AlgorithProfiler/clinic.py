import bisect

def reverse_insort(a, x):
    lo = 0
    hi = len(a)
    while lo < hi:
        mid = (lo+hi)//2
        if x > a[mid]: hi = mid
        else: lo = mid+1
    a.insert(lo, x)
    
N, K = map(int, input().split())

patient_queue = []

def orderval (x):
    return (-(x[1] if K == 0 else x[1] / K - x[0]), x[2])

for _ in range(N):
    l = input().split()
    if l[0] == "1":
        T = int(l[1])
        M = l[2]
        S = int(l[3])

        ov = orderval((T,S,M))
        reverse_insort(patient_queue, ov) 
        
    elif l[0] == "2":
        T = int(l[1])

        if len(patient_queue) == 0:
            print ("doctor takes a break")
        else:
            _,f = patient_queue.pop()
            print (f)
        
    elif l[0] == "3":
        M = l[2]
        for index, item in patient_queue:
            if item[1] == M:
                del patient_queue[index]
                break


# 5 30
# 1 10 Aa 10
# 1 10 Al 11
# 2 20
# 2 25
# 2 30

# 5 5
# 1 10 Al 5
# 1 10 Aa 5
# 2 20
# 2 25
# 2 30

# 6 5
# 1 10 Clara 5
# 1 15 Bob 15
# 1 20 Alice 55
# 2 25
# 2 30
# 2 35

# 6 0
# 1 10 Clara 5
# 1 15 Bob 15
# 1 20 Alice 55
# 2 0
# 2 0
# 2 0

# 6 0
# 1 10 Clara 45
# 1 15 Bob 15
# 1 20 Alice 55
# 2 0
# 2 0
# 2 0

# 41 0
# 1 10 A 1
# 1 15 B 2
# 1 20 C 3
# 1 25 D 4
# 1 30 E 5
# 1 35 F 6
# 1 40 G 7
# 1 45 H 8
# 1 50 I 9
# 1 55 J 10
# 1 60 K 11
# 1 65 L 12
# 1 70 M 13
# 1 75 N 14
# 1 80 O 15
# 1 85 P 16
# 1 90 Q 17
# 1 95 R 18
# 1 100 S 19
# 1 105 T 20
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0

# 41 5
# 1 1 A 10
# 1 2 B 15
# 1 3 C 18
# 1 4 D 26
# 1 5 E 30
# 1 6 F 35
# 1 7 G 40
# 1 8 H 45
# 1 9 I 50
# 1 10 J 55
# 1 11 K 60
# 1 12 L 65
# 1 13 M 70
# 1 14 N 75
# 1 15 O 80
# 1 16 P 85
# 1 17 Q 90
# 1 18 R 95
# 1 19 S 100
# 1 20 T 105
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0



# 41 0
# 1 10 A 1
# 1 15 B 2
# 1 20 C 3
# 1 25 D 4
# 1 30 E 5
# 1 35 F 6
# 1 40 G 7
# 1 45 H 8
# 1 50 I 9
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 1 55 J 10
# 1 60 K 11
# 1 65 L 12
# 1 70 M 13
# 1 75 N 14
# 1 80 O 15
# 1 85 P 16
# 1 90 Q 17
# 1 95 R 18
# 1 100 S 19
# 1 105 T 20
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0
# 2 0


# k = 10 000 000
# 0 - 10 000 000 * 10 000 000 = 100 000 000 000 000
#                               214 748 364 7
#                               214 748 364 8
#                         9 223 372 036 854 775 808
