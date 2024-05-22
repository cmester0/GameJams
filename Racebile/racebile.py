from math import cos, sin, pi

from PIL import Image

# Setup
width = 2000
height = 2000

m = [[(0, 0, 0) for j in range(height)] for i in range(width)]

# Helper functions

def hex_coord(x,y):
    return (x * cos(pi/6), x * sin(pi/6) + y)

def draw_hex(m, xi, yi, scale, color):
    for k in range(-scale, scale+1):
        m[int(xi)+k][int(yi)+2*scale] = color
        m[int(xi)+k][int(yi)-2*scale] = color

    for k in range(-scale, scale+1+3):
        m[round(xi-scale-(k+scale)*cos(pi/3))][round(yi-2*scale+(k+scale)*sin(pi/3))] = color
        m[round(xi-scale-(k+scale)*cos(pi/3))][round(yi+2*scale-(k+scale)*sin(pi/3))] = color

        m[round(xi+scale+(k+scale)*cos(pi/3))][round(yi-2*scale+(k+scale)*sin(pi/3))] = color
        m[round(xi+scale+(k+scale)*cos(pi/3))][round(yi+2*scale-(k+scale)*sin(pi/3))] = color

def draw_filled_hex(m, xi, yi, scale, color):
    for k in range(-scale, scale+3):
        for i in range(round(xi-scale-(k+scale)*cos(pi/3)), round(xi+scale+(k+scale)*cos(pi/3))):
            m[i][round(yi-2*scale+(k+scale)*sin(pi/3)+1)] = color
            m[i][round(yi+2*scale-(k+scale)*sin(pi/3)-1)] = color

def draw_hex_dir(m, xi, yi, d, scale, color):
    for k in range(int(scale * 1.5)):
        rx,ry = cos(d*pi/3+pi/6)*k, sin(d*pi/3+pi/6)*k
        m[int(xi+rx)][int(yi+ry)] = color


game_map = [
    (0,0,1),
    (0,1,1),
    (0,2,1),
    (0,3,1),
    (0,4,0),
    (1,4,0),
    (2,4,5),
    (3,3,5),
    (4,2,4),
    (4,1,4),
    (4,0,4),
    (4,-1,4),
    (4,-2,4),
    (4,-3,4),
    (4,-4,3),
    (3,-4,2),
    (2,-3,2),
    (1,-2,2),
    (0,-1,1)]

# Game

for i in range(-10,10):
    for j in range(-10,10):
        scale = 10
        xi, yi = hex_coord(i, j)
        xi = width // 2 + xi * (scale * 4)
        yi = height // 2 + yi * (scale * 4)

        if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
            continue

        color = (255, 255, 255)
        draw_hex(m, xi, yi, scale, color)

for i,j,d in game_map:
    scale = 10
    xi, yi = hex_coord(i, j)
    xi = width // 2 + xi * (scale * 4)
    yi = height // 2 + yi * (scale * 4)

    if not (2 * scale <= xi < width-2 * scale and 2 * scale <= yi < width-2 * scale):
        continue

    # color = (255,255,0)
    color = (int(i / 100 * 255), int((j + 100) / 200) * 255, 255)

    draw_filled_hex(m, xi, yi, scale, color)

    draw_hex_dir(m, xi, yi, d, scale, (255,255,0))

# Print map

flat_m = [m[i][j] for j in reversed(range(height)) for i in range(width)]

img = Image.new('RGB', (width, height)) # width, height
img.putdata(flat_m)
img.save('map.png')
