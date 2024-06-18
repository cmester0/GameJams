from math import cos, sin, pi, floor, ceil, inf
import colorsys

from PIL import Image
from PIL import GifImagePlugin
GifImagePlugin.LOADING_STRATEGY = GifImagePlugin.LoadingStrategy.RGB_AFTER_DIFFERENT_PALETTE_ONLY

from threading import Thread
import numpy as np

class DrawHexMap:
    def __init__(self, width, height, game_map):
        self.width = width
        self.height = height
        self.game_map = game_map

        self.scale = None
        self.cx = None
        self.cy = None
        self.compute_scale_and_center()

        self.frames = []
        self.m = np.array([[(0, 0, 0) for j in range(self.width)] for i in range(self.height)],dtype=np.uint8)
        self.pre_draw()
        self.m_init = np.array(self.m)

    def raw_hax_coord(self, x, y):
        xs, ys = 3 * cos(pi/3), sin(pi/3)
        xi, yi = (xs * x, ys * (x + y * 2))
        return xi, yi

    def hex_coord(self,x,y):
        xi,yi = self.raw_hax_coord(x,y)
        xi = self.width // 2 + (xi-self.cx) * self.scale
        yi = self.height // 2 + (yi-self.cy) * self.scale
        return xi, yi

    def draw_hex(self, xi, yi, color):
        for k in range(round(-self.scale*cos(pi/3)), round(self.scale*cos(pi/3))+1):
            self.m[round(xi+k)][round(yi+self.scale*sin(pi/3))] = color
            self.m[round(xi+k)][round(yi-self.scale*sin(pi/3))] = color

        for k in range(0, self.scale + 1):
            self.m[round(xi-self.scale*cos(pi/3)-k*cos(pi/3))][round(yi-self.scale*sin(pi/3)+k*sin(pi/3))] = color
            self.m[round(xi-self.scale*cos(pi/3)-k*cos(pi/3))][round(yi+self.scale*sin(pi/3)-k*sin(pi/3))] = color

            self.m[round(xi+self.scale*cos(pi/3)+k*cos(pi/3))][round(yi-self.scale*sin(pi/3)+k*sin(pi/3))] = color
            self.m[round(xi+self.scale*cos(pi/3)+k*cos(pi/3))][round(yi+self.scale*sin(pi/3)-k*sin(pi/3))] = color

    def draw_filled_small_hex(self, xi, yi, color):
        for k in range(1, self.scale // 2 + 1):
            for i in range(round(xi-self.scale // 2*cos(pi/3)-k*cos(pi/3))+1, round(xi+self.scale // 2*cos(pi/3)+k*cos(pi/3))-1 +1):
                self.m[i][round(yi-self.scale // 2*sin(pi/3)+k*sin(pi/3))] = color
                self.m[i][round(yi+self.scale // 2*sin(pi/3)-k*sin(pi/3))] = color

    def draw_filled_hex(self, xi, yi, color):
        for k in range(1, self.scale + 1):
            for i in range(round(xi-self.scale*cos(pi/3)-k*cos(pi/3))+1, round(xi+self.scale*cos(pi/3)+k*cos(pi/3))-1 +1):
                self.m[i][round(yi-self.scale*sin(pi/3)+k*sin(pi/3))] = color
                self.m[i][round(yi+self.scale*sin(pi/3)-k*sin(pi/3))] = color

    def draw_hex_dir(self, xi, yi, d, color):
        for k in range(int(self.scale*3/4)):
            rx,ry = cos(d*pi/3+pi/6)*k, sin(d*pi/3+pi/6)*k
            self.m[int(xi+rx)][int(yi+ry)] = color

            for i in range(2):
                self.m[int(xi+rx+i)][int(yi+ry)] = color
                self.m[int(xi+rx-i)][int(yi+ry)] = color

                self.m[int(xi+rx)][int(yi+ry+i)] = color
                self.m[int(xi+rx)][int(yi+ry-i)] = color

    def draw_circle(self, xi, yi, radius, color):
        for i in range(360):
            self.m[round(xi + radius * cos(i / 360 * 2 * pi))][round(yi + radius * sin(i / 360 * 2 * pi))] = color

    def pre_draw(self):
        raw_coords = [(xi, yi) for xi, yi in self.game_map]
        xm = list(map(lambda x: x[0], raw_coords))
        x_max, x_min = (max(xm), min(xm))

        ym = list(map(lambda x: x[1], raw_coords))
        y_max, y_min = (max(ym), min(ym))

        # Grid
        for i in range(x_min-1, x_max+1+1):
            for j in range(y_min-1, y_max+1+1):
                xi, yi = self.hex_coord(i, j)

                if not (self.scale <= xi < self.width-self.scale and self.scale <= yi < self.width-self.scale):
                    continue

                color = (255, 255, 255)
                self.draw_hex(xi, yi, color)

        # Map
        for (i,j) in self.game_map:
            dirs,t = self.game_map[(i,j)]

            xi, yi = self.hex_coord(i, j)

            if not (self.scale <= xi <= self.width-self.scale and self.scale <= yi <= self.height-self.scale):
                continue

            h = 210.6/360 if 2 in t else 51.1/360
            s = 1.0
            v = 0.5 if 1 in t else 0.75
            r, g, b = colorsys.hsv_to_rgb(h,s,v)

            color = (int(r * 255), int(g * 255), int(b * 255))

            self.draw_filled_hex(xi, yi, color)

            if 3 in t:
                h = 1.9/360
                s = 1.0
                v = 0.75
                r, g, b = colorsys.hsv_to_rgb(h,s,v)
                color = (int(r * 255), int(g * 255), int(b * 255))
                self.draw_filled_small_hex(xi, yi, color)

            for d in dirs:
                if 4 in t or 5 in t:
                    self.draw_hex_dir(xi, yi, d, (0,0,0))
                else:
                    self.draw_hex_dir(xi, yi, d, (255,255,255))

    def draw_map(self, players, player_steps, fell_off_map):
        # pre_draw(m,game_map, cx, cy, scale)
        self.m = np.array(self.m_init,dtype=np.uint8)

        pl, player_steps = player_steps
        for ps,(i,j,d) in enumerate(player_steps):
            xi, yi = self.hex_coord(i, j)

            if not (self.scale <= xi <= self.width-self.scale and self.scale <= yi <= self.width-self.scale):
                continue

            h = 5 * pl / 8 # len(players)
            s = ((ps+1) / (len(player_steps)+1))
            v = ((ps+1) / (len(player_steps)+1))
            r, g, b = colorsys.hsv_to_rgb(h,s,v)

            color = (int(r * 255), int(g * 255), int(b * 255))

            self.draw_circle(xi, yi, self.scale // 3 + 2 * pl, color)
            self.draw_circle(xi, yi, self.scale // 3 - 2 * pl, color)

            self.draw_circle(xi, yi, self.scale // 3 + 2 * pl + 1, color)
            self.draw_circle(xi, yi, self.scale // 3 - 2 * pl - 1, color)

            self.draw_hex_dir(xi, yi, d, scale, color)

        # Players
        for pl, ((i,j),d,_,_,_) in enumerate(players):
            xi, yi = self.hex_coord(i, j)

            if not (self.scale <= xi <= self.width-self.scale and self.scale <= yi <= self.width-self.scale):
                continue

            h = 5 * pl / 8 # len(players)
            s = 1.0
            v = 1.0
            r, g, b = colorsys.hsv_to_rgb(h,s,v)

            color = (int(r * 255), int(g * 255), int(b * 255))

            self.draw_circle(xi, yi, self.scale // 3 + 2 * pl, color)
            self.draw_circle(xi, yi, self.scale // 3 - 2 * pl, color)

            self.draw_circle(xi, yi, self.scale // 3 + 2 * pl + 1, color)
            self.draw_circle(xi, yi, self.scale // 3 - 2 * pl - 1, color)

            self.draw_hex_dir(xi, yi, d, self.scale, color)

        # Players
        fell_off_total = max([fell_off_map[(x,y)] for x, y in fell_off_map] + [1])
        for (x,y) in fell_off_map:
            h = 0
            s = 1
            v = fell_off_map[(x,y)]/(fell_off_total)
            r, g, b = colorsys.hsv_to_rgb(h,s,v)
            color = (int(r * 255), int(g * 255), int(b * 255))
            xi, yi = self.hex_coord(x, y)
            self.draw_filled_small_hex(xi, yi, color)

    def compute_scale_and_center(self):
        extra_x = 1
        extra_y = 0

        raw_coords = [self.raw_hax_coord(xi, yi) for xi, yi in self.game_map]
        xm = list(map(lambda x: x[0], raw_coords))
        x_max, x_min = (max(xm)+extra_x, min(xm)-extra_x)

        ym = list(map(lambda x: x[1], raw_coords))
        y_max, y_min = (max(ym)+extra_y, min(ym))

        self.cx = (x_max + x_min) // 2
        self.cy = (y_max + y_min) // 2

        self.scale = int(min(self.width / 2 / (x_max+2-self.cx), self.height / 2 / (y_max+2-self.cy)))

    def save_map(self, filename, players, player_steps, fell_off_map):
        self.draw_map(players, player_steps, fell_off_map)

        img = Image.fromarray(self.m, "RGB")
        img = img.rotate(90)

        t = Thread(target=lambda i,f: i.save(f), args=[img, filename])
        t.start()

        self.frames.append(img)

        return img
