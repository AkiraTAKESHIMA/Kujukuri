import os
import sys
import copy
import numpy as np
from numpy import sin, cos, tan, arctan2, sqrt, pi
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import json
import shapefile


d2r = pi / 180


def get_angle(x0, y0, x1, y1):
    return arctan2(y1-y0, x1-x0)


def get_directional_point(x, y, r, l):
    return x - l*cos(r), y - l*sin(r)


def get_ortho_points(x, y, w, r):
    x1 = x + w * cos(r+pi/2)
    y1 = y + w * sin(r+pi/2)
    x2 = x + w * cos(r-pi/2)
    y2 = y + w * sin(r-pi/2)
    return (x1, y1), (x2, y2)


def calc_intersection(x1, y1, r1, x2, y2, r2):
    xp = (cos(r1)*cos(r2)*(y1-y2) - cos(r2)*sin(r1)*x1 + cos(r1)*sin(r2)*x2) / sin(r2-r1)
    yp = (sin(r1)*sin(r2)*(x2-x1) + sin(r2)*cos(r1)*y1 - sin(r1)*cos(r2)*y2) / sin(r2-r1)

    return (xp, yp)


# Intersection of rank 1, 1
def makeChannelSurface_1_1(xya, wa, xyb, wb, xy, w):
    xak, yak = xya
    xbk, ybk = xyb
    x, y = xy

    ra = get_angle(x, y, xak, yak)
    rb = get_angle(x, y, xbk, ybk)

    (xak1, yak1), (xak2, yak2) = get_ortho_points(xak, yak, wa, ra+pi)
    (xbk1, ybk1), (xbk2, ybk2) = get_ortho_points(xbk, ybk, wb, rb)
    (xa1, ya1), (xa2, ya2) = get_ortho_points(x, y, w, ra+pi)
    (xb1, yb1), (xb2, yb2) = get_ortho_points(x, y, w, rb)

    ra1 = get_angle(xa1, ya1, xak1, yak1)
    ra2 = get_angle(xa2, ya2, xak2, yak2)
    rb1 = get_angle(xb1, yb1, xbk1, ybk1)
    rb2 = get_angle(xb2, yb2, xbk2, ybk2)

    xp1, yp1 = calc_intersection(xa1, ya1, ra1, xb1, yb1, rb1)
    xp2, yp2 = calc_intersection(xa2, ya2, ra2, xb2, yb2, rb2)


    # Plot
    figsize = (8,8)
    lw_in = 1
    lw_out = 2
    color_a = 'tomato'
    color_b = 'dodgerblue'

    # (1) Center lines
    fig1 = plt.figure(figsize=figsize)
    ax1 = fig1.add_subplot()
    ax1.set_aspect(1.0)

    # -- center lines
    ax1.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, zorder=2)
    ax1.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, zorder=2)

    # (2) Outlines, center lines and intersections
    fig2 = plt.figure(figsize=figsize)
    ax2 = fig2.add_subplot()
    ax2.set_aspect(1.0)

    # -- Center lines
    ax2.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, linestyle='dashed', zorder=2)
    ax2.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, linestyle='dashed', zorder=2)

    # -- Outlines
    ax2.plot((xak1, xa1, xa2, xak2, xak1), (yak1, ya1, ya2, yak2, yak1),
             linewidth=lw_in, color=color_a, zorder=3)
    ax2.plot((xbk1, xb1, xb2, xbk2, xbk1), (ybk1, yb1, yb2, ybk2, ybk1),
             linewidth=lw_in, color=color_b, zorder=3)

    # -- Intersections of outlines
    ax2.plot((xa1,xp1), (ya1,yp1), 
             linewidth=lw_in, linestyle='dashed', color=color_a, zorder=1)
    ax2.plot((xa2,xp2), (ya2,yp2), 
             linewidth=lw_in, linestyle='dashed', color=color_a, zorder=1)
    ax2.plot((xb1,xp1), (yb1,yp1), 
             linewidth=lw_in, linestyle='dashed', color=color_b, zorder=1)
    ax2.plot((xb2,xp2), (yb2,yp2), 
             linewidth=lw_in, linestyle='dashed', color=color_b, zorder=1)
    ax2.scatter((xp1,xp2), (yp1,yp2), 
                s=10, marker='o', color='k', zorder=5)

    # (3) Surface
    fig3 = plt.figure(figsize=figsize)
    ax3 = fig3.add_subplot()
    ax3.set_aspect(1.0)

    shpa = ((xak1, xp1, xp2, xak2, xak1), (yak1, yp1, yp2, yak2, yak1))
    shpb = ((xbk1, xp1, xp2, xbk2, xbk1), (ybk1, yp1, yp2, ybk2, ybk1))
    ax3.fill(shpa[0], shpa[1],
             facecolor=color_a, edgecolor='none', alpha=0.6, zorder=1)
    ax3.fill(shpb[0], shpb[1],
             facecolor=color_b, edgecolor='none', alpha=0.6, zorder=1)
    ax3.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, linestyle='dashed', zorder=2)
    ax3.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, linestyle='dashed', zorder=2)
    ax3.plot(shpa[0], shpa[1],
             color=color_a, linewidth=lw_out, zorder=2)
    ax3.plot(shpb[0], shpb[1],
             color=color_b, linewidth=lw_out, zorder=2)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax1.set_xlim(xmin, xmax)
    ax1.set_ylim(ymin, ymax)

    plt.show()


# Intersection of rank 1, 1, 2
# A: rank 1, width wa -> w
# B: rank 1, width wb -> w
# C: rank 2, width wc -> wcf
def makeChannelSurface_1_1_2(xya, wa, xyb, wb, xy, w, xyc, wc, wcf):
    x, y = xy
    xak, yak = xya
    xbk, ybk = xyb

    ra = get_angle(x, y, xak, yak)
    rb = get_angle(x, y, xbk, ybk)

    (xak1, yak1), (xak2, yak2) = get_ortho_points(xak, yak, wa, ra+pi)
    (xbk1, ybk1), (xbk2, ybk2) = get_ortho_points(xbk, ybk, wb, rb)
    (xa1, ya1), (xa2, ya2) = get_ortho_points(x, y, w, ra+pi)
    (xb1, yb1), (xb2, yb2) = get_ortho_points(x, y, w, rb)

    ra1 = get_angle(xa1, ya1, xak1, yak1)
    ra2 = get_angle(xa2, ya2, xak2, yak2)
    rb1 = get_angle(xb1, yb1, xbk1, ybk1)
    rb2 = get_angle(xb2, yb2, xbk2, ybk2)

    xp1, yp1 = calc_intersection(xa1, ya1, ra1, xb1, yb1, rb1)
    xp2, yp2 = calc_intersection(xa2, ya2, ra2, xb2, yb2, rb2)

    xck, yck = xyc
    rc = get_angle(x, y, xck, yck)
    (xck1, yck1), (xck2, yck2) = get_ortho_points(xck, yck, wc, rc)
    (xc1, yc1), (xc2, yc2) = get_ortho_points(x, y, wcf, rc)
    rc1 = get_angle(xc1, yc1, xck1, yck1)
    rc2 = get_angle(xc2, yc2, xck2, yck2)


    def get_q(x, y, r, xk, yk, xp, yp, xc, yc, rc, xck, yck):
        xq, yq = calc_intersection(x, y, r, xc, yc, rc)

        if min(xk,xp) <= xq <= max(xk,xp) and \
           min(xck,xc) <= xq <= max(xck,xc):
            if xq == xp:
                yq = yp

            return (xq, yq)


    def get_q_all(xc, yc, rc, xck, yck):
        lst_q = []
        for name, x, y, r, xk, yk, xp, yp in\
                (('a',xa1,ya1,ra1,xak1,yak1,xp1,yp1), 
                 ('b',xb1,yb1,rb1,xbk1,ybk1,xp1,yp1),
                 ('a',xa2,ya2,ra2,xak2,yak2,xp2,yp2), 
                 ('b',xb2,yb2,rb2,xbk2,ybk2,xp2,yp2)):
            q = get_q(x, y, r, xk, yk, xp, yp, xc, yc, rc, xck, yck)
            if q is not None:
                lst_q.append((name,q))

        if len(lst_q) == 0:
            raise Exception(f'len(lst_q) == 0')
        elif len(lst_q) == 1:
            name, q = lst_q[0]
        elif len(lst_q) == 2:
            if lst_q[0] == lst_q[1]:
                name, q = lst_q[0]
            else:
                raise Exception(\
                  f'lst_q[0]: {lst_q[0]}'+\
                  f'lst_q[1]: {lst_q[1]}')

        return name, q

    qname1, (xq1, yq1) = get_q_all(xc1, yc1, rc1, xck1, yck1)
    qname2, (xq2, yq2) = get_q_all(xc2, yc2, rc2, xck2, yck2)


    # Plot
    figsize = (8,8)
    lw_in = 1
    lw_out = 2
    color_a = 'tomato'
    color_b = 'dodgerblue'
    color_c = 'orange'

    # Figure (1) Only center lines
    fig1 = plt.figure(figsize=figsize)
    ax1 = fig1.add_subplot()
    ax1.set_aspect(1.0)

    # -- Center lines
    ax1.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, zorder=1)
    ax1.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, zorder=1)
    ax1.plot((xck, x), (yck, y),
             linewidth=lw_in, color=color_c, zorder=1)

    # (2) Outlines, center lines and intersections
    fig2 = plt.figure(figsize=figsize)
    ax2 = fig2.add_subplot()
    ax2.set_aspect(1.0)

    # -- Center lines
    ax2.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, linestyle='dashed', zorder=1)
    ax2.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, linestyle='dashed', zorder=1)
    ax2.plot((xck, x), (yck, y),
             linewidth=lw_in, color=color_c, linestyle='dashed', zorder=1)

    # -- Outlines
    ax2.plot((xak1, xa1, xa2, xak2, xak1), (yak1, ya1, ya2, yak2, yak1),
             linewidth=lw_in, color=color_a, zorder=2)
    ax2.plot((xbk1, xb1, xb2, xbk2, xbk1), (ybk1, yb1, yb2, ybk2, ybk1),
             linewidth=lw_in, color=color_b, zorder=2)
    ax2.plot((xck1, xc1, xc2, xck2, xck1), (yck1, yc1, yc2, yck2, yck1),
             linewidth=lw_in, color=color_c, zorder=2)

    # -- Intersections of outlines
    ls = dict(linewidth=lw_in, linestyle='dashed', zorder=1)
    ax2.plot((xa1,xp1), (ya1,yp1), 
             linewidth=lw_in, linestyle='dashed', color=color_a, zorder=1)
    ax2.plot((xa2,xp2), (ya2,yp2), 
             linewidth=lw_in, linestyle='dashed', color=color_a, zorder=1)
    ax2.plot((xb1,xp1), (yb1,yp1), 
             linewidth=lw_in, linestyle='dashed', color=color_b, zorder=1)
    ax2.plot((xb2,xp2), (yb2,yp2), 
             linewidth=lw_in, linestyle='dashed', color=color_b, zorder=1)
    ax2.scatter((xp1,xp2), (yp1,yp2), 
                s=30, marker='x', linewidth=0.5, color='k', zorder=3)
    ax2.scatter((xq1,xq2), (yq1,yq2), 
                s=30, marker='+', linewidth=0.5, color='k', zorder=3)

    # (3) Surface
    fig3 = plt.figure(figsize=figsize)
    ax3 = fig3.add_subplot()
    ax3.set_aspect(1.0)

    shpa = ((xak1, xp1, xp2, xak2, xak1), (yak1, yp1, yp2, yak2, yak1))
    shpb = ((xbk1, xp1, xp2, xbk2, xbk1), (ybk1, yp1, yp2, ybk2, ybk1))
    if qname1 == qname2:
        shpc = ((xck1, xq1, xq2, xck2, xck1), (yck1, yq1, yq2, yck2, yck1))
    else:
        shpc = ((xck1, xq1, xp1, xq2, xck2, xck1), (yck1, yq1, yp1, yq2, yck2, yck1))
    ax3.fill(shpa[0], shpa[1],
             facecolor=color_a, edgecolor='none', alpha=0.6, zorder=1)
    ax3.fill(shpb[0], shpb[1],
             facecolor=color_b, edgecolor='none', alpha=0.6, zorder=1)
    ax3.fill(shpc[0], shpc[1],
             facecolor=color_c, edgecolor='none', alpha=0.6, zorder=1)
    ax3.plot((xak, x), (yak, y),
             linewidth=lw_in, color=color_a, linestyle='dashed', zorder=2)
    ax3.plot((xbk, x), (ybk, y),
             linewidth=lw_in, color=color_b, linestyle='dashed', zorder=2)
    ax3.plot((xck, x), (yck, y),
             linewidth=lw_in, color=color_c, linestyle='dashed', zorder=2)
    ax3.plot(shpa[0], shpa[1],
             color=color_a, linewidth=lw_out, zorder=2)
    ax3.plot(shpb[0], shpb[1],
             color=color_b, linewidth=lw_out, zorder=2)
    ax3.plot(shpc[0], shpc[1],
             color=color_c, linewidth=lw_out, zorder=2)

    xmin, xmax, ymin, ymax = ax2.axis()
    ax1.set_xlim(xmin, xmax)
    ax1.set_ylim(ymin, ymax)


    plt.show()


xy = (1.0, 2.0)
w = 5.0
wa = 4.0
wb = 6.0
ra = -150
la = 20
rb = -10
lb = 20
xya = (xy[0]+la*cos(ra*d2r),xy[1]+la*sin(ra*d2r))
xyb = (xy[0]+lb*cos(rb*d2r),xy[1]+lb*sin(rb*d2r))
#makeChannelSurface_1_1(xya, wa, xyb, wb, xy, w)

wcf = 2.0
wc = 1.5
rc = 110
lc = 20
xyc = (xy[0]+lc*cos(rc*d2r),xy[1]+lc*sin(rc*d2r))
makeChannelSurface_1_1_2(xya, wa, xyb, wb, xy, w, xyc, wc, wcf)






