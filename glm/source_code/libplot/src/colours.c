/******************************************************************************
 *                                                                            *
 * colours.c                                                                  *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Agriculture and Environment                                  *
 *     The University of Western Australia                                    *
 *                                                                            *
 * Copyright 2013 - 2018 -  The University of Western Australia               *
 *                                                                            *
 *  This file is part of libplot - the plotting library used in GLM           *
 *                                                                            *
 *  libplot is free software: you can redistribute it and/or modify           *
 *  it under the terms of the GNU General Public License as published by      *
 *  the Free Software Foundation, either version 3 of the License, or         *
 *  (at your option) any later version.                                       *
 *                                                                            *
 *  libplot is distributed in the hope that it will be useful,                *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 *  GNU General Public License for more details.                              *
 *                                                                            *
 *  You should have received a copy of the GNU General Public License         *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *
 *                                                                            *
 ******************************************************************************/

#include <stdlib.h>
#include <gd.h>

#include <colours.h>

#if DO_XCMAPS_TOO
#include <X11/X.h>
#include <X11/Xlib.h>

/******************************************************************************/
extern Display *display;
extern Window window;
extern GC gc;
extern int Black;
#endif

static rgb_val _x_map[2048];
rgb_val _map[256];
int black, grey, white, red, green, blue;

//#define LIM (MAX_COL_VAL-1)
#define LIM MAX_COL_VAL

/******************************************************************************/
void make_colour_map(gdImagePtr im, int style)
{
    int i=0;
#if DO_XCMAPS_TOO
    Colormap cmap = DefaultColormap(display, DefaultScreen(display));
    XColor colour;
#endif

    /**************************************************************************
     * Make sure white is allocated as index 0 because idx 0 is the default   *
     * background colour. Also add it to our maps idx 0.                      *
     **************************************************************************/
    _map[0].r = 255; _map[0].g = 255; _map[0].b = 255;
    _map[0].count = 0;
    white = gdImageColorAllocate(im, 255, 255, 255);
    _map[0].col = white;

    /**************************************************************************
     * For styles 1 and 2 we create 1024 colours then select every 4th        *
     **************************************************************************/
    if ( style <= 4 ) {
        int r = 255, g = 0, b = 0;
        if ( style == 1 || style == 2 ) {
            r = 127;
            while (r < 255) {
                _x_map[i].r = r++;
                _x_map[i].g = g;
                _x_map[i].b = b;
                _x_map[i].count = 0;
                i++;
            }
            while (g < 255) {
                _x_map[i].r = r;
                _x_map[i].g = g++;
                _x_map[i].b = b;
                _x_map[i].count = 0;
                i++;
            }
            while (b < 255) {
                _x_map[i].r = r--;
                _x_map[i].g = g;
                _x_map[i].b = b++;
                _x_map[i].count = 0;
                i++;
            }
            while (g > 0) {
                _x_map[i].r = r;
                _x_map[i].g = g--;
                _x_map[i].b = b;
                _x_map[i].count = 0;
                i++;
            }
            while (b > 128) {
                _x_map[i].r = r;
                _x_map[i].g = g;
                _x_map[i].b = b--;
                _x_map[i].count = 0;
                i++;
            }
        } else {
            while (1) {
                _x_map[i].r = r;
                _x_map[i].g = g;
                _x_map[i].b = b;
                _x_map[i].count = 0;
                i++;

                if ( r == 255 && g < 255 ) g++;
                else if (r > 0 ) r--;
                else if (b < 255 ) b++;
                else if (b == 255 && g > 0) g--;
                else break;
            }
        }
        while (i < 1024) {
            _x_map[i].r = 0;
            _x_map[i].g = 0;
            _x_map[i].b = 0;
            _x_map[i].count = 0;
            i++;
        }

       /***********************************************************************
        * Since 0 is white, start from idx 1.                                 *
        ***********************************************************************/
        if ( style == 2 || style == 4 ) {
            for (i = 1; i <= LIM; i++) {
                _map[i] = _x_map[i*4];
                _map[i].col = gdImageColorAllocate(im, _map[i].r, _map[i].g, _map[i].b);
            }
        } else {
            for (i = 1; i <= LIM; i++) {
                _map[i] = _x_map[(LIM-i)*4];
                _map[i].col = gdImageColorAllocate(im, _map[i].r, _map[i].g, _map[i].b);
            }
        }
    }
    else if ( style == 5 ) {
        for (i = 1; i <= LIM; i++) {
            _map[i].r = i;
            _map[i].g = LIM - abs(i-128);
            _map[i].b = LIM-i;
            _map[i].count = 0;
            _map[i].col = gdImageColorAllocate(im, _map[i].r, _map[i].g, _map[i].b);
        }
    } else {
        for (i = 1; i <= LIM; i++) {
            _map[i].r = LIM-i;
            _map[i].g = LIM - abs(i-128);
            _map[i].b = i;
            _map[i].count = 0;
            _map[i].col = gdImageColorAllocate(im, _map[i].r, _map[i].g, _map[i].b);
        }
    }

#if DO_XCMAPS_TOO
    for (i = 0; i <= LIM; i++) {
        colour.red = _map[i].r<<8;
        colour.green = _map[i].g<<8;
        colour.blue = _map[i].b<<8;
        XAllocColor(display, cmap, &colour);
        _map[i].xcolour = colour.pixel;
    }
#endif

    grey = gdImageColorAllocate(im, 192, 192, 192);
    black = gdImageColorAllocate(im, 0, 0, 0);

    red   = gdImageColorAllocate(im, 255, 0, 0);
    green = gdImageColorAllocate(im, 0, 255, 0);
    blue  = gdImageColorAllocate(im, 0, 0, 255);

#if DEBUG
    for (i = 0; i <= LIM; i++) {
        fprintf(stderr, "colour #%03d = %03d (%3d,%3d,%3d)\n",
                            i, _map[i].col, _map[i].r, _map[i].g, _map[i].b);
    }
    fprintf(stderr, "colour white = %03d\n", white);
    fprintf(stderr, "colour grey  = %03d\n", grey);
    fprintf(stderr, "colour black = %03d\n", black);
    fprintf(stderr, "colour red   = %03d\n", red);
    fprintf(stderr, "colour green = %03d\n", green);
    fprintf(stderr, "colour blue  = %03d\n", blue);
#endif
}

/******************************************************************************/

/******************************************************************************/
void make_grey_map(gdImagePtr im)
{
    int i=0;

    for (i = 0; i < 256; i++) {
        _map[i].r = 255 - i;
        _map[i].g = 255 - i;
        _map[i].b = 255 - i;
        _map[i].count = 0;
        _map[i].col = gdImageColorAllocate(im, _map[i].r, _map[i].g, _map[i].b);
    }

    white = _map[0].col;
    grey = _map[192].col;
    black = _map[255].col;
}


/******************************************************************************/
void ShowColourMapH(gdImagePtr im, int h, int v)
{
    int i;
    for (i = 0; i <= LIM; i++)
        gdImageLine(im, h+i, v, h+i, v+10, _map[i].col);
}

/******************************************************************************/
void ShowColourMapV(gdImagePtr im, int h, int v)
{
    int i;

    v += LIM;
    for (i = 0; i <= LIM; i++) {
#if DO_XCMAPS_TOO
        XSetForeground(display, gc, _map[i].col);
        XDrawLine(display, window, gc, h, v-i, h+10, v-i);
#else
        gdImageLine(im, h, v-i, h+10, v-i, _map[i].col);
#endif
    }
#if DO_XCMAPS_TOO
    XSetForeground(display, gc, Black);
#endif
}
