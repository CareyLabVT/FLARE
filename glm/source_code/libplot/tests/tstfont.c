/******************************************************************************
 *                                                                            *
 * tstfont.c                                                                  *
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
#include <string.h>

#include <time.h>

#include <gd.h>
#include <gdfonts.h>
#include <gdfontmb.h>
#include <gdfontl.h>

#include <colours.h>

#ifdef _WIN32
 #define snprintf _snprintf
#endif
#include <ui_basic.h>

#define pushButton        0

char *progname = NULL;

int tst_font(gdImagePtr im, const char*fl, double sz);

/******************************************************************************/
void usage()
{
    fprintf(stderr, "tstfont [-s <size>] [fontname]\n");
    fprintf(stderr, "  -s <size> lets you select a font point size to display\n");
    fprintf(stderr, "  fontname is the name of a font to display\n");
    fprintf(stderr, "  if no fontname is given a selection of fonts is displayed\n");
}


/******************************************************************************/
#ifdef _WIN32
char *strndup(const char *s, int len)
{
    int l = strlen(s);
    char *t = malloc(min(l,len)+1);
    if ( t != NULL ) { strncpy(t,s,min(l,len)+1); t[min(l,len)] = 0; }
    return t;
}
char *ctime_r(const time_t *timep, char *buf)
{
    char *s = ctime(timep);
    strcpy(buf,s);
    return buf;
}
#endif


/******************************************************************************/
int gdStringWidth(const char *s, gdFontPtr f)
{
    return strlen(s) * (f->w);
}

/******************************************************************************/
int main(int argc, char *argv[])
{
    gdImagePtr im;
    int maxx, maxy;
    int okItm, hit;
    char *fc = NULL;
    double sz = 24.;


    if (argc > 1) {
        progname = argv[0];
        argv++; argc--;
        while ( argc > 0 ) {
            if ( strcmp(argv[0], "-s") == 0 ) {
                argv++; argc--;
                if ( argc > 0 ) {
                    sz = atof(argv[0]);
                    argv++; argc--;
                } else break;
            } else {
                if ( fc == NULL )
                    fc = strdup(argv[0]);
                argv++; argc--;
            }
        }
    }

    maxx=420; maxy=360;
    InitUI(&maxx, &maxy);

    okItm = NewControl(pushButton, "Done", 310, 320, 80, 20);
    DisableControl(okItm);

    im = gdImageCreateTrueColor(400, 300);

    make_colour_map(im, 1);
    gdImageFilledRectangle(im, 0, 0, 400, 300, white);

    tst_font(im, fc, sz);

    NewPicture(im, 1, 10, 10, 400, 300);

    EnableControl(okItm);

    do  {
        hit = DoUI();
        fprintf(stderr, "hit = 0x%04X\n", hit);
    } while ( hit != okItm && hit != -1 );

    gdImageDestroy(im);

    CleanupUI();
    exit(0);
}



/******************************************************************************/
static int brect[8];

int show_font(gdImagePtr im, char *fc, double sz, int x, int y)
{
    char *err = gdImageStringFT(NULL, &brect[0], 0, fc, sz, 0.0, 0, 0, fc);
    y += (brect[3] - brect[5] + 6);
    err = gdImageStringFT(im, &brect[0], black, fc, sz, 0.0, x, y, fc);
    if (err) { fputs(err, stderr); return -1; }
    return y + 2;
}

/******************************************************************************/
int tst_font(gdImagePtr im, const char*fc, double sz)
{
    int x, y, i;
    char buf[1024];

    /* Signal that all freetype font calls in this program will receive
       fontconfig patterns rather than filenames of font files */
    gdFTUseFontConfig(1);

    x = 6; y = 6;
    if ( fc != NULL ) {
        y = show_font(im, (char*)fc, sz, x, y);

        for (i = 0; i < 8; i++) fprintf(stderr, "%d => %d\n", i, brect[i]);

        strcpy(buf, fc); strcat(buf, ":bold");        y = show_font(im, buf, sz, x, y);
        strcpy(buf, fc); strcat(buf, ":italic");      y = show_font(im, buf, sz, x, y);
        strcpy(buf, fc); strcat(buf, ":italic:bold"); y = show_font(im, buf, sz, x, y);
    } else {
        y = show_font(im, "Times", sz, x, y);
        y = show_font(im, "Palatino", sz, x, y);
        y = show_font(im, "Helvetica", sz, x, y);
        y = show_font(im, "AvantGarde", sz, x, y);
        y = show_font(im, "Monaco", sz, x, y);
    }

    return 0;
}
