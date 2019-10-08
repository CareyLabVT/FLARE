/******************************************************************************
 *                                                                            *
 * tstmap.c                                                                   *
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
    char buf[256];
    gdImagePtr im;
    int i, h, maxx, maxy, mx;
    int item_id, okItm, hit;

    maxx=420; maxy=360;
    InitUI(&maxx, &maxy);

    okItm = NewControl(pushButton, "Done", 310, 320, 80, 20);
    DisableControl(okItm);

    mx = 10;
    im = gdImageCreateTrueColor(400, 300);

    for ( i = 1; i <= 6; i++ ) {
        make_colour_map(im, i);
        if ( i == 1 ) gdImageFilledRectangle(im, 0, 0, 400, 300, white);

        ShowColourMapV(im, mx, 5);

        sprintf(buf, "%02d", i);
        h = ((mx - gdStringWidth(buf, gdFontGetMediumBold())) / 2) + 10;
        gdImageString(im, gdFontGetMediumBold(), mx, 265, (unsigned char *)buf, black);

        mx += 40;
    }

    item_id = NewPicture(im, 1, 10, 10, 400, 300);

    EnableControl(okItm);

    do  {
        hit = DoUI();
        fprintf(stderr, "hit = 0x%04X\n", hit);
    } while ( hit != okItm && hit != -1 );

    gdImageDestroy(im);

    CleanupUI();
    exit(0);
}
