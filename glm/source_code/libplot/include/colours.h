/******************************************************************************
 *                                                                            *
 * colours.h                                                                  *
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

#ifndef _COLOURS_H_
#define _COLOURS_H_

#define GRAYSCALE 0

typedef struct _rgb {
    int r;
    int g;
    int b;
    int col;
    unsigned long int xcolour;
    int count;
    } rgb_val;

extern rgb_val _map[256];

#define MAX_COL_VAL 250
extern int black, grey, white, red, green, blue;

#define JET 1

void make_colour_map(gdImagePtr im, int style);
void ShowColourMapH(gdImagePtr im, int h, int v);
void ShowColourMapV(gdImagePtr im, int h, int v);


#endif
