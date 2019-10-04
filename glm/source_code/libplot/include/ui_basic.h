/******************************************************************************
 *                                                                            *
 * ui_basic.h                                                                 *
 *                                                                            *
 * Description :                                                              *
 *                                                                            *
 *   A simple GUI example. This is the generic header.                        *
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
 *                     -------------------------------                        *
 *                                                                            *
 *  Derived with permission from                                              *
 *                                                                            *
 * Copyright 2003 - Ambinet System                                            *
 *                                                                            *
 ******************************************************************************/

#ifndef _UI_BASIC_H_
#define _UI_BASIC_H_

#include <gd.h>

/******************************************************************************/
int InitUI(int *width, int *height);
int CleanupUI(void);

    /**********************************************************/
void FlushUI(void);
int CheckUI(void);
int DoUI(void);
void GetMouse(int *x, int *y);
char *DoSaveDialog(char *fname);

    /**********************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height);
void RenameControl(int itm_id, const char*title);
void DisableControl(int itm_id);
void EnableControl(int itm_id);

    /**********************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height);
void FlushPicture(gdImagePtr im, int item_id);

    /**********************************************************/
int NewEditTextItem(int left, int top, int width, int height, const char*text);
int NewTextItem(int left, int top, int width, int height, const char*text);

    /**********************************************************/
int create_menu(const char*title, const char *data);

#endif
