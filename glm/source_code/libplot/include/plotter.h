/******************************************************************************
 *                                                                            *
 * plotter.h                                                                  *
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

#ifndef _PLOTTER_H_
#define _PLOTTER_H_

#define MAX_PLOTS  16

/* the C prototypes */
int init_plotter(int *maxx, int *maxy);
int init_plotter_max(int maxplots, int *maxx, int *maxy);
int create_plot(int posx, int posy, int maxx, int maxy, const char *title);
void show_h_line(int plot, AED_REAL y);
void set_plot_x_limits(int plot, double min, double max);
void set_plot_y_limits(int plot, double min, double max);
void set_plot_z_limits(int plot, double min, double max);
void set_plot_version(int plot, const char *vers);
void set_plot_varname(int plot, const char *varname);
void plot_value(int plot, double x, double y, double z);
void flush_plot(int plot);
void do_cleanup(int saveall);

#endif
