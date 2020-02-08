/******************************************************************************
 *                                                                            *
 * libplot.h                                                                  *
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
#ifndef _LIB_PLOT_H_
#define _LIB_PLOT_H_

#define LIB_PLOT_VERSION "1.0.23"

#define PF_TITLE  1
#define PF_LABEL  2

#ifdef _FORTRAN_SOURCE_
# if SINGLE
#   define AED_REAL real
# else
#   define AED_REAL double precision
#endif

  INTERFACE
     SUBROUTINE set_progname(name,len) BIND(C, name="set_progname_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: name(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE
     INTEGER FUNCTION init_plotter(maxx, maxy) BIND(C, name="init_plotter_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(inout) :: maxx, maxy
     END FUNCTION
     SUBROUTINE set_plot_font(which, size, font, len) BIND(C, name='set_plot_font_')
        USE ISO_C_BINDING
        CINTEGER,INTENT(in)   :: which
        CINTEGER,INTENT(in)   :: size
        CCHARACTER,INTENT(in) :: font(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE set_plot_font
     INTEGER FUNCTION create_plot(posx, posy, maxx, maxy, title, len) BIND(C, name="create_plot_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in)   :: posx, posy, maxx, maxy
        CCHARACTER,INTENT(in) :: title(*)
        CINTEGER,INTENT(in)   :: len
     END FUNCTION
     SUBROUTINE set_plot_x_label(label, len) BIND(C, name="set_plot_x_label_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: label(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE
     SUBROUTINE set_plot_y_label(label, len) BIND(C, name="set_plot_y_label_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: label(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE
     SUBROUTINE set_plot_z_label(label, len) BIND(C, name="set_plot_z_label_")
        USE ISO_C_BINDING
        CCHARACTER,INTENT(in) :: label(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE
     SUBROUTINE set_plot_x_limits(plot, min, max) BIND(C, name="set_plot_x_limits_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
        AED_REAL,INTENT(in) :: min, max
     END SUBROUTINE
     SUBROUTINE set_plot_y_limits(plot, min, max) BIND(C, name="set_plot_y_limits_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
        AED_REAL,INTENT(in) :: min, max
     END SUBROUTINE
     SUBROUTINE set_plot_z_limits(plot, min, max) BIND(C, name="set_plot_z_limits_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
        AED_REAL,INTENT(in) :: min, max
     END SUBROUTINE
     SUBROUTINE set_plot_version(plot, version, len) BIND(C, name="set_plot_version_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
        CCHARACTER,INTENT(in) :: version(*)
        CINTEGER,INTENT(in)   :: len
     END SUBROUTINE set_plot_version
     SUBROUTINE x_plot_value(plot, x, y, z) BIND(C, name="x_plot_value_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot, x
        AED_REAL,INTENT(in) :: y, z
     END SUBROUTINE
     SUBROUTINE plot_value(plot, x, y, z) BIND(C, name="plot_value_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
        AED_REAL,INTENT(in) :: x, y, z
     END SUBROUTINE
     SUBROUTINE flush_plot(plot) BIND(C, name="flush_plot_")
        USE ISO_C_BINDING
        CINTEGER,INTENT(in) :: plot
     END SUBROUTINE
     SUBROUTINE do_cleanup(saveall) BIND(C, name="do_cleanup_")
        USE ISO_C_BINDING
        CLOGICAL,INTENT(in) :: saveall
     END SUBROUTINE
  END INTERFACE

#else

   #if SINGLE
     #define AED_REAL float
   #else
     #define AED_REAL double
   #endif


   void set_progname(const char *name);
   void set_shortprogname(const char *name);
   void set_aboutmessage(const char *name);
   void init_plotter_no_gui(void);
   int init_plotter(int *maxx, int *maxy);
   int init_plotter_max(int max_plots, int *maxx, int *maxy);
   void set_plot_font(int which, int size, const char *font);
   int create_plot(int posx, int posy, int maxx, int maxy, const char *title);
   void show_h_line(int plot, AED_REAL y);
   void set_plot_x_label(int plot, const char *label);
   void set_plot_y_label(int plot, const char *label);
   void set_plot_z_label(int plot, const char *label);
   void set_plot_x_limits(int plot, double min, double max);
   void set_plot_y_limits(int plot, double min, double max);
   void set_plot_z_limits(int plot, double min, double max);
   void set_plot_version(int plot, const char *version);
   void set_plot_varname(int plot, const char *varname);
   void set_plot_x_step(int plot, AED_REAL xstep);
   void set_plot_y_step(int plot, AED_REAL ystep);
   void set_plot_z_step(int plot, AED_REAL zstep);
   void plot_value(int plot, double x, double y, double z);
#ifdef _WIN32
   char *ctime_r(const time_t *timep, char *buf);
#endif
   void flush_plot(int plot);
   void flush_all_plots(void);
   void save_all_plots_named(const char*name);
   void do_cleanup(int saveall);

#endif

#endif
