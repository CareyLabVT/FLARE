/******************************************************************************
 *                                                                            *
 * plotter.c                                                                  *
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
#include <math.h>

#include <time.h>

#include <gd.h>
#include <gdfonts.h>
#include <gdfontmb.h>
#include <gdfontl.h>

#include <colours.h>
#include <ui_basic.h>
#ifdef _WIN32
  #define snprintf _snprintf
#endif
#define DEBUG_VALS 0
#include <libplot.h>
#include <plotter.h>

//#define DEBUG 1

#ifndef M_PI_2
# define M_PI_2             1.57079632679489661923  /* pi/2 */
#endif

void save_all_plots(void);
void save_plot(int plot);


/******************************************************************************/
typedef struct _plot_ {
    int    plot_id;
    int    item_id;
    int    save_id;
    char  *title;
    gdImagePtr im;
    int    count;
    int    maxx, maxy, lasty;
    int    xstep, xposp;
    int    ystep, yposp;
    int    zstep, zposp;
    double lastx;
    double xmin,xmax,xscale;
    double ymin,ymax,yscale;
    double zmin,zmax,zscale;
    int    havex, havey, havez;
    char  *xname, *yname, *zname;
    double zzmin, zzmax;
    int    zinit;
    int   *data;
    const char *version;
} Plot;

static int max_plots = MAX_PLOTS;
static Plot *_plots = NULL;
static int last_plot = -1;
static int okItm = -1;
static int saveAllItm = -1;
static int saveAllIn1Itm = -1;
static int my_xdisp = 0;
static char *title_font = NULL;
static int title_size = 40;
static int tfu = 0, tfd = 0, lfu = 0, lfd = 0;
static char *label_font = NULL;
static int label_size = 16;
char *progname = "Plot Window";
char *short_progname = NULL, *about_message = NULL;


/******************************************************************************/
/* The fortran interfaces */
/******************************************************************************/
int init_plotter_(int *maxx, int *maxy);
/* the final int is the length of the title string that fortran adds */
int create_plot_(int *posx, int *posy, int *maxx, int *maxy, const char *title, int *sl);
void set_plot_x_limits_(int *plot, AED_REAL *min, AED_REAL *max);
void set_plot_y_limits_(int *plot, AED_REAL *min, AED_REAL *max);
void set_plot_z_limits_(int *plot, AED_REAL *min, AED_REAL *max);
void plot_value_(int *plot, AED_REAL *x, AED_REAL *y, AED_REAL *z);
void save_plot_(int *plot);
void flush_plot_(int *plot);
void do_cleanup_(int *saveall);
void set_plot_x_step_(int *plot, AED_REAL *xstep);
void set_plot_y_step_(int *plot, AED_REAL *ystep);
void set_plot_z_step_(int *plot, AED_REAL *zstep);

/******************************************************************************/
static void calendar_date(int julian, int *yyyy, int *mm, int *dd);


/******************************************************************************/
#ifdef _WIN32
char *strndup(const char *s, int len)
{
    size_t l = strlen(s);
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
void set_progname_(const char *n, int *len)
{
    char *s = malloc(*len+1);
    strncpy(s, n, *len);
    s[*len] = 0;
    progname = s;
}
/*----------------------------------------------------------------------------*/
void set_progname(const char *name)
{ progname = strdup(name); }
/*----------------------------------------------------------------------------*/

/******************************************************************************/
void set_shortprogname_(const char *n, int *len)
{
    char *s = malloc(*len+1);
    strncpy(s, n, *len);
    s[*len] = 0;
    short_progname = s;
}
/*----------------------------------------------------------------------------*/
void set_shortprogname(const char *name)
{ short_progname = strdup(name); }
/*----------------------------------------------------------------------------*/

/******************************************************************************/
void set_aboutmessage_(const char *n, int *len)
{
    char *s = malloc(*len+1);
    strncpy(s, n, *len);
    s[*len] = 0;
    about_message = s;
}
/*----------------------------------------------------------------------------*/
void set_aboutmessage(const char *name)
{ about_message = strdup(name); }
/*----------------------------------------------------------------------------*/

static int _s_maxx = 0;
/******************************************************************************/
int init_plotter_(int *maxx, int *maxy) { return init_plotter(maxx, maxy); }
/*----------------------------------------------------------------------------*/
void init_plotter_no_gui()
{
/*
    char buf[256];
    time_t t = time(NULL);
    ctime_r(&t, buf);
    printf("Started  @ %s\n", buf);
*/
    _plots = malloc(max_plots*sizeof(Plot));
}
/*----------------------------------------------------------------------------*/
int init_plotter(int *maxx, int *maxy)
{
#define pushButton        0

    init_plotter_no_gui();

    my_xdisp = 1;
    *maxx+=20; *maxy+=60;
    if ( InitUI(maxx, maxy) < 0 ) return -1;
    _s_maxx = *maxx;
    *maxx-=20; *maxy-=60;
    okItm = NewControl(pushButton, "Done", *maxx - 90, *maxy + 20, 80, 20);
    DisableControl(okItm);
    saveAllItm = NewControl(pushButton, "Save All", *maxx - 230, *maxy + 20, 80, 20);
    DisableControl(saveAllItm);
    saveAllIn1Itm = NewControl(pushButton, "Save All-in-1", *maxx - 360, *maxy + 20, 110, 20);
    DisableControl(saveAllIn1Itm);

    return 0;
}
int init_plotter_max(int _max_plots, int *maxx, int *maxy)
{
    if ( _max_plots > MAX_PLOTS )
        max_plots = _max_plots + 2;

    return init_plotter(maxx, maxy);
}

/******************************************************************************/
#define ALIGN_LEFT     0x001
#define ALIGN_RIGHT    0x002
#define ALIGN_CENTER   0x004

#define ALIGN_TOP      0x010
#define ALIGN_BOTTOM   0x020
#define ALIGN_MIDDLE   0x040

#define DRAW_UP        0x100

#define IS_TITLE       0x200



/*----------------------------------------------------------------------------*/
void drawText(gdImagePtr im, int left, int right, int top, int bottom,
                                                    int flags, const char *txt)
{
    int x = 0, y = 0, w, h, sz, u, d;
    char *f = NULL;
    gdFontPtr gf = NULL;
    int brect[8];

#if DEBUG
    gdImageLine(im, left, top,    right, top,    red);
    gdImageLine(im, left, bottom, right, bottom, red);
    gdImageLine(im, left,  top, left,  bottom, red);
    gdImageLine(im, right, top, right, bottom, red);
#endif

    if ( flags & IS_TITLE ) {
        f = title_font; sz = title_size;
        gf = gdFontGetMediumBold();
        u = tfu; d = tfd;
    } else {
        f = label_font; sz = label_size;
        gf = gdFontGetSmall();
        u = lfu; d = lfd;
    }

    if ( f != NULL ) {
        gdImageStringFT(NULL, &brect[0], black, f, sz, 0.0, 0, 0, (char*)txt);
        w = brect[2] - brect[0];
        h = brect[3] - brect[5];

        if ( flags & DRAW_UP ) {
            if ( flags & ALIGN_CENTER ) y = bottom - (bottom - top - w) / 2;
            if ( flags & ALIGN_LEFT )   y = bottom - brect[0];
            if ( flags & ALIGN_RIGHT )  y = top + w - brect[0];

            // The problem with using the first version is that not all strings
            // presented will have parts both above and below the 0 point. (eg "0.0" vs "Depth")
        #if 1
            if ( flags & ALIGN_TOP )    x = left - u;
            if ( flags & ALIGN_BOTTOM ) x = right - d;
        #else
            if ( flags & ALIGN_TOP )    x = left + h;
            if ( flags & ALIGN_BOTTOM ) x = right;
        #endif
            if ( flags & ALIGN_MIDDLE ) x = right - (right - left - h) / 2;

            gdImageStringFT(im, &brect[0], black, f, sz, M_PI_2, x, y, (char*)txt);
        } else {
            if ( flags & ALIGN_CENTER ) x = left + (right - left - w) / 2;
            if ( flags & ALIGN_LEFT )   x = left - brect[0];
            if ( flags & ALIGN_RIGHT )  x = right - w - brect[0];

        #if 1
            if ( flags & ALIGN_TOP )    y = top - u;
            if ( flags & ALIGN_BOTTOM ) y = bottom - d;
        #else
            if ( flags & ALIGN_TOP )    y = top + h;
            if ( flags & ALIGN_BOTTOM ) y = bottom;
        #endif
            if ( flags & ALIGN_MIDDLE ) y = top + (bottom - top - h) / 2;

            gdImageStringFT(im, &brect[0], black, f, sz, 0.0, x, y, (char*)txt);
        }
    } else {
        w = strlen(txt) * gf->w;
        h = gf->h;

        if ( flags & DRAW_UP ) {
            if ( flags & ALIGN_CENTER ) y = bottom - (bottom - top - w) / 2;
            if ( flags & ALIGN_LEFT )   y = bottom;
            if ( flags & ALIGN_RIGHT )  y = top + w;

            if ( flags & ALIGN_TOP )    x = left;
            if ( flags & ALIGN_BOTTOM ) x = right - h;
            if ( flags & ALIGN_MIDDLE ) x = right - (right - left - h) / 2;

            gdImageStringUp(im, gf, x, y, (unsigned char *)txt, black);
        } else {
            if ( flags & ALIGN_CENTER ) x = left + (right - left - w) / 2;
            if ( flags & ALIGN_LEFT )   x = left;
            if ( flags & ALIGN_RIGHT )  x = right - w;

            if ( flags & ALIGN_TOP )    y = top;
            if ( flags & ALIGN_BOTTOM ) y = bottom - h;
            if ( flags & ALIGN_MIDDLE ) y = top + (bottom - top - h) / 2;

            gdImageString(im, gf, x, y, (unsigned char *)txt, black);
        }
    }
}

/******************************************************************************/
static void font_metric(const char*f, int sz, int *u, int *d)
{
    int brect[8];
    gdImageStringFT(NULL, &brect[0], black, (char*)f, sz, 0.0, 0, 0, "Dgq");
    *u = brect[5]; *d = brect[3];
}

/******************************************************************************/
void set_plot_font_(int *which, int *size, const char *font, int *len)
{
#ifndef _WIN32

    if ( *len == 0 ) return;

    gdFTUseFontConfig(1);
    if ( *which == PF_TITLE ) {
        title_font = strndup(font, *len);
        if ( *size ) title_size = *size;
        font_metric(title_font, title_size, &tfu, &tfd);
    } else if ( *which == PF_LABEL ) {
        label_font = strndup(font, *len);
        if ( *size ) label_size = *size;
        font_metric(label_font, label_size, &lfu, &lfd);
    }
#endif
}

/******************************************************************************/
void set_plot_font(int which, int size, const char *font)
{
#ifndef _WIN32
    if ( font == NULL ) return;

    gdFTUseFontConfig(1);
    if ( which == PF_TITLE ) {
        title_font = strdup(font);
        if ( size ) title_size = size;
        font_metric(title_font, title_size, &tfu, &tfd);
    } else if ( which == PF_LABEL ) {
        label_font = strdup(font);
        if ( size ) label_size = size;
        font_metric(label_font, label_size, &lfu, &lfd);
    }
#endif
}

#if 0
/******************************************************************************/
static int gdStringWidth(const char *s, gdFontPtr f)
{
    return strlen(s) * (f->w);
}

/******************************************************************************/
static int gdStringWidthFT(const char *s, const char *f, double sz)
{
    int brect[8];
    gdImageStringFT(NULL, &brect[0], black, (char*)f, sz, 0.0, 0, 0, (char*)s);
    return brect[2] - brect[0];
}
#endif

/******************************************************************************/
int create_plot_(int *posx, int *posy, int *maxx, int *maxy, const char *title, int *sl)
{
    char *s = strndup(title, *sl);
    int ret;

    ret = create_plot(*posx, *posy, *maxx, *maxy, s);
    free(s);
    return ret;
}
/*----------------------------------------------------------------------------*/
int create_plot(int posx, int posy, int maxx, int maxy, const char *title)
{
    gdImagePtr im;
    int mx, my;
    int item_id, save_id;

    last_plot++;
    if ( last_plot >= max_plots ) return -1;

    mx = maxx+80; my = maxy+60;
    im = gdImageCreate(mx, my);
    make_colour_map(im, 1); // this is an attempt at what MatLab calls "Jet"

    ShowColourMapV(im, mx-46, 5);

    drawText(im, 20, maxx+20, 2, 20, IS_TITLE|ALIGN_TOP|ALIGN_CENTER, (char*)title);

    if ( my_xdisp ) {
        item_id = NewPicture(im, 0, posx, posy, mx, my);
        FlushPicture(im, item_id);

        save_id = NewControl(pushButton, "Save", posx + maxx - 0, posy + maxy + 65, 80, 20);
        DisableControl(save_id);

        _plots[last_plot].item_id = item_id;
        _plots[last_plot].save_id = save_id;
    }

    _plots[last_plot].plot_id = last_plot;
    _plots[last_plot].title = strdup(title);
    _plots[last_plot].im = im;
    _plots[last_plot].havex = 0;
    _plots[last_plot].havey = 0;
    _plots[last_plot].havez = 0;
    _plots[last_plot].maxx = maxx;
    _plots[last_plot].maxy = maxy;
    _plots[last_plot].xstep = 0;
    _plots[last_plot].ystep = 0;
    _plots[last_plot].zstep = 0;
    _plots[last_plot].count = 0;
    _plots[last_plot].xname = NULL;
    _plots[last_plot].yname = NULL;
    _plots[last_plot].zname = NULL;
    _plots[last_plot].zinit = 0;
    _plots[last_plot].version = NULL;

    return last_plot;
}


/******************************************************************************/
void set_plot_x_label_(int *plot, const char *label, int *sl)
{ char *s = strndup(label, *sl); set_plot_x_label(*plot, s); free(s); }
/*----------------------------------------------------------------------------*/
void set_plot_x_label(int plot, const char *label)
{
    if ( plot < 0 ) return;
    _plots[plot].havex = 1;
    _plots[plot].xname = strdup(label);
}


/******************************************************************************/
void set_plot_y_label_(int *plot, const char *label, int *sl)
{ char *s = strndup(label, *sl); set_plot_y_label(*plot, s); free(s); }
/*----------------------------------------------------------------------------*/
void set_plot_y_label(int plot, const char *label)
{
    if ( plot < 0 ) return;
    _plots[plot].havey = 1;
    _plots[plot].yname = strdup(label);
}


/******************************************************************************/
void set_plot_z_label_(int *plot, const char *label, int *sl)
{ char *s = strndup(label, *sl); set_plot_z_label(*plot, s); free(s); }
/*----------------------------------------------------------------------------*/
void set_plot_z_label(int plot, const char *label)
{
    if ( plot < 0 ) return;
    _plots[plot].havez = 1;
    _plots[plot].zname = strdup(label);
}


/******************************************************************************/
void show_l_line(int plot, AED_REAL y)
{
    int ypos;
    ypos = _plots[plot].maxy - ((y - _plots[plot].ymin) * _plots[plot].yscale);

    gdImageLine(_plots[plot].im, _plots[plot].xposp+2, ypos+20,
                                 _plots[plot].xposp+7, ypos+20, red);
}
/******************************************************************************/
void show_h_line(int plot, AED_REAL y)
{
    int ypos;
    ypos = _plots[plot].maxy - ((y - _plots[plot].ymin) * _plots[plot].yscale);

    gdImageLine(_plots[plot].im,                   19, ypos+20,
                                 _plots[plot].maxx+23, ypos+20, grey);
}


/******************************************************************************/
void set_plot_x_limits_(int *plot, AED_REAL *min, AED_REAL *max)
{ set_plot_x_limits(*plot, *min, *max); }
/*----------------------------------------------------------------------------*/
void set_plot_x_limits(int plot, double min, double max)
{
    char lab[20];
    int y,m,d;
    char *xname;

    if ( plot < 0 ) return;

    _plots[plot].xmin = min;
    _plots[plot].xmax = max;
    _plots[plot].xscale = _plots[plot].maxx / (max - min);
    _plots[plot].havex = 1;
#if DEBUG
//  if ( plot == 1 )
    fprintf(stderr, "plot %d xmin %8.2lf xmax %8.2lf xscale %8.2lf\n", plot,
                  _plots[plot].xmin, _plots[plot].xmax, _plots[plot].xscale);
#endif
    gdImageLine(_plots[plot].im,            19, _plots[plot].maxy+21,
                          _plots[plot].maxx+21, _plots[plot].maxy+21, black);

    if ( _plots[plot].xname != NULL ) xname = _plots[plot].xname;
    else                              xname = "Date";
    drawText(_plots[plot].im,
                    20, _plots[plot].maxx+20,
                    _plots[plot].maxy+22, _plots[plot].maxy+40,
                                         ALIGN_TOP|ALIGN_CENTER, (char*)xname);

    calendar_date(min, &y, &m, &d);
    snprintf(lab, 20, "%02d/%02d/%d", d, m, y);
    drawText(_plots[plot].im,
                    20, _plots[plot].maxx+20,
                    _plots[plot].maxy+22, _plots[plot].maxy+40,
                                     ALIGN_TOP|ALIGN_LEFT, (char*)lab);

    calendar_date(max, &y, &m, &d);
    snprintf(lab, 20, "%02d/%02d/%d", d, m, y);
    drawText(_plots[plot].im,
                    20, _plots[plot].maxx+20,
                    _plots[plot].maxy+22, _plots[plot].maxy+40,
                                     ALIGN_TOP|ALIGN_RIGHT, (char*)lab);
}


/******************************************************************************/
void set_plot_y_limits_(int *plot, AED_REAL *min, AED_REAL *max)
{ set_plot_y_limits(*plot, *min, *max); }
/*----------------------------------------------------------------------------*/
void set_plot_y_limits(int plot, double min, double max)
{
    char lab[20];
    double yabs;
    char *yname = NULL;

    if ( plot < 0 ) return;

    _plots[plot].ymin = min;
    _plots[plot].ymax = max;
    _plots[plot].yscale = _plots[plot].maxy / (max - min);
    _plots[plot].havey = 1;
#if DEBUG
    fprintf(stderr, "plot %d ymin %8.2lf ymax %8.2lf yscale %8.2lf\n", plot,
                  _plots[plot].ymin, _plots[plot].ymax, _plots[plot].yscale);
#endif
    gdImageLine(_plots[plot].im, 19, 19,
                                 19, _plots[plot].maxy+21, black);

    if ( _plots[plot].yname != NULL ) yname = _plots[plot].yname;
    else if ( _plots[plot].havez )    yname = "Height (m)";
    if ( yname != NULL )
        drawText(_plots[plot].im,
                     0, 17,
                    20, _plots[plot].maxy+20,
                                DRAW_UP|ALIGN_BOTTOM|ALIGN_CENTER, (char*)yname);

    //snprintf(lab, 20, "%.2lf", _plots[plot].ymax);
    yabs = fabs(_plots[plot].ymax);
    if ( ( yabs > 0. && yabs < 0.01 ) || yabs > 10000. )
        snprintf(lab, 20, "%.2le", _plots[plot].ymax);
    else
        snprintf(lab, 20, "%.2lf", _plots[plot].ymax);
    drawText(_plots[plot].im,
                     0, 17,
                    20, _plots[plot].maxy+20,
                                DRAW_UP|ALIGN_BOTTOM|ALIGN_RIGHT, (char*)lab);

    //snprintf(lab, 20, "%.2lf", _plots[plot].ymin);
    yabs = fabs(_plots[plot].ymin);
    if ( ( yabs > 0. && yabs < 0.01 ) || yabs > 10000. )
        snprintf(lab, 20, "%.2le", _plots[plot].ymin);
    else
        snprintf(lab, 20, "%.2lf", _plots[plot].ymin);
    drawText(_plots[plot].im,
                     0, 17,
                    20, _plots[plot].maxy+20,
                                DRAW_UP|ALIGN_BOTTOM|ALIGN_LEFT, (char*)lab);
}


/******************************************************************************/
void set_plot_z_limits_(int *plot, AED_REAL *min, AED_REAL *max)
{ set_plot_z_limits(*plot, *min, *max); }
/*----------------------------------------------------------------------------*/
void set_plot_z_limits(int plot, double min, double max)
{
    char lab[20];
    double zabs;
//  char *zname = NULL;

    if ( plot < 0 ) return;

    _plots[plot].zmin = min;
    _plots[plot].zmax = max;
    _plots[plot].zscale = MAX_COL_VAL / (max - min);
    _plots[plot].havez = 1;
#if DEBUG
    fprintf(stderr, "plot %d zmin %8.2lf zmax %8.2lf zscale %8.2lf\n", plot+1,
                  _plots[plot].zmin, _plots[plot].zmax, _plots[plot].zscale);
#endif

/*
    snprintf(lab, 20, "%.2lf", _plots[plot].zmax);
    gdImageString(_plots[plot].im, gdFontGetSmall(), _plots[plot].maxx+48,
                                               3, (unsigned char *)lab, black);
    snprintf(lab, 20, "%.2lf", _plots[plot].zmin);
    gdImageString(_plots[plot].im, gdFontGetSmall(), _plots[plot].maxx+48,
                            _plots[plot].maxy+42, (unsigned char *)lab, black);
*/
    if ( _plots[plot].zname != NULL )
        drawText(_plots[plot].im,
                    _plots[plot].maxx+45, _plots[plot].maxx+65,
                     8, _plots[plot].maxy+52,
                       DRAW_UP|ALIGN_TOP|ALIGN_CENTER, (char*)_plots[plot].zname);

    zabs = fabs(_plots[plot].zmax);
    if ( ( zabs > 0. && zabs < 0.01 ) || zabs > 10000. )
        snprintf(lab, 20, "%.2le", _plots[plot].zmax);
    else
        snprintf(lab, 20, "%.2lf", _plots[plot].zmax);
    drawText(_plots[plot].im,
                    _plots[plot].maxx+45, _plots[plot].maxx+65,
                     8, _plots[plot].maxy+52,
                                DRAW_UP|ALIGN_TOP|ALIGN_RIGHT, (char*)lab);

    zabs = fabs(_plots[plot].zmin);
    if ( ( zabs > 0. && zabs < 0.01 ) || zabs > 10000. )
        snprintf(lab, 20, "%.2le", _plots[plot].zmin);
    else
        snprintf(lab, 20, "%.2lf", _plots[plot].zmin);
    drawText(_plots[plot].im,
                    _plots[plot].maxx+45, _plots[plot].maxx+65,
                     8, MAX_COL_VAL-8,
                                DRAW_UP|ALIGN_TOP|ALIGN_LEFT, (char*)lab);
}


/******************************************************************************/
void set_plot_x_step_(int *plot, AED_REAL *xstep)
{ set_plot_x_step(*plot, *xstep); }
/*----------------------------------------------------------------------------*/
void set_plot_x_step(int plot, AED_REAL xstep)
{ _plots[plot].xstep = xstep * _plots[plot].xscale; }
/******************************************************************************/
void set_plot_y_step_(int *plot, AED_REAL *ystep)
{ set_plot_y_step(*plot, *ystep); }
/*----------------------------------------------------------------------------*/
void set_plot_y_step(int plot, AED_REAL ystep)
{ _plots[plot].ystep = ystep * _plots[plot].zscale; }
/******************************************************************************/
void set_plot_z_step_(int *plot, AED_REAL *zstep)
{ set_plot_z_step(*plot, *zstep); }
/*----------------------------------------------------------------------------*/
void set_plot_z_step(int plot, AED_REAL zstep)
{ _plots[plot].zstep = zstep * _plots[plot].zscale; }


/******************************************************************************/
void set_plot_version_(int *plot, const char *version, int *len)
{ char *s = strndup(version, *len); set_plot_version(*plot, s); free(s); }
/*----------------------------------------------------------------------------*/
void set_plot_version(int plot, const char *version)
{
    _plots[plot].version = strdup(version);
    gdImageString(_plots[plot].im, gdFontGetSmall(), 5, _plots[plot].maxy+44,
                                               (unsigned char *)version, grey);
}
/*----------------------------------------------------------------------------*/
void set_plot_varname(int plot, const char *varname)
{
    size_t w;

    w = strlen(varname) * (gdFontGetSmall()->w);
    gdImageString(_plots[plot].im, gdFontGetSmall(),
                                      _plots[plot].maxx + 25 - w,
                                      _plots[plot].maxy + 44,
                                               (unsigned char *)varname, grey);
}


/******************************************************************************/
void x_plot_value_(int *plot, int *x, AED_REAL *y, AED_REAL *z)
{ plot_value(*plot, *x, *y, *z); }
/*----------------------------------------------------------------------------*/
void plot_value_(int *plot, AED_REAL *x, AED_REAL *y, AED_REAL *z)
{ plot_value(*plot, *x, *y, *z); }
/*----------------------------------------------------------------------------*/
void plot_value(int plot, double x, double y, double z)
{
    int colour=0, xpos, xposp, ypos;

    if ( plot < 0 ) return;

#if DEBUG
//  if ( plot == 3 )
    fprintf(stderr,"   plot_value(plot,(%d) x(%f), y(%f), z(%f))\n", plot, x, y, z);
#endif
    xpos =                     ((x - _plots[plot].xmin) * _plots[plot].xscale) + 20;
    ypos = _plots[plot].maxy - ((y - _plots[plot].ymin) * _plots[plot].yscale) + 20;
    if ( _plots[plot].xstep != 0 ) xpos += _plots[plot].xstep;

    if ( _plots[plot].count == 0 ) {
        _plots[plot].lastx = x;
        _plots[plot].lasty = _plots[plot].maxy + 20;
        _plots[plot].xposp = 20; //xposp = xpos;
        xposp = 20;
    } else {
        if ( _plots[plot].lastx != x ) {
            if ( _plots[plot].havez ) _plots[plot].lasty = _plots[plot].maxy + 20;
            _plots[plot].xposp = ((_plots[plot].lastx - _plots[plot].xmin) * _plots[plot].xscale) + 20;
            if ( _plots[plot].xstep != 0 ) _plots[plot].xposp += _plots[plot].xstep;
        }
        xposp = _plots[plot].xposp;
    }

    if ( xpos <= xposp ) xpos = xposp + 1;

#if DEBUG
//  if ( plot == 3 )
    fprintf(stderr, "    xposp = %d xpos = %d ypos = %d xscale = %f count %d\n", xposp, xpos, ypos, _plots[plot].xscale, _plots[plot].count);
#endif

    if ( !_plots[plot].havez ) z = y;
    if ( _plots[plot].zinit ) {
        if (_plots[plot].zzmin > z) _plots[plot].zzmin = z;
        if (_plots[plot].zzmax < z) _plots[plot].zzmax = z;
    } else {
        _plots[plot].zzmin = z;
        _plots[plot].zzmax = z;
        _plots[plot].zinit = 1;
    }
    if ( ! isnan(z) ) {
        if ( _plots[plot].havez ) {
            colour = (z - _plots[plot].zmin) * _plots[plot].zscale;
#if DEBUG_VALS
            if (z < _plots[plot].zmin || z > _plots[plot].zmax)
                fprintf(stderr, "plot %02d %8.6f < %8.6f < %8.6f ?\n",
                        plot, _plots[plot].zmin, z, _plots[plot].zmax);
#endif
            // Index 0 is white so we really want to be above that
            if (colour < 1) colour = 1;
            if (colour > MAX_COL_VAL) colour = MAX_COL_VAL;

            if ( _plots[plot].lastx != x ) _plots[plot].lasty = _plots[plot].maxy + 20;

            if ( colour < 0 || colour > MAX_COL_VAL ) {
                /* should now never happen because of the bounds check above */
                gdImageFilledRectangle(_plots[plot].im, xposp, ypos, xpos, _plots[plot].lasty, black);
                fprintf(stderr, "plot %d z value out of bounds (%8.6f)\n", plot, z);
            } else
                gdImageFilledRectangle(_plots[plot].im, xposp, ypos, xpos, _plots[plot].lasty, _map[colour].col);
        } else {
            colour = black;
            if ( _plots[plot].count == 0 ) {
                _plots[plot].lastx = x;
                _plots[plot].lasty = ypos;
            } else
                gdImageLine(_plots[plot].im, xposp, _plots[plot].lasty, xpos, ypos, black);
        }
    }

    _plots[plot].count++;
    _plots[plot].lastx = x;
    _plots[plot].lasty = ypos;

#if DEBUG
    fprintf(stderr, "plot %d xpos %d (%8.2lf) ypos %d (%8.2lf %8.2lf %8.2lf) colour %d (%8.2lf)\n",
                 plot, xpos, x, ypos, y, _plots[plot].ymin, _plots[plot].yscale, colour, z);
#endif
}


/******************************************************************************/
void flush_all_plots()
{
    int plot;

    for (plot = 0; plot <= last_plot; plot++)
        FlushPicture(_plots[plot].im, _plots[plot].item_id);
    CheckUI();
}

/******************************************************************************/
void flush_plot_(int *plot) { flush_plot(*plot); }
/*----------------------------------------------------------------------------*/
void flush_plot(int plot)
{
    CheckUI();

    if ( plot < 0 ) return;

    FlushPicture(_plots[plot].im, _plots[plot].item_id);
}


/******************************************************************************/
void save_all_plots_named(const char*name)
{
    FILE *fout;
    gdImagePtr im;
    int w, h, acrs, i;
    int plot_width, plot_height;
    int img_w, img_h;
    char *fname = NULL;

    plot_width = _plots[0].maxx + 80;
    plot_height = _plots[0].maxy + 60;

    acrs = (_s_maxx + 10) / (20 + plot_width);
    if (acrs == 0) acrs = 3;

    img_w = acrs * (plot_width+20);
    img_h = ((last_plot+acrs) / acrs) * (plot_height+20);

    im = gdImageCreate(img_w, img_h);
    make_colour_map(im, 1); // this is an attempt at what MatLab calls "Jet"

    w = 10;
    h = 10;
    for (i = 0; i <= last_plot; i++) {
        gdImageCopy(im, _plots[i].im, w, h, 0, 0, plot_width, plot_height);
        gdImageRectangle(im, w-1, h-1, w+plot_width+1, h+plot_height+1, black);

        w += plot_width + 20;
        if ( ((i+1) % acrs) == 0 ) {
            w = 10;
            h += plot_height + 20;
        }
    }

    /* Write PNG */
    if (name == NULL) {
        fname = malloc(strlen(progname)+12);
        strcpy(fname, progname);
        strcat(fname, "_Plots.png");
    } else
        fname = (char*)name;
    fout = fopen(fname, "wb");
    gdImagePng(im, fout);
    fclose(fout);
    if (name == NULL) free(fname);

    gdImageDestroy(im);
}
/*----------------------------------------------------------------------------*/
void save_all_plots_() { save_all_plots_named(NULL); }
/*----------------------------------------------------------------------------*/
void save_all_plots() { save_all_plots_named(NULL); }


/******************************************************************************/
void save_plot_(int *plot) { save_plot(*plot); }
/*----------------------------------------------------------------------------*/
void save_plot(int plot)
{
    FILE *fout;
    char *pname = malloc(strlen(_plots[plot].title) + 6);

    strcpy(pname, _plots[plot].title);

    if ( pname != NULL ) {
        strcat(pname, ".png");
        fout = fopen(pname, "wb");
        /* Write PNG */
        gdImagePng(_plots[plot].im, fout);
        fclose(fout);
        free(pname);
    }
}


/******************************************************************************/
void do_cleanup_(int *saveall) { do_cleanup(*saveall); }
/*----------------------------------------------------------------------------*/
void do_cleanup(int saveall)
{
    int i;
    int hit;
/*
    char buf[256];
    time_t t = time(NULL);
    ctime_r(&t, buf);
    printf("Finished @ %s\n", buf);
*/
    for (i = 0; i <= last_plot; i++) {
        if ( _plots[i].havez )
            printf("plot %d : zmin = %8.2le ; zmax = %8.2le (supplied %8.2le ; %8.2le) %s\n",
                                   i, _plots[i].zzmin, _plots[i].zzmax,
                                      _plots[i].zmin,  _plots[i].zmax,
                                      _plots[i].title);
        else
            printf("plot %d : ymin = %8.2le ; ymax = %8.2le (supplied %8.2le ; %8.2le) %s\n",
                                   i, _plots[i].zzmin, _plots[i].zzmax,
                                      _plots[i].ymin,  _plots[i].ymax,
                                      _plots[i].title);
    }

    if ( saveall ) {
        if ( saveall > 1 ) {
            save_all_plots();
            return;
        }
        for (i = 0; i <= last_plot; i++) {
            save_plot(i);
        }
        if ( my_xdisp ) CleanupUI();
        return;
    }

    if ( !my_xdisp ) return;

    EnableControl(okItm);
    EnableControl(saveAllItm);
    EnableControl(saveAllIn1Itm);

    /* Enable save items for each plot */
    for (i = 0; i <= last_plot; i++) {
        if ( _plots[i].save_id > 0 )
            EnableControl(_plots[i].save_id);
    }

    while ( (hit = DoUI()) >= 0 ) {
        if ( hit == okItm ) break;
        if ( hit == saveAllIn1Itm ) {
            save_all_plots();
            DisableControl(saveAllIn1Itm);
        } else {
            for (i = 0; i <= last_plot; i++) {
                if ( hit == saveAllItm || hit == _plots[i].save_id ) {
                    save_plot(i);
                    DisableControl(_plots[i].save_id);
                    if ( hit != saveAllItm ) break;
                }
                if ( hit == saveAllItm )
                    DisableControl(saveAllItm);
            }
        }
    }
    CleanupUI();
}


/******************************************************************************
 *                                                                            *
 *  Convert a true Julian day to a calendar date --- year, month and day.     *
 *                                                                            *
 ******************************************************************************/
static void calendar_date(int julian, int *yyyy, int *mm, int *dd)
{
    int j = julian;
    int y, m, d;

    j = j - 1721119 ;
    y = (4 * j - 1) / 146097;

    j = 4 * j - 1 - 146097 * y;
    d = j / 4;
    j = (4 * d + 3) / 1461;

    d = 4 * d + 3 - 1461 * j;
    d = (d + 4) / 4 ;
    m = (5 * d - 3) / 153;

    d = 5 * d - 3 - 153 * m;
    d = (d + 5) / 5 ;
    y = 100 * y + j ;

    if (m < 10)
        m = m + 3;
    else {
        m = m - 9;
        y = y + 1;
    }
    *yyyy = y;
    *mm = m;
    *dd = d;
}
