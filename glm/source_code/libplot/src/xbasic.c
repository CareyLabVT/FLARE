/******************************************************************************
 *                                                                            *
 * xbasic.c                                                                   *
 *                                                                            *
 *   A very basic X Windows example.                                          *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Agriculture and Environment                                  *
 *     The University of Western Australia                                    *
 *                                                                            *
 * Copyright 2013-2019 -  The University of Western Australia                 *
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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <ui_basic.h>

/******************************************************************************/
#define FONT_R  "9x15"
#define FONT_B  "7x13bold"
#define FONT_F  "fixed"

#define CTL_ITEM    1
#define PIC_ITEM    2
#define TXT_ITEM    3
#define EDT_ITEM    4
#define WIN_MENU    5
#define MNU_ITEM    6

/******************************************************************************/
#define pushButton        0
#define checkBox          1
#define radioButton       2
#define scrollBar        16

#define inButton         10
#define inCheckBox       11
#define inUpButton       20
#define inDownButton     21
#define inPageUp         22
#define inPageDown       23
#define inThumb         129

/******************************************************************************/
typedef struct _win_item {
    struct _win_item *next;
    int               id;
    int               type;
    int               left;
    int               top;
    int               right;
    int               bottom;
    void             *data;
} WindowItem;

/******************************************************************************/
typedef struct _win_rec {
    struct _win_rec *next;
    WindowItem      *itm_lst;
    Window           win;
    GC               gc;
    int              curEdit;
    int              mbarItm;
} WindowRecord, *WindowPtr;

/******************************************************************************/
typedef int (*ProcPtr)(void*ctl);

typedef struct _ctl_item {
    Window            owner;
    int               left;
    int               top;
    int               width;
    int               height;
    char              visible;
    unsigned char     hilite;
    long int          value;
    long int          min;
    long int          max;
    int               variant;
    ProcPtr           action;
    char             *title;
} Control;

/******************************************************************************/
typedef struct _pic_item {
    XImage           *image;
    unsigned char    *img;
    int               true_colour;
    int               left;
    int               top;
    int               width;
    int               height;
    int               offset_left;
    int               offset_top;
} PictureItem;

/******************************************************************************/
#define MENU_BAR_HEIGHT 20

#define _IN_MBAR_   1
#define _IN_MENU_   2

#define MENU_ITEM_HEIGHT 20

typedef struct _menu_info {
    int    menuID;
    int    nItems;
    int    h, tsize;
    int    last;
    int    width, height;
    char **items;
    char  *flags;
    char  *key;
    Window win;
    int    win_itm;
} Menu;

typedef struct _bar_item {
    int   nMenus;
    int   last;
    Menu *menus;
    Window win;
} MenuBar;


/******************************************************************************/
static Window _new_window(int left, int top,
                                          int width, int height, int transient);
static int _check_event(void);

static WindowPtr _find_window(Window win);

static void _dialog_key(Window win, char key);

static Control* _new_control(Window win,
                         int left, int top, int width, int height,
                         const char *title, char visible,
                         long int value, long int min, long int max,
                         int procID, long int refCon);
static int _point_in_ctl(Control * ctl, int h, int v);
static void _hilite_control(Control * ctl, int state);
static void _draw_control(Control * ctl);

static void _draw_picture(PictureItem *pic);

static void _draw_window_items(void);

static void _draw_mbar(MenuBar *mbar);
static void _draw_menu(Menu *menu);

/******************************************************************************/
static void _draw_string(int h, int v, Font font, const char *str);
static void _make_arcs(XArc *arcs, int left, int top, int right, int bottom,
                                                 int ovalWidth, int ovalHeight);

/******************************************************************************/
void _invert_rect(int left, int top, int width, int height);
void _frame_round_rect(int left, int top, int width, int height,
                                                 int ovalWidth, int ovalHeight);
void _invert_round_rect(int left, int top, int width, int height,
                                                 int ovalWidth, int ovalHeight);

/******************************************************************************/
int Alert(const char *message, const char *but1, const char *but2);
void About(const char *message);

extern char *short_progname, *about_message;
extern char *progname;
static Display *display = NULL;
static Window  _window = 0L;
static Window  _main_window = 0L;
static GC      _gc = 0L;

unsigned long int Black, White, Grey, LightGrey;
unsigned long int Red, Green, Blue;

static int screen = -1;
static Visual *visual;
static Font font_r, font_b, font_f;

static unsigned int  win_width,  win_height;
static unsigned int cwin_width, cwin_height;
static Colormap cmap;
static unsigned int display_width, display_height;

static int cur_x, cur_y;

static WindowPtr    _win_lst = NULL;

/******************************************************************************/
#define DEPTH 4
#define R_OFS 2
#define G_OFS 1
#define B_OFS 0

#define IMG_TOP 6
#define IMG_LFT 6

/******************************************************************************/
static int _new_top(int top)
{
    WindowPtr  wptr = _find_window(_window);
    if ( wptr->mbarItm != -1 )
        return top + MENU_BAR_HEIGHT;
    return top;
}

/******************************************************************************/
static int idhint = 1;
static int _add_item(int type, void *data,
                                       int left, int top, int width, int height)
{
    WindowItem *item = malloc(sizeof(WindowItem));
    WindowItem *ti = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return -1;
    ti = wptr->itm_lst;

    item->next = NULL;
    if ( ti == NULL )
        wptr->itm_lst = item;
    else {
        while ( ti->next )
            ti = ti->next;
        ti->next = item;
    }

    item->type = type;
    item->data = data;
    item->left = left;
    item->top = top;
    item->right = left+width;
    item->bottom = top+height;
    item->id = idhint++;

    return item->id;
}

/******************************************************************************/
static WindowItem *_find_item(int itm_id)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->id == itm_id )
            return item;
        item = item->next;
    }
    return NULL;
}

/******************************************************************************/
static WindowItem *_which_item(int x, int y)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( ( x >= item->left && x <= item->right ) &&
             ( y >= item->top  && y <= item->bottom ) )
            return item;
        item = item->next;
    }
    return NULL;
}

/******************************************************************************/
static WindowItem *_find_item_of_type(Window win, int type)
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(win);

    if ( wptr == NULL ) return NULL;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == type )
            return item;
        item = item->next;
    }

    return NULL;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static void _add_window(Window win, GC gc)
{
    WindowRecord *wrec = malloc(sizeof(WindowRecord));
    WindowRecord *tr = _win_lst;

    memset(wrec, 0, sizeof(WindowRecord));
    wrec->next = NULL;

    if ( tr == NULL )
        _win_lst = wrec;
    else {
        while ( tr->next )
            tr = tr->next;
        tr->next = wrec;
    }

    wrec->win = win;
    wrec->gc = gc;
    wrec->itm_lst = NULL;
    wrec->curEdit = -1;
    wrec->mbarItm = -1;
}

/******************************************************************************/
static WindowPtr _find_window(Window win)
{
    WindowPtr wptr = _win_lst;

    while ( wptr != NULL ) {
        if ( wptr->win == win )
            return wptr;
        wptr = wptr->next;
    }
    return NULL;
}

/******************************************************************************/
static void _set_window(Window win)
{
    WindowPtr wptr = _find_window(win);
    XWindowAttributes window_attributes;

    if ( wptr != NULL ) {
        _window  = wptr->win;
        _gc      = wptr->gc;

        XGetWindowAttributes(display, _window, &window_attributes);
        cwin_width = window_attributes.width;
        cwin_height= window_attributes.height;
    } else {
        _window  = 0;
        _gc      = 0;
        cwin_width = 0;
        cwin_height= 0;
    }
}

/******************************************************************************/
static void _delete_window(Window win)
{
    WindowPtr wptr = _win_lst;
    WindowPtr tptr = NULL;
    WindowItem *item = NULL;

    XDestroyWindow(display, win);

    if ( wptr == NULL ) return; /* cant be in t' list */

    if ( wptr->win == win ) {
        tptr = wptr;
        if ( wptr->next != NULL ) {
            _window  = (wptr->next)->win;
            _gc      = (wptr->next)->gc;
        } else {
            _window  = 0;
            _gc      = 0;
        }
        _win_lst = wptr->next;
    } else {
        while ( wptr->next != NULL ) {
            if ( (wptr->next)->win == win ) {
                tptr = wptr->next;
                wptr->next = (wptr->next)->next;
                _window  = wptr->win;
                _gc      = wptr->gc;
                break;
            }
            wptr = wptr->next;
        }
    }

    if ( tptr == NULL ) return;

    item = tptr->itm_lst;

    while ( item != NULL ) {
        if ( item->type == PIC_ITEM ) {
            PictureItem *pic = item->data;
            if ( pic->image != NULL )
                XDestroyImage(pic->image);
            free(item->data);
        }
        else if (item->type == CTL_ITEM ) {
            free(((Control*)(item->data))->title);
            free(item->data);
        }
        else if (item->type == TXT_ITEM ||
                 item->type == EDT_ITEM ) {
            free(item->data);
        }
        item = item->next;
    }
    free(tptr);
}

/******************************************************************************/
void _draw_picture(PictureItem *pic)
{
    XSetFunction(display, _gc, GXcopy);
    if ( pic->image != NULL )
        XPutImage(display, _window, _gc, pic->image, 0, 0,
                                    pic->left, pic->top, win_width, win_height);

    XDrawRectangle(display, _window, _gc, pic->left-1, pic->top-1,
                                                   pic->width+1, pic->height+1);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static Window _new_window(int left, int top,
                          int width, int height, int transient)
{
    XClassHint xch;
    XSizeHints xsh;
    XWMHints   xwmh;
    XSetWindowAttributes xswa;
    int mask;
    Window win;
    GC gc = 0L;

    xswa.background_pixel = White;
    xswa.border_pixel = Black;
    xswa.backing_store = WhenMapped;
    mask = CWBackPixel | CWBorderPixel | CWBackingStore;

//  if ( !transient && _mbar != -1 ) height += MENU_BAR_HEIGHT;

    win = XCreateWindow(display,
            (_window == 0L) ? RootWindow(display, screen) : _window,
                              left, top, width, height,
            (transient) ? 1 : 3,
                              CopyFromParent,
                              InputOutput, CopyFromParent, mask, &xswa);

    xsh.flags = PSize | PMinSize | PMaxSize;
    xsh.x = left;
    xsh.y = top;
    xsh.flags |= USPosition;
    xsh.width = xsh.min_width = xsh.max_width = width;
    xsh.height = xsh.min_height = xsh.max_height = height;
    xsh.max_height += MENU_BAR_HEIGHT;
    XSetNormalHints(display, win, &xsh);

    xch.res_name = progname;
    xch.res_class = "lock";
    XSetClassHint(display, win, &xch);

    XStoreName(display, win, progname);
    XSetIconName(display, win, progname);

    xwmh.flags = InputHint | StateHint ;
    xwmh.input = True;
    xwmh.initial_state = NormalState;
    XSetWMHints(display, win, &xwmh);

    if ( transient )
        XSetTransientForHint(display, win, win);

    gc = XCreateGC(display, win, 0, NULL);
    XCopyGC(display, DefaultGC(display, DefaultScreen(display)), -1, gc);

    XSelectInput(display, win,
        KeyPressMask | KeyReleaseMask | ExposureMask | PointerMotionMask |
        ButtonPressMask | ButtonReleaseMask | StructureNotifyMask);

    _add_window(win, gc);
    if ( ! transient ) {
        XMapWindow(display, win);
        _set_window(win);
    }

    return win;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void _show_position(int h, int v)
{
    char         buff[64];

    XClearArea(display, _window, h-10, v-15, 100, 20, False);
    XDrawRectangle(display, _window, _gc, h-10, v-15, 100, 20);
    sprintf(buff, "%4d,%-4d", cur_x, cur_y);
    _draw_string(h, v, font_r, buff);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static int _process_event(XEvent *ev)
{
    char         buf[26];
    KeySym       ks;
    Window       twin;
    int          len = 0, i;

    twin = _window;
    // remember to restore before leaving - at the moment the only "return"
    // is at the end of this routine.
    _set_window(ev->xany.window);
//  if ( _window != ev->xany.window )
//      return 0;

    switch (ev->type) {
        case KeymapNotify:
            XRefreshKeyboardMapping(&ev->xmapping);
            break;

        case KeyPress:
            break;
        case KeyRelease:
            len = XLookupString(&ev->xkey, buf, 25, &ks, NULL);
            cur_x = ev->xkey.x; cur_y = ev->xkey.y;
            for (i = 0; i < len; i++) _dialog_key(_window, buf[i]);
            break;

        case Expose:
            XClearArea(display, _window, 0, 0, cwin_width, cwin_height, False);
            if ( _main_window != _window )
                XDrawRectangle(display, _window, _gc, 1, 1, cwin_width-2, cwin_height-2);
            _draw_window_items();
            break;

        case MotionNotify:
            while (XCheckTypedEvent(display, MotionNotify, ev))
                ;

            cur_x = ev->xmotion.x; cur_y = ev->xmotion.y;
//          _show_position(win_width-120, 30);

            XFlush(display);
            break;

        case MapNotify:
        case UnmapNotify:
            break;

        case ButtonPress:
            cur_x = ev->xbutton.x; cur_y = ev->xbutton.y;
            break;
        case ButtonRelease:
            cur_x = ev->xbutton.x; cur_y = ev->xbutton.y;
            break;
/*
        default:
            printf("Unprocessed event %d\n", ev->type);
            break;
*/
    }
    _set_window(twin);
    return ev->type;
}

/******************************************************************************/
static Control *_trking_ctl = NULL;
static int _trking_in = 0;
static int _trking_itmid = 0;

/******************************************************************************/
static void _start_track_control(Control * ctl, int h, int v, ProcPtr action)
{
    _trking_ctl = ctl;
    _trking_in = True;
    _hilite_control(ctl, 254);
}

/******************************************************************************/
static int _check_track_control()
{
    XEvent ev;

    ev.type = 0;
    while ( XCheckMaskEvent(display, -1L, &ev) == True ) {
        if ( _process_event(&ev) != ButtonRelease ) {
            if ( _point_in_ctl(_trking_ctl, cur_x, cur_y) ) {
                if ( !_trking_in ) {
                    _hilite_control(_trking_ctl, 254);
                    _trking_in = True;
                }
            } else {
                if ( _trking_in ) {
                    _hilite_control(_trking_ctl, 0);
                    _trking_in = False;
                }
            }
            return 0;
        }
        else
            return 1;
    }
    return -1;
}

/******************************************************************************/
static int _finish_track_control()
{
    if ( _trking_in )
        _hilite_control(_trking_ctl, 0);
    _trking_ctl = NULL;

    return _trking_in;
}

/******************************************************************************
 *                                                                            *
 *                                                                            *
 ******************************************************************************/


/******************************************************************************/
Control * _new_control(Window win,
                int left, int top, int width, int height,
                const char *title, char visible,
                long int value, long int min, long int max,
                int procID, long int refCon)
{
    Control * ctl = malloc(sizeof(Control));

    ctl->owner = win;
    ctl->left = left;
    ctl->top = top;
    ctl->width = width;
    ctl->height = height;
    ctl->visible = visible;
    ctl->hilite = 0;
    ctl->value = value;
    ctl->min = min;
    ctl->max = max;
    ctl->variant = procID;
    ctl->action = NULL;
    ctl->title = strdup(title);

    return ctl;
}

/******************************************************************************/
static void _set_ctl_state(int itm_id, unsigned char state)
{
    Control *ctl = NULL;
    WindowItem *item = _find_item(itm_id);

    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( (ctl = item->data) == NULL ) return;

    ctl->hilite = state;
    _draw_control(ctl);
}

/******************************************************************************/
int _point_in_ctl(Control * ctl, int h, int v)
{
    /* cant be in disabled controls */
    if ( ctl->hilite == 255 ) return False;

    if ( h < ctl->left || v < ctl->top ||
         h > ctl->left+ctl->width ||
         v > ctl->top+ctl->height )
        return False;
    return True;
}

/******************************************************************************/
void _draw_string(int h, int v, Font font, const char *str)
{
    XTextItem ti;

    ti.font = font;
    ti.chars = (char*)str;
    ti.nchars = strlen(str);
    ti.delta = 0;
    XDrawText(display, _window, _gc, h, v, &ti, 1);
}

/******************************************************************************/
void _hilite_control(Control *ctl, int state)
{
    ctl->hilite = state;
    _draw_control(ctl);
}

#define A_UP 0
#define A_DN 1
#define A_LF 2
#define A_RT 3

/******************************************************************************/
void _arrow_box(int left, int top, int right, int bottom, int dir, int hilite)
{
    int hcentre, vcentre;
    XPoint points[8];

    hcentre = (left + right)/2;
    vcentre = (top + bottom)/2;

    XDrawRectangle(display, _window, _gc, left, top, right-left, bottom-top);

    switch ( dir ) {
        case A_UP:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = right;   points[2].y = vcentre;
            points[3].x = right-5; points[3].y = vcentre;
            points[4].x = right-5; points[4].y = bottom;
            points[5].x = left+5;  points[5].y = bottom;
            points[6].x = left+5;  points[6].y = vcentre;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_DN:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = bottom;
            points[2].x = right;   points[2].y = vcentre;
            points[3].x = right-5; points[3].y = vcentre;
            points[4].x = right-5; points[4].y = top;
            points[5].x = left+5;  points[5].y = top;
            points[6].x = left+5;  points[6].y = vcentre;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_LF:
            points[0].x = left;    points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = hcentre; points[2].y = top+5;
            points[3].x = right;   points[3].y = top+5;
            points[4].x = right;   points[4].y = bottom-5;
            points[5].x = hcentre; points[5].y = bottom-5;
            points[6].x = hcentre; points[6].y = bottom;
            points[7].x = left;    points[7].y = vcentre;
            break;
        case A_RT:
            points[0].x = right;   points[0].y = vcentre;
            points[1].x = hcentre; points[1].y = top;
            points[2].x = hcentre; points[2].y = top+5;
            points[3].x = left;    points[3].y = top+5;
            points[4].x = left;    points[4].y = bottom-5;
            points[5].x = hcentre; points[5].y = bottom-5;
            points[6].x = hcentre; points[6].y = bottom;
            points[7].x = right;   points[7].y = vcentre;
            break;
        default:
            return;
    }

    XDrawLines(display, _window, _gc, points, 8, CoordModeOrigin);
    if ( hilite )
        XFillPolygon(display, _window, _gc, points, 8, Convex, CoordModeOrigin);
}

/******************************************************************************/
void _draw_control(Control * ctl)
{
    int str_length, title_length;
    int left, top, width, height, right, bottom;

    if ( ctl == NULL || ctl->visible == 0 )
        return;

    left = ctl->left; width = ctl->width;
    top = ctl->top; height = ctl->height;
    right = left + width;
    bottom = top + height;

    XClearArea(display, _window, left, top, width, height, False);

    switch (ctl->variant) {
        case pushButton :
            _frame_round_rect(left, top, width, height, 16, 16);

            str_length = strlen(ctl->title);
            title_length = XTextWidth(XQueryFont(display, font_b),
                                                        ctl->title, str_length);
            // this is how to grey out a control
            if ( ctl->hilite == 255 ) XSetForeground(display, _gc, Grey);
            _draw_string(left+((width-title_length)/2), bottom-5,
                                                            font_b, ctl->title);
            XSetForeground(display, _gc, Black);

            if (ctl->hilite && ctl->hilite != 255 )
                _invert_round_rect(left, top, width, height, 16, 16);
            break;
        case checkBox :
        case radioButton :
            if (ctl->variant == checkBox) {
                XDrawRectangle(display, _window, _gc, left, top, 15, 15);
                if (ctl->value) {
                   XDrawLine(display, _window, _gc, left, top, left+15, top+15);
                   XDrawLine(display, _window, _gc, left, top+15, left+15, top);
                }
            } else {
                XDrawArc(display, _window, _gc, left, top, width, height,
                                                                     0, 360*64);

                if (ctl->value) {
                    XSetFunction(display, _gc, GXnor);
                    XDrawArc(display, _window, _gc, left+3, top+3,
                                                  width-6, height-6, 0, 360*64);
                    XSetFunction(display, _gc, GXcopy);
                }
            }
            if ( ctl->hilite == 255 ) XSetForeground(display, _gc, Grey);
            _draw_string(left+20, top+12, font_r, ctl->title);
            XSetForeground(display, _gc, Black);

            break;
        case scrollBar :
            XDrawRectangle(display, _window, _gc, left, top, width, height);
            if ( height > width ) {
                _arrow_box(left, top, right, top+width, A_UP, ctl->hilite);
                _arrow_box(left, bottom-width, right,bottom, A_DN, ctl->hilite);
            } else {
                _arrow_box(left, top, left+height, bottom, A_LF, ctl->hilite);
                _arrow_box(right-height, top, right, bottom, A_RT, ctl->hilite);
            }
            break;
    }
}

/******************************************************************************/
static void _draw_window_items()
{
    WindowItem *item = NULL;
    WindowPtr  wptr = _find_window(_window);

    if ( wptr == NULL ) return;
    item = wptr->itm_lst;

    while ( item != NULL ) {
        switch ( item->type ) {
            case CTL_ITEM: _draw_control(item->data); break;
            case PIC_ITEM: _draw_picture(item->data); break;
            case TXT_ITEM:
            case EDT_ITEM:
                _draw_string(item->left+6, item->top+15,
                            (item->type == TXT_ITEM)?font_b:font_r, item->data);
                if ( item->type == EDT_ITEM )
                    XDrawRectangle(display, _window, _gc,
                            item->left, item->top,
                            item->right-item->left+1, item->bottom-item->top+1);
                break;
            case WIN_MENU: _draw_mbar(item->data); break;
            case MNU_ITEM: _draw_menu(item->data); break;
            default:
                break;
        }
        item = item->next;
    }
}

/******************************************************************************/
static void _make_arcs(XArc *arcs, int left, int top, int right, int bottom,
                                                  int ovalWidth, int ovalHeight)
{
    int i;
    for (i=0; i<4; i++) {
        arcs[i].width = ovalWidth;
        arcs[i].height = ovalHeight;
        arcs[i].angle1 = (90*64)*i;
        arcs[i].angle2 = 90*64;
    }

    arcs[0].x = right-ovalWidth; arcs[0].y = top,
    arcs[1].x = left;            arcs[1].y = top,
    arcs[2].x = left;            arcs[2].y = bottom-ovalHeight;
    arcs[3].x = right-ovalWidth; arcs[3].y = bottom-ovalHeight;
}

/******************************************************************************/
void _invert_rect(int left, int top, int width, int height)
{
    XSetFunction(display, _gc, GXnor);
    XFillRectangle(display, _window, _gc, left, top, width, height);
}

/******************************************************************************/
void _frame_round_rect(int left, int top, int width, int height,
                                                  int ovalWidth, int ovalHeight)
{
    int right = left + width,
        bottom = top + height,
        harc = ovalHeight / 2,
        warc = ovalWidth / 2;
    XArc        arcs[4];
    XSegment    segs[4];

    _make_arcs(arcs, left, top, right, bottom, ovalWidth, ovalHeight);

    segs[0].x1 = right;          segs[0].y1 = top+harc;
    segs[0].x2 = right;          segs[0].y2 = bottom-harc;
    segs[1].x1 = right-warc;     segs[1].y1 = bottom;
    segs[1].x2 = left+warc;      segs[1].y2 = bottom;
    segs[2].x1 = left;           segs[2].y1 = top+harc;
    segs[2].x2 = left;           segs[2].y2 = bottom-harc;
    segs[3].x1 = left+warc;      segs[3].y1 = top;
    segs[3].x2 = right-warc;     segs[3].y2 = top;

    XSetFunction(display, _gc, GXcopy);
    XDrawArcs(display, _window, _gc, arcs, 4);
    XDrawSegments(display, _window, _gc, segs, 4);
}

/******************************************************************************/
void _invert_round_rect(int left, int top, int width, int height,
                                                  int ovalWidth, int ovalHeight)
{
    int right = left + width,
        bottom = top + height,
        harc = ovalHeight / 2,
        warc = ovalWidth / 2;
    XArc        arcs[4];
    XRectangle  rcts[4];

    _make_arcs(arcs, left, top, right, bottom, ovalWidth, ovalHeight);

    rcts[0].x      = left+warc;
    rcts[0].y      = top;
    rcts[0].width  = right-left-ovalWidth;
    rcts[0].height = bottom-top;

    rcts[1].x      = left;
    rcts[1].y      = top+harc;
    rcts[1].width  = warc;
    rcts[1].height = bottom-top-ovalHeight;

    rcts[2].x      = right-warc;
    rcts[2].y      = top+harc;
    rcts[2].width  = warc;
    rcts[2].height = bottom-top-ovalHeight;

    XSetFunction(display, _gc, GXnor);
    XFillArcs(display, _window, _gc, arcs, 4);
    XFillRectangles(display, _window, _gc, rcts, 3);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int CheckUI()
{
    int ret;
    if ( _window == 0L ) return -1;
    ret = _check_event();

    if ( ret < 0 ) {
        if ( ret == -1 ) {
            About(about_message);
        } else {
            // quit item selected
            if ( Alert("Are you sure you want to quit?", "OK", "Cancel") )
                exit(1);
        }
    }

    return ret;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int DoUI()
{
    int ret;
    if ( _window == 0L ) return -1;
    while ( (ret = _check_event()) == 0 )
        ;
    return ret;
}

/******************************************************************************/
void GetMouse(int *x, int *y)
{
    *x = cur_x; *y = cur_y;
}

/******************************************************************************/
int NewTextItem(int left, int top, int width, int height, const char*text)
{
    top = _new_top(top);
    return _add_item(TXT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static long int _next_tickle = 0;
static int      _carat_on    = 0;

/******************************************************************************/
static void _tickle_carat(Window win, int force)
{
    char        *name_buf;
    unsigned int len;
    long int     now = time(NULL);
    WindowItem  *item = NULL;
    int          left = 0, top = 0, right = 0, bottom = 0;

    item = _find_item_of_type(win, EDT_ITEM);
    if ( item == NULL ) return;

    if ( !force && now < _next_tickle ) return;

    top = item->top + 2;
    left = item->left + 6;
    bottom = item->bottom - 2;

    name_buf = item->data;
    len = strlen(name_buf);

    left += XTextWidth(XQueryFont(display, font_r), name_buf, len);
    right = left + 1;
    if ( _carat_on )
        XClearArea(display, win, left, top, right-left, bottom-top, False);
    else {
        /* basically, invert the rectangle */
        XSetFunction(display, _gc, GXnor);
        XFillRectangle(display, win, _gc, left, top, right-left, bottom-top);
    }

    _carat_on = !_carat_on;
    _next_tickle = now + 1;
}

/******************************************************************************/
static void _dialog_key(Window win, char key)
{
    char        *name_buf;
    char         ch;
    unsigned int len;
    WindowItem  *item = NULL;
    int          left = 0, top = 0, right = 0, bottom = 0;

    item = _find_item_of_type(win, EDT_ITEM);
    if ( item == NULL ) return;

    if (_carat_on) _tickle_carat(win, True);

    top = item->top + 1;
    left = item->left + 6;
    bottom = item->bottom - 1;

    name_buf = item->data;
    len = strlen(name_buf);

    left += XTextWidth(XQueryFont(display, font_r), name_buf, len);

    if (key == 0x08 || key == 0x7F) { /* backspace or delete */
        if ( len ) {
            ch = name_buf[len-1];
            name_buf[len-1] = 0;
            right = left;
            left -= XTextWidth(XQueryFont(display, font_r), &ch, 1);
            XClearArea(display, win, left, top, right-left, bottom-top, False);
        }
    } else {
        if ( (item->data = realloc(name_buf, len+10)) == NULL )
            item->data = name_buf;
        else
            name_buf = item->data;
        name_buf[len] = key; name_buf[len+1] = 0;
        _draw_string(left, bottom-5, font_r, &name_buf[len]);
    }
}

/******************************************************************************/
int NewEditTextItem(int left, int top, int width, int height, const char*text)
{
    top = _new_top(top);
    return _add_item(EDT_ITEM, strdup(text), left, top, width, height);
}

/******************************************************************************/
static void _copy_img(gdImagePtr im, PictureItem *pic)
{
    int x, y;

    if ( pic->true_colour ) {
        int *tt = (int*)pic->img;

        for (y = 0; y < gdImageSY(im); y++) {
            for (x = 0; x < gdImageSX(im); x++ ) {
                *tt++ = gdImageTrueColorPixel(im, x, y);
            }
        }
    } else {
        unsigned char *tt = pic->img;

        for (y = 0; y < gdImageSY(im); y++) {
            for (x = 0; x < gdImageSX(im); x++ ) {
                int c = gdImagePalettePixel(im, x, y);
                tt[R_OFS] = gdImageRed(im, c);
                tt[G_OFS] = gdImageGreen(im, c);
                tt[B_OFS] = gdImageBlue(im, c);
                tt+=DEPTH;
            }
        }
    }
}

/******************************************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height)
{
    PictureItem *pic = malloc(sizeof(PictureItem));

    top = _new_top(top);

    pic->img = malloc(gdImageSX(im) * gdImageSY(im) * DEPTH);
    pic->true_colour = true_colour;
    pic->left = left; pic->top = top;
    pic->width = width; pic->height = height;

    _copy_img(im, pic);

    pic->image = XCreateImage(display, visual, 24, ZPixmap, 0,
                                    (char*)pic->img, width, height, 8, width*4);

    _draw_picture(pic);
    return _add_item(PIC_ITEM, pic, left, top, width, height);
}

/******************************************************************************/
void FlushPicture(gdImagePtr im, int itm_id)
{
    WindowItem *itm = _find_item(itm_id);
    PictureItem *pic;

    if ( itm == NULL ) return;
    if ( itm->type != PIC_ITEM ) return;
    pic = itm->data;
    if ( pic == NULL ) return;

    _copy_img(im, pic);

    _draw_picture(pic);
}

/******************************************************************************/
char *DoSaveDialog(char *fname)
{
    Window dwin;

    dwin = _new_window(100, 100, 300, 200, True);
    NewControl(pushButton, "OK", 230, 170, 60, 20);
    _add_item(TXT_ITEM, strdup("Save as :"), 20, 20, 80, 20);
    _add_item(EDT_ITEM, strdup(fname), 20, 60, 200, 20);
    //_add_item(EDT_ITEM, strdup(fname), 20, 90, 200, 20);

    // while ( _still_down() ) {
    while ( !_check_event() ) {
        _tickle_carat(dwin, False);
        ;
    }

    _delete_window(dwin);

    return NULL;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void DisableControl(int itm_id)
{
    _set_ctl_state(itm_id, 255);
}

/******************************************************************************/
void EnableControl(int itm_id)
{
    _set_ctl_state(itm_id, 0);
}

/******************************************************************************/
void RenameControl(int itm_id, const char*title)
{
    Control *ctl = NULL;
    WindowItem *item = _find_item(itm_id);

    if ( item == NULL ) return;
    if ( item->type != CTL_ITEM ) return;
    if ( (ctl = item->data) == NULL ) return;

    if ( ctl->title != NULL ) free(ctl->title);

    ctl->title = strdup(title);
    _draw_control(ctl);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height)
{
    Control * ctl ;
    top = _new_top(top);
    ctl = _new_control(_window,
                left, top, width, height,
                title, True,
                0, 0, 0,
                type, 0);

    _draw_control(ctl);
    return _add_item(CTL_ITEM, ctl, left, top, width, height);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static MenuBar *_trking_mbar = NULL;
static Menu *_trking_menu = NULL;
static int _trk_menu_in = 0;

/******************************************************************************/
static MenuBar *_new_menu_bar()
{
    MenuBar *mbar = malloc(sizeof(MenuBar));
    mbar->nMenus = 0;
    mbar->menus = NULL;
    mbar->win = _window;

    return mbar;
}

/******************************************************************************/
static void _draw_mbar(MenuBar *mbar)
{
    int i, h;

    XClearArea(display, _window, 0, 0, win_width, MENU_BAR_HEIGHT, False);
    XSetForeground(display, _gc, LightGrey);
    XDrawLine(display, _window, _gc, 0,
                                   MENU_BAR_HEIGHT, win_width, MENU_BAR_HEIGHT);
    XSetForeground(display, _gc, Black);

    h = 0;
    for (i = 0; i < mbar->nMenus; i++) {
        char *str = mbar->menus[i].items[0];
        mbar->menus[i].h = h;
        _draw_string(h+6, 16, font_b, str);
        mbar->menus[i].tsize =
               (10 + XTextWidth(XQueryFont(display, font_b), str, strlen(str)));
        h += mbar->menus[i].tsize;
    }
}

/******************************************************************************/
static void _draw_menu_item(Menu *menu, int item, int v)
{
    char    keyStr[12] = "Ctrl+_";

    _draw_string(6, v, font_b, menu->items[item]);
    if ( menu->key[item] != 0 ) {
        int tkl;
        keyStr[5] = toupper(menu->key[item]);
        tkl = XTextWidth(XQueryFont(display, font_b), keyStr, 6);
        _draw_string(menu->width-tkl-6, v, font_b, keyStr);
    }
}

/******************************************************************************/
static void _hilite_menu(Menu *menu, int state)
{
    int h, l;
    Window twin = _window;

    h = menu->h; l = menu->tsize;

    _set_window(_trking_mbar->win);

    XClearArea(display, _window, h+1, 0, l-2, MENU_BAR_HEIGHT, False);
    _draw_string(h+6, 16, font_b, menu->items[0]);
    if ( state != 0 ) {
        XSetFunction(display, _gc, GXnor);
        XFillRectangle(display, _window, _gc, h+1, 1, l-2, MENU_BAR_HEIGHT-1);
    }

    _set_window(twin);
}

/******************************************************************************/
static void _hilite_menu_item(Menu *menu, int item, int state)
{
    int l, v;
    Window twin = _window;

    if ( !menu->flags[item] ) return;

    _set_window(menu->win);

    l = menu->width;
    v = (item-1)*MENU_ITEM_HEIGHT;

    XSetFunction(display, _gc, GXcopy);
    if ( state != 0 ) {
        XSetForeground(display, _gc, Black);
        menu->last = item;
    } else {
        XSetForeground(display, _gc, White);
        menu->last = 0;
    }
    XFillRectangle(display, menu->win, _gc, 1, v+1, l-2, MENU_ITEM_HEIGHT-2);

    XSetFunction(display, _gc, GXnor);
    if ( strcmp(menu->items[item], "-") == 0 )
        XDrawLine(display, menu->win, _gc, 3, v+10, menu->width-6, v+10);
    else
        _draw_menu_item(menu, item, v+16);

    XSetFunction(display, _gc, GXcopy);
    XSetForeground(display, _gc, Black);

    _set_window(twin);
}

/******************************************************************************/
static void _draw_menu(Menu *menu)
{
    int i, v;
    Window twin = _window;

    _set_window(menu->win);
    v = 16;
    /* start the count from 1 because 0 is the title */
    for (i = 1; i < menu->nItems; i++) {
        char *str = menu->items[i];
        if ( menu->flags[i] == 0 ) XSetForeground(display, _gc, LightGrey);
        if ( strcmp(str, "-") == 0 )
            XDrawLine(display, menu->win, _gc, 3, v-6, menu->width-6, v-6);
        else
            _draw_menu_item(menu, i, v);
        XSetForeground(display, _gc, Black);
        v += MENU_ITEM_HEIGHT;
    }
    _set_window(twin);
}

/******************************************************************************/
static Menu *_which_menu(MenuBar *mbar, int x, int y)
{
    int i, h, l;

    for (i = 0; i < mbar->nMenus; i++) {
        h = mbar->menus[i].h;
        l = mbar->menus[i].tsize;
        if ( x >= h && x <= h+l )
            return &(mbar->menus)[i];
    }
    return NULL;
}

/******************************************************************************/
static int _point_in_menu(Menu *menu, int x, int y)
{
    if ( menu == NULL ) return 0;

    if ( y <= MENU_BAR_HEIGHT ) return _IN_MBAR_;

    if ( x >= menu->h && x <= menu->h+menu->width &&
         y <= MENU_BAR_HEIGHT+menu->height )
        return _IN_MENU_;

    return 0;
}

/******************************************************************************/
static void _start_track_menu(MenuBar *mbar, int x, int y)
{
    Menu* menu = NULL;

    if ( (menu = _which_menu(mbar, x, y)) != NULL ) {
        _trking_mbar = mbar;
        _trking_menu = menu;
        _trking_menu->last = 0;
        _trk_menu_in = True;

        _hilite_menu(_trking_menu, 254);
        XMapWindow(display, _trking_menu->win);
    }
}

/******************************************************************************/
static int _finish_track_menu()
{
    int res = 0;

    if ( _trk_menu_in ) {
        res = _trking_menu->last;

        XUnmapWindow(display, _trking_menu->win);
        _hilite_menu(_trking_menu, 0);
    }

    _trking_menu = NULL;
    _trking_mbar = NULL;
    _trk_menu_in = False;

    return res;
}

/******************************************************************************/
static int _check_track_menu()
{
    XEvent ev;
    Menu *menu;
    int which;

    while ( XCheckMaskEvent(display, -1L, &ev) ) {
        if ( _process_event(&ev) == ButtonRelease ) {
            return 1;
        }
        return -1;
    }

    switch ( _point_in_menu(_trking_menu, cur_x, cur_y) ) {
        case _IN_MBAR_ :
            menu = _which_menu(_trking_mbar, cur_x, cur_y);
            if ( _trk_menu_in ) {
                if ( _trking_menu != menu ) {
                    XUnmapWindow(display, _trking_menu->win);
                    _hilite_menu(_trking_menu, 0);

                    _trking_menu->last = 0;
                    _trking_menu = NULL;
                    _trk_menu_in = False;
                }
            } else {
                if ( menu != NULL ) {
                    _trk_menu_in = True;
                    _trking_menu = menu;
                    _trking_menu->last = 0;

                    _hilite_menu(_trking_menu, 254);
                    XMapWindow(display, _trking_menu->win);
                }
            }
            break;
        case _IN_MENU_ :
            which = ((cur_y - MENU_BAR_HEIGHT) / MENU_ITEM_HEIGHT) + 1;
            if (which >= _trking_menu->nItems)
                which = _trking_menu->nItems - 1;

            if ( _trking_menu->last != which ) {
                if ( _trking_menu->last != 0 )
                    _hilite_menu_item(_trking_menu, _trking_menu->last, 0);
                _hilite_menu_item(_trking_menu, which, 254);
            }
            break;
        default :
            if ( !_trk_menu_in ) {
                if ( (menu = _which_menu(_trking_mbar, cur_x, cur_y)) != NULL )
                    _trking_menu = menu;
            }
            break;
    }
    return 0;
}

/******************************************************************************/
void _add_menu_item(Menu *menu, const char *item, int enabled, char key)
{
    static char keyStr[8] = " Ctrl+_";
    int sl = XTextWidth(XQueryFont(display, font_b), item, strlen(item)) + 20;
    int slen = strlen(item);

    menu->items = realloc(menu->items, sizeof(char*)*(menu->nItems+1));
    menu->items[menu->nItems] = malloc(slen + 1);
    strncpy(menu->items[menu->nItems], item, slen+1);
    (menu->items[menu->nItems])[slen] = 0;

    menu->flags = realloc(menu->flags, sizeof(char)*(menu->nItems+1));
    menu->flags[menu->nItems] = (!enabled) ? 0x00 : 0x0F;

    menu->key = realloc(menu->key, sizeof(char)*(menu->nItems+1));
    menu->key[menu->nItems] = key;

    if ( menu->key[menu->nItems] != 0 ) {
        keyStr[6] = menu->key[menu->nItems];
        sl += XTextWidth(XQueryFont(display, font_b), keyStr, 7);
    }
    if ( sl > menu->width ) menu->width = sl;

    menu->nItems++;
}

/******************************************************************************/
Menu *_new_menu(const char *title)
{
    Menu *menu = NULL;
    MenuBar *mbar = NULL;
    WindowItem *item = NULL;

    item = _find_item_of_type(_window, WIN_MENU);

    if ( item == NULL || item->data == NULL ) {
        WindowPtr  wptr = _find_window(_window);
        if ( wptr == NULL ) return NULL;

        mbar = _new_menu_bar();

        item = wptr->itm_lst;
        while ( item != NULL ) {
            item->top += MENU_BAR_HEIGHT;
            item->bottom += MENU_BAR_HEIGHT;
            if ( item->type == CTL_ITEM )
                ((Control*)(item->data))->top += MENU_BAR_HEIGHT;
            if ( item->type == PIC_ITEM )
                ((PictureItem*)(item->data))->top += MENU_BAR_HEIGHT;
            item = item->next;
        }
        wptr->mbarItm =
                    _add_item(WIN_MENU, mbar, 0, 0, win_width, MENU_BAR_HEIGHT);
        win_height += MENU_BAR_HEIGHT;
        XResizeWindow(display, _window, win_width, win_height);
    } else
        mbar = item->data;

    mbar->menus = realloc(mbar->menus, sizeof(Menu)*(mbar->nMenus+1));

    menu = &(mbar->menus)[mbar->nMenus];
    if ( mbar->nMenus == 0 ) menu->h = 0;
    else menu->h = (mbar->menus)[mbar->nMenus-1].h + (mbar->menus)[mbar->nMenus-1].tsize;
    mbar->nMenus++;

    menu->menuID = mbar->nMenus;
    menu->items = NULL;
    menu->flags = NULL;
    menu->key   = NULL;
    menu->width = 0;

    menu->win = _window;
    menu->nItems = 0;

    menu->tsize = XTextWidth(XQueryFont(display, font_b), title, strlen(title)) + 12;

    _add_menu_item(menu, title, True, 0);
    return menu;
}

/******************************************************************************/
void _append_menu(Menu * menu, const char*idata)
{
    char key = 0;
    char *data = strdup(idata);
    char *bt = data;

    while (*data) {
        char *s, *t, *ts, meta;
        int l;

        meta = *data;
        t = (char*)data;
        if ( meta == '(' ) t++;
        else meta = 0;
        s = strchr(t, ';');

        if ( s != NULL ) { *s = 0; l = s - t; }
        else l = strlen(t);

        if ( (ts = strchr(t, '/')) != NULL ) {
            *ts++ = 0; // terminate item string at first meta
            key = *ts;
        }

        data += l;
        if ( meta == '(' ) data++;
        if ( s != NULL ) { *s = 0; data++; }

        _add_menu_item(menu, t, (meta!='('), key);
    }
    free(bt);
}

/******************************************************************************/
int _add_menu(Menu * menu)
{
    Window  twin = _window;

    menu->height = MENU_ITEM_HEIGHT * (menu->nItems - 1); // item 0 is the title
    if (menu->height <= 0) menu->height = 1;
    menu->win = (Window)_new_window(menu->h, MENU_BAR_HEIGHT,
                                               menu->width, menu->height, True);
    _set_window(menu->win);
    menu->win_itm = _add_item(MNU_ITEM, menu, 0, 0, menu->width, menu->height);
    _set_window(twin);

    return menu->win_itm;
}

/******************************************************************************/
int create_menu(const char*title, const char*data)
{
    void *menu = NULL;

    menu = _new_menu(title);

    _append_menu(menu, data);

    return _add_menu(menu);
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
static int _check_event()
{
    XEvent ev;
    int ret = 0;

    if ( _trking_ctl != NULL ) {
        if ( _check_track_control() == 1 && _finish_track_control() )
            return _trking_itmid;
        return 0;
    }

    if ( _trking_mbar != NULL ) {
        if ( _check_track_menu() == 1 && (ret = _finish_track_menu()) ) {
            return -ret;
        }
        return 0;
    }

    ev.type = 0;
    while ( XCheckMaskEvent(display, -1L, &ev) == True ) {
        ret = _process_event(&ev);
        if ( ret == ButtonPress ) {
            WindowItem *itm = _which_item(cur_x, cur_y);
            if ( itm != NULL ) {
                _trking_itmid = itm->id;
                if ( itm->type == CTL_ITEM )
                    _start_track_control(itm->data, cur_x, cur_y, NULL);
                else if ( itm->type == WIN_MENU )
                    _start_track_menu(itm->data, cur_x, cur_y);
            }
        }
    }
    return 0;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/

/******************************************************************************/
unsigned long int MakeColour(int red, int green, int blue)
{
    XColor color;

    color.red = red<<8;
    color.green = green<<8;
    color.blue = blue<<8;
    if ( !XAllocColor(display, cmap, &color) )
        fprintf(stderr, "XAllocColor failed?\n");;
    return color.pixel;
}

/******************************************************************************/
int InitUI(int *width, int *height)
{
    char AboutMnu[128];
    char *mnuTitle = NULL;
    display = XOpenDisplay(NULL);  /* open the default display */
    if ( display == NULL ) {
        fprintf(stderr, "Cannot open default display\n");
        return -1;
    }
    screen = DefaultScreen(display);
    visual = DefaultVisual(display, screen);
    display_width = DisplayWidth(display, screen);
    display_height = DisplayHeight(display, screen);

    /* get colours, probably better to get named colours, using this only if
     * that failed */
    Black = BlackPixel(display, screen);
    White = WhitePixel(display, screen);

    cmap = DefaultColormap(display, screen);

    LightGrey = MakeColour(200, 200, 206);
    Grey  = MakeColour(128, 128, 134);
    Red   = MakeColour(255, 0, 0);
    Green = MakeColour(0, 255, 0);
    Blue  = MakeColour(0, 0, 255);

    /* get fonts */
    if ( (font_f = XLoadFont(display, FONT_F)) == 0 ) {
        fprintf(stderr, "Cannot allocate fixed font\n");
        exit(1);
    }
    if ( (font_r = XLoadFont(display, FONT_R)) == 0 )
        font_r = font_f;
    if ( (font_b = XLoadFont(display, FONT_B)) == 0 )
        font_b = font_f;

    if (  *width+30 > display_width )  *width  = display_width - 30;
    if ( *height+80 > display_height ) *height = display_height - 80;
    win_width = *width; win_height = *height;
    _window = _new_window(10, 10, *width, *height, False);
    _main_window = _window;

    if (short_progname != NULL)
        mnuTitle = strdup(short_progname);
    else
        mnuTitle = strdup(progname);
    mnuTitle[0] = toupper(mnuTitle[0]);
//  create_menu("File", "Quit");
    if (short_progname != NULL) {
        snprintf(AboutMnu, 126, "About %s;(-;Quit/q", short_progname);
        create_menu(mnuTitle, AboutMnu);
    } else
        create_menu(mnuTitle, "About;(-;Quit/q");
    free(mnuTitle);
    return 0;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int CleanupUI()
{
    if ( _window == 0L ) return -1;

    _delete_window(_window);

    if ( font_r != font_f ) XUnloadFont(display, font_r);
    if ( font_b != font_f ) XUnloadFont(display, font_b);
    XUnloadFont(display, font_f);
    _window = 0L;
    XCloseDisplay(display);
    return 0;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
int Alert(const char *message, const char *but1, const char *but2)
{
    Window twin, dwin;
    int b1 = -1, b2 = -1, ret = -1;

    twin = _window;
    dwin = _new_window(100, 100, 300, 100, True);
    XMapWindow(display, dwin);
    _set_window(dwin);

    if (but1 == NULL) b1 = NewControl(pushButton, "OK", 230, 70, 60, 20);
    else              b1 = NewControl(pushButton, but1, 230, 70, 60, 20);

    if (but2 != NULL) b2 = NewControl(pushButton, but2, 160, 70, 60, 20);

    _add_item(TXT_ITEM, strdup(message), 60, 20, 80, 20);

    while (1) {
        if ( (ret = _check_event()) > 0 ) {
            if ( ret == b1 ) ret = 1;
            else if ( ret == b2 ) ret = 0;
            break;
        }
    }

    _delete_window(dwin);
    _set_window(twin);

    return ret;
}

/******************************************************************************
 *                                                                            *
 ******************************************************************************/
void About(const char *message)
{
    Window twin, dwin;
    int b1 = -1, b2 = -1, ret = -1;

    twin = _window;
    dwin = _new_window(100, 100, 400, 200, True);
    XMapWindow(display, dwin);
    _set_window(dwin);

    b1 = NewControl(pushButton, "OK", 330, 170, 60, 20);

    char *k, *m, *s = strdup(message);
    int v = 20;
    k = s;

    while (*s) {
        m = s;
        while (*s && (*s != '\n') ) s++;
        if (*s == '\n') *s++ = 0;
        _add_item(TXT_ITEM, strdup(m), 60, v, 180, 20);
        v+=20;
    }
    free(k);

    while (1) {
        if ( (ret = _check_event()) > 0 ) {
            if ( ret == b1 ) ret = 1;
            else if ( ret == b2 ) ret = 0;
            break;
        }
    }

    _delete_window(dwin);
    _set_window(twin);
}
