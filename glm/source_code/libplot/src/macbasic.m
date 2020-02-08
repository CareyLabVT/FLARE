/******************************************************************************
 *                                                                            *
 * macbasic.m                                                                 *
 *                                                                            *
 *   A very basic Mac OS-X GUI example.                                       *
 *                                                                            *
 * Developed by :                                                             *
 *     AquaticEcoDynamics (AED) Group                                         *
 *     School of Agriculture and Environment                                  *
 *     The University of Western Australia                                    *
 *                                                                            *
 * Copyright 2015-2019 -  The University of Western Australia                 *
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
 *  Created CAB 20150105                                                      *
 *                                                                            *
 ******************************************************************************/

// set minimum OS version to El Capitan
#undef __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__
#define __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ 101100

#import <Cocoa/Cocoa.h>
#include <ui_basic.h>

#if __clang_major__ == 7
#ifndef NSEventModifierFlagOption
#define NSEventModifierFlagOption NSAlternateKeyMask
#define NSEventModifierFlagCommand NSCommandKeyMask
#define NSWindowStyleMaskTitled NSTitledWindowMask
#define NSEventMaskAny NSAnyEventMask
#define NSAlertStyleWarning NSWarningAlertStyle
#define NSAlertStyleInformational NSInformationalAlertStyle
#endif
#endif

extern char *progname, *short_progname, *about_message;
int Alert(const char *message, const char *but1, const char*but2);

/******************************************************************************/
#define CTL_ITEM    1
#define PIC_ITEM    2
#define TXT_ITEM    3
#define EDT_ITEM    4
#define WIN_MENU    5
#define MNU_ITEM    6

/******************************************************************************/
typedef struct _pic_item {
    CGContextRef      context;
    unsigned char    *img;
    NSImage          *ns_img;
    NSImageView      *ns_imgv;
    int               true_colour;
    int               left;
    int               top;
    int               width;
    int               height;
} PictureItem;

/******************************************************************************/
typedef struct _win_item {
    struct _win_item *next;
    int               id;
    int               type;
    void             *data;
} WindowItem;


static int stopit = 0;

/******************************************************************************/

@interface Controller : NSObject
{
    id menubar;
    id appName;
    id window;
}
- (id)init;
- (void) applicationDidFinishLaunching:(NSNotification *)notification;
- (void)runUIEvents:(NSDate*) wait;
- (void)cleanUp;
- (void)addMenus;
- (int)addWindow:(int) x ypos:(int) y width:(int) w height:(int) h;
- (id)addButton:(int) x ypos:(int) y width:(int) w height:(int) h title:(NSString*)title;
- (id)addPicture:(PictureItem*) pic;
- (BOOL)validateMenuItem:(NSMenuItem *)item;
- (IBAction)endProgram:(id)sender;
- (IBAction)buttonPressed:(id)sender;
- (IBAction)menuHit:(id)sender;
@end

/******************************************************************************/
static Controller *sharedController;
static id my_window;

/*============================================================================*/
@implementation Controller

/*----------------------------------------------------------------------------*/
- (id) init
{
    self = [super init];

    NSApplicationLoad();
    appName = [[NSProcessInfo processInfo] processName];

    sharedController = self;
    return self;
}

/*----------------------------------------------------------------------------*/
- (void) applicationDidFinishLaunching:(NSNotification *)notification
{
//  NSLog(@"%@ %s", self, __func__);
    [NSApp activateIgnoringOtherApps:YES];
}

/*----------------------------------------------------------------------------*/
- (IBAction) endProgram:(id)sender
{
//  NSLog(@"%@ %s", self, __func__);
    stopit = -2;
//  [NSApp terminate];
}

/*----------------------------------------------------------------------------*/
- (IBAction) menuHit:(id)sender
{
    stopit = ((NSMenuItem*)sender).tag;
//  NSLog(@ "Menu Hit %d\n", stopit);
}

/*----------------------------------------------------------------------------*/
- (IBAction) About:(id)sender
{
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];

    [alert setMessageText:[NSString stringWithUTF8String:about_message]];

    [alert setAlertStyle:NSAlertStyleInformational];

    if ([alert runModal] == NSAlertFirstButtonReturn) {
        // OK clicked, delete the record
    }
    [alert release];
}

/*----------------------------------------------------------------------------*/
- (void) addMenus
{
    id menuItem;
    id appMenuItem = [[NSMenuItem new] autorelease];

    menubar = [[NSMenu new] autorelease];

    [NSApp setMainMenu:menubar];
    [menubar addItem:appMenuItem];

    id appMenu = [[NSMenu new] autorelease];

    if (short_progname != NULL) {
        menuItem = [[[NSMenuItem alloc] initWithTitle:[@"About " stringByAppendingString:
                                               [NSString stringWithUTF8String:short_progname]]
//                                         action:@selector(orderFrontStandardAboutPanel:)
                                           action:@selector(About:)
                                    keyEquivalent:@""] autorelease];
    } else {
        menuItem = [[[NSMenuItem alloc] initWithTitle:[@"About " stringByAppendingString:appName]
//                                         action:@selector(orderFrontStandardAboutPanel:)
                                           action:@selector(About:)
                                    keyEquivalent:@""] autorelease];
    }

    [appMenu addItem:menuItem];

    [appMenu addItem:[NSMenuItem separatorItem]];

    menuItem = [[[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"Preferences%C", (unichar)0x2026]
                                           action:@selector(myPrefs:)
                                    keyEquivalent:@","] autorelease];
    [appMenu addItem:menuItem];

    menuItem = [[[NSMenuItem alloc] initWithTitle:[@"Hide " stringByAppendingString:appName]
                                           action:@selector(hide:)
                                    keyEquivalent:@"h"] autorelease]; //Hide app
    [appMenu addItem:menuItem];

    menuItem = [[[NSMenuItem alloc] initWithTitle:@"Hide Others"
                                           action:@selector(hideOtherApplications:)
                                    keyEquivalent:@"h"] autorelease]; // Hide Others
    [menuItem setKeyEquivalentModifierMask:(NSEventModifierFlagOption|NSEventModifierFlagCommand)];
    [appMenu addItem:menuItem];

    menuItem = [[[NSMenuItem alloc] initWithTitle:@"Show All"
                                           action:@selector(unhideAllApplications:)
                                    keyEquivalent:@""] autorelease]; // Show All
    [appMenu addItem:menuItem];

    [appMenu addItem:[NSMenuItem separatorItem]];

    menuItem = [[[NSMenuItem alloc] initWithTitle:[@"Quit " stringByAppendingString:appName]
//                                         action:@selector(menuHit:)
                                           action:@selector(endProgram:)
                                    keyEquivalent:@"q"] autorelease];
    [appMenu addItem:menuItem];

    [appMenuItem setSubmenu:appMenu];
}

/*----------------------------------------------------------------------------*/
- (BOOL)validateMenuItem:(NSMenuItem *)item
{
//  NSLog(@"%@ %s %@", self, __func__, item);

//  if ( [item action] == @selector(endProgram:) )
//      return YES;

    return YES;
}

/*----------------------------------------------------------------------------*/
- (int) addWindow:(int) x ypos:(int) y width:(int) w height:(int) h
{
    window = [[NSWindow alloc] initWithContentRect:NSMakeRect(x, y, w, h)
        styleMask:NSWindowStyleMaskTitled backing:NSBackingStoreBuffered defer:NO];

    if ( !window ) {
        fprintf(stderr, "Failed to make window\n");
        return -1;
    }

    [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
    if (progname != NULL)
        [window setTitle:[NSString stringWithUTF8String:progname]];
    else
        [window setTitle:appName];
    [window makeKeyAndOrderFront:self];
    [window retain];
    my_window = window;

    [NSApp updateWindows];
    return 0;
}

/*----------------------------------------------------------------------------*/
- (id) addButton:(int) x ypos:(int) y width:(int) w height:(int) h title:(NSString*)title
{
    id button = [[NSButton alloc] initWithFrame:NSMakeRect(x, y, w, h)];
    [button setTitle:title];
    [button setTarget:self];
    [button setAction:@selector(buttonPressed:)];
    [button setBezelStyle:NSBezelStyleRounded];

    [button retain];

    [[window contentView] addSubview:button];
    return button;
}

/*----------------------------------------------------------------------------*/
- (id) addPicture:(PictureItem*) pic
{
    id img = [[NSImage alloc] initWithCGImage:CGBitmapContextCreateImage(pic->context)
                                              size:NSZeroSize];
    NSImageView *imgv = [[[NSImageView alloc] initWithFrame:NSMakeRect(pic->left-1, pic->top-1, pic->width+2, pic->height+2)] retain];
    [imgv setImage:img];
    [imgv setImageFrameStyle:NSImageFramePhoto];

    [[window contentView] addSubview:imgv];

    pic->ns_img = img;
    pic->ns_imgv = imgv;

    return img;
}

/*----------------------------------------------------------------------------*/
- (void) runUIEvents:(NSDate*) wait
{
    NSEvent *event = NULL;
//  do  {
        event=[ NSApp nextEventMatchingMask:NSEventMaskAny
                                  untilDate:wait
                                     inMode:NSDefaultRunLoopMode
                                    dequeue:YES ];
        if ( event != NULL ) {
            [NSApp sendEvent:event];
//          stopit = (1 << 16);
        }
//  } while (event != NULL);
}

/*----------------------------------------------------------------------------*/
- (IBAction) buttonPressed:(id)sender
{
    stopit = ((NSButton*)sender).tag;
//  fprintf(stderr, "Button pressed %d\n", stopit);
}

/*----------------------------------------------------------------------------*/
- (void) cleanUp
{
    [window release];
}

@end
/******************************************************************************/


static WindowItem *itm_lst = NULL;
static int idhint = 1;

/******************************************************************************/
static int _add_item(int type, void *data,
                                       int left, int top, int width, int height)
{
    WindowItem *item = malloc(sizeof(WindowItem));
    WindowItem *ti = NULL;

    ti = itm_lst;

    item->next = NULL;
    if ( ti == NULL )
        itm_lst = item;
    else {
        while ( ti->next )
            ti = ti->next;
        ti->next = item;
    }

    item->type = type;
    item->data = data;
    item->id = idhint++;

    return item->id;
}

/******************************************************************************/
static void _release_items(void)
{
    WindowItem *item = NULL, *next;

    item = itm_lst;

    while ( item != NULL ) {
        if ( item->type == CTL_ITEM ) {
            [(id)(item->data) release];
        }
        else if ( item->type == PIC_ITEM ) {
            PictureItem *pic = item->data;
            [(id)(pic->context) release];
            free(pic->img);
            free(pic);
        }
        next = item->next;
        free(item);
        item = next;
    }
}

/******************************************************************************/
static WindowItem *_find_item(int itm_id)
{
    WindowItem *item = NULL;

    item = itm_lst;

    while ( item != NULL ) {
        if ( item->id == itm_id )
            return item;
        item = item->next;
    }
    return NULL;
}


/******************************************************************************/
#define DEPTH 4
#define R_OFS 0
#define G_OFS 1
#define B_OFS 2
/*----------------------------------------------------------------------------*/
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


static int wHeight = 100;

/******************************************************************************/
int InitUI(int *width, int * height)
{
//  fprintf(stderr, "InitUI\n");

    CGDirectDisplayID display = CGMainDisplayID();
    size_t sheight = CGDisplayPixelsHigh(display);
    size_t swidth  = CGDisplayPixelsWide(display);
    Controller *controller;

    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

    /* adjust height/width so window will fit on the display */
    if ( sheight - 40 < *height ) *height = sheight - 40;
    if ( swidth < *width ) *width = swidth;
    wHeight = *height;

    // create our gui "Controller" instance
    if (!(controller = [[Controller alloc] init]))
        return -1;
    [NSApp setDelegate: (id)controller];   // tell NSApp to use this one

    [controller retain];
    [controller addMenus];
    [controller addWindow:0 ypos:0 width:(*width) height:(*height)];

    [NSApp finishLaunching];
    return 0;
}


/******************************************************************************/
static int doing_ui = 0;
int CheckUI(void)
{
    /* If we are just checking the ui we want to return as quickly as         *
     * possible unless there is a quit requested.  If we are "doing_ui" then  *
     * we have finished processing and can wait for user input indefinitely   */
    if ( doing_ui ) {
        [sharedController runUIEvents:[NSDate distantFuture]];
    } else {
        [sharedController runUIEvents:[NSDate dateWithTimeIntervalSinceNow: 0.0]];
        if ( stopit < 0 ) {
            if ( Alert("Are you sure you want to quit?", "OK", "Cancel") )
                exit(1);
            else
                stopit = 0;
        }
    }
    return stopit;
}


/******************************************************************************/
int DoUI(void)
{
    doing_ui = 1;
    while (!CheckUI()) ;
    doing_ui = 0;
    return stopit;
}

/******************************************************************************/
int CleanupUI(void)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    [sharedController cleanUp];

    _release_items();

    [localPool release];
    return 0;
}

/******************************************************************************/
int NewControl(int type, const char*title,
                                      int left, int top, int width, int height)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

    int id_no;

    top = wHeight - top - height;
    id button = [sharedController addButton:left ypos:top
                                   width:width height:height
                                   title:[NSString stringWithUTF8String:title]];
    [button setBezelStyle:NSBezelStyleRounded];

    id_no = _add_item(CTL_ITEM, button, left, top, width, height);
    ((NSButton*)button).tag = id_no;

    [localPool release];
    return id_no;
}

/******************************************************************************/
int NewPicture(gdImagePtr im, int true_colour,
                                      int left, int top, int width, int height)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];

    CGContextRef    context = NULL;
    CGColorSpaceRef colorSpace = NULL;
    PictureItem *pic = malloc(sizeof(PictureItem));

    top = wHeight - top - height;

    pic->img = malloc(gdImageSX(im) * gdImageSY(im) * DEPTH);
    pic->true_colour = true_colour;
    pic->left = left; pic->top = top;
    pic->width = width; pic->height = height;

    width = gdImageSX(im);
    height = gdImageSY(im);

    _copy_img(im, pic);

    colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);

    context = CGBitmapContextCreate(pic->img,
                                    width,
                                    height,
                                    8,      // bits per component
                                    width * DEPTH,
                                    colorSpace,
                                    (CGBitmapInfo)kCGImageAlphaNoneSkipLast);
    if (context== NULL) {
        free(pic->img); free(pic);
        return -1;
    }
    pic->context = context;
    CGColorSpaceRelease(colorSpace);

    [sharedController addPicture: pic];

    [localPool release];
    return _add_item(PIC_ITEM, pic, left, top, width, height);
}

/******************************************************************************/
void FlushPicture(gdImagePtr im, int itm_id)
{
    WindowItem *itm = _find_item(itm_id);
    PictureItem *pic = itm->data;

    _copy_img(im, pic);

    [pic->ns_img release];

    pic->ns_img = [[NSImage alloc] initWithCGImage:CGBitmapContextCreateImage(pic->context)
                                              size:NSZeroSize];

    [pic->ns_imgv setImage:pic->ns_img];
    [pic->ns_imgv setNeedsDisplayInRect:NSMakeRect(pic->left, pic->top, pic->width, pic->height)];
}

/******************************************************************************/
void EnableControl(int itm_id)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    WindowItem *itm = _find_item(itm_id);
    id myButton = itm->data;
    [myButton setEnabled: YES];
    [localPool release];
}

/******************************************************************************/
void DisableControl(int itm_id)
{
    NSAutoreleasePool *localPool = [[NSAutoreleasePool alloc] init];
    WindowItem *itm = _find_item(itm_id);
    id myButton = itm->data;
    [myButton setEnabled: NO];
    [localPool release];
}

/******************************************************************************/
int Alert(const char *message, const char *but1, const char*but2)
{
    int ret = 0;
    NSAlert *alert = [[NSAlert alloc] init];
    if ( but1 == NULL ) [alert addButtonWithTitle:@"OK"];
    else                [alert addButtonWithTitle:[NSString stringWithUTF8String:but1]];
    if ( but2 != NULL ) [alert addButtonWithTitle:[NSString stringWithUTF8String:but2]];

    [alert setMessageText:[NSString stringWithUTF8String:message]];


    if ( but2 != 0 ) [alert setAlertStyle:NSAlertStyleWarning];
    else             [alert setAlertStyle:NSAlertStyleInformational];
                       // or .. NSAlertStyleCritical

    if ([alert runModal] == NSAlertFirstButtonReturn) {
        // OK clicked, delete the record
        ret = 1;
    }
    [alert release];
    return ret;
}
