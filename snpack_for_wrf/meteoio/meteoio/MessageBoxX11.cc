/*
   I, David Oberhollenzer, author of this file hereby place the contents of
   this file into the public domain. Please feel free to use this file in any
   way you wish.
   I want to do this, because a lot of people are in the need of a simple X11
   message box function.
   The original version was written in C++ and has been ported to C. This
   version is entirely leak proof! (According to valgrind)
 */

#ifdef __linux

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include <string.h>
#include <stdlib.h>


/**************************************************************************
 * A "small" and "simple" function that creates a message box with an OK  *
 * button, using ONLY Xlib.                                               *
 * The function does not return until the user closes the message box,    *
 * using the OK button, the escape key, or the close button what means    *
 * that you can't do anything in the mean time(in the same thread).       *
 * The code may look very ugly, because I pieced it together from         *
 * tutorials and manuals and I use an awfull lot of magic values and      *
 * unexplained calculations.                                              *
 *                                                                        *
 * title: The title of the message box.                                   *
 * text:  The contents of the message box. Use '\n' as a line terminator. *
 **************************************************************************/
void MessageBoxX11( const char* title, const char* text ) {
	static const char* wmDeleteWindow = "WM_DELETE_WINDOW";

	/* Open a display */
	Display* dpy = XOpenDisplay(0);
	if (!dpy) return;

	/* Get us a white and black color */
	const unsigned long black = BlackPixel( dpy, DefaultScreen(dpy) );
	const unsigned long white = WhitePixel( dpy, DefaultScreen(dpy) );
	static const char grey[] = "#dcdad5";
	Colormap colormap = DefaultColormap(dpy, 0);
	XColor color;
	XParseColor(dpy, colormap, grey, &color);
	XAllocColor(dpy, colormap, &color);

	/* Create a window with the specified title */
	Window w = XCreateSimpleWindow( dpy, DefaultRootWindow(dpy), 0, 0, 100, 100,
				0, black, color.pixel );
	XSelectInput( dpy, w, ExposureMask | StructureNotifyMask |
				KeyReleaseMask | PointerMotionMask |
				ButtonPressMask | ButtonReleaseMask   );
	XMapWindow( dpy, w );
	XStoreName( dpy, w, title );

	Atom wmDelete = XInternAtom( dpy, wmDeleteWindow, True );
	XSetWMProtocols( dpy, w, &wmDelete, 1 );

	/* Create a graphics context for the window */
	GC gc = XCreateGC( dpy, w, 0, 0 );
	XSetForeground( dpy, gc, black );
	XSetBackground( dpy, gc, white );

	/* Split the text down into a list of lines */
	char **strvec = NULL;
	size_t strvec_size = 0, text_len = strlen(text)+1;
	char *temp = (char *)malloc( text_len );
	strncpy( temp, text, text_len );

	char *pch = strtok( temp, "\n" );
	while ( pch!=NULL ) {
		strvec = (char **)realloc( strvec, (strvec_size+1)*sizeof(char**) );
		strvec[ strvec_size ] = (char *)malloc( strlen(pch)+1 );
		strncpy( strvec[ strvec_size ], pch, strlen(pch)+1 );
		++strvec_size;
		pch = strtok( NULL, "\n" );
	}
	free( temp );

	/* Compute the printed length and height of the longest and the tallest line */
	XFontStruct* font = XQueryFont( dpy, XGContextFromGC(gc));
	if (!font) return;

	int length=0, height=0, direction, ascent, descent;
	XCharStruct overall;
	for ( size_t i=0; i<strvec_size; ++i ) {
		XTextExtents( font, strvec[ i ], static_cast<int>(strlen(strvec[ i ])), &direction, &ascent, &descent, &overall );
		length =  overall.width  >length ? overall.width    : length;
		height = (ascent+descent)>height ? (ascent+descent) : height;
	}

	/* Compute the shape of the window, needed to display the text and adjust the window accordingly */
	const int X = DisplayWidth ( dpy, DefaultScreen(dpy) )/2 - length/2-10;
	const int Y = DisplayHeight( dpy, DefaultScreen(dpy) )/2 - static_cast<int>(height/2 - height - 10);
	const int W = length + 20;
	const int H = static_cast<int>(strvec_size*height + height + 40);
	XMoveResizeWindow( dpy, w, X, Y, W, H );

	/* Compute the shape of the OK button */
	XTextExtents( font, "OK", 2, &direction, &ascent, &descent, &overall );
	const int okWidth = overall.width;
	const int okHeight = ascent + descent;
	const int okX1 = W/2 - okWidth/2 - 15;
	const int okY1 = static_cast<int>(strvec_size*height + 20) + 5;
	const int okX2 = W/2 + okWidth/2 + 15;
	const int okY2 = okY1 + 4 + okHeight;
	const int okBaseX = okX1 + 15;
	const int okBaseY = okY1 + 2 + okHeight;

	//XFreeFontInfo( NULL, font, 1 ); /* We don't need that anymore */

	/* Make the window non resizeable */
	XUnmapWindow( dpy, w );
	XSizeHints* hints = XAllocSizeHints( );
	hints->flags      = PSize | PMinSize | PMaxSize;
	hints->min_width  = hints->max_width  = hints->base_width  = W;
	hints->min_height = hints->max_height = hints->base_height = H;
	XSetWMNormalHints( dpy, w, hints );
	XFree( hints );

	XMapRaised( dpy, w );
	XFlush( dpy );

	/* Event loop */
	int run = 1, buttonFocus = 0;
	do {
		XEvent e;
		XNextEvent( dpy, &e );
		int offset = 0;

		if ( e.type == MotionNotify ) {
			if ( e.xmotion.x>=okX1 && e.xmotion.x<=okX2 && e.xmotion.y>=okY1 && e.xmotion.y<=okY2 ) {
				if ( !buttonFocus ) e.type = Expose;
				buttonFocus = 1;
			} else {
				if ( buttonFocus ) e.type = Expose;
				buttonFocus = 0;
				offset = 0;
			}
		}

		switch( e.type ) {
			case ButtonPress:
			case ButtonRelease:
				if ( e.xbutton.button!=Button1 ) break;

				if ( buttonFocus ) {
					offset = e.type==ButtonPress ? 1 : 0;
					if ( !offset ) run = 0;
				} else {
					offset = 0;
				}

			case Expose:
			case MapNotify:
				XClearWindow( dpy, w );

				/* Draw text lines */
				for ( size_t i=0; i<strvec_size; ++i )
					XDrawString( dpy, w, gc, 10, static_cast<int>(10+height + height*i), strvec[i], static_cast<int>(strlen(strvec[i])) );

				/* Draw OK button */
				if ( buttonFocus ) {
					XFillRectangle( dpy, w, gc, offset+okX1, offset+okY1, okX2-okX1, okY2-okY1 );
					XSetForeground( dpy, gc, white );
				} else {
					XDrawLine( dpy, w, gc, okX1, okY1, okX2, okY1 );
					XDrawLine( dpy, w, gc, okX1, okY2, okX2, okY2 );
					XDrawLine( dpy, w, gc, okX1, okY1, okX1, okY2 );
					XDrawLine( dpy, w, gc, okX2, okY1, okX2, okY2 );
				}

				XDrawString( dpy, w, gc, offset+okBaseX, offset+okBaseY, "OK", 2 );

				if ( buttonFocus ) {
					XSetForeground( dpy, gc, black );
				}

				XFlush(dpy);
				break;

			case KeyRelease:
				if ( XLookupKeysym( &e.xkey, 0 ) == XK_Escape ) run = 0;
				break;

			case ClientMessage:
				char *atom = XGetAtomName( dpy, e.xclient.message_type );
				if ( *atom == *wmDeleteWindow ) run = 0;
				XFree(atom);
				break;
		};
	} while ( run );

	/* Clean up */
	for ( size_t i=0; i<strvec_size; ++i ) free( strvec[i] );
	free( strvec );
	XFreeGC( dpy, gc );
	XDestroyWindow( dpy, w );
	XCloseDisplay( dpy );
}

#endif

