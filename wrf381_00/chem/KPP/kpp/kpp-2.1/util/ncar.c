#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include "ncar.h"

#define MAX_COLORS 9
#define MAX_GRAPHS 8 
#define WS_ID 1

Grgb colors[] = {{ 0.0, 0.0, 0.0 },
		 { 1.0, 1.0, 1.0 },
		 { 0.0, 1.0, 0.0 },
                 { 1.0, 1.0, 0.0 },
                 { 0.0, 1.0, 1.0 }, 
		 { 1.0, 0.4, 0.4 },
                 { 1.0, 0.0, 1.0 }, 
		 { 0.7, 1.0, 0.7 },
		 { 0.5, 0.5, 1.0 }
		};


int nGraphs = 0;
int nMax;
int nCrt;
int CrtGraph;

char *graphName[ MAX_GRAPHS ];
char *graphTitle;
char * startMsg = "Working... Press CTRL-C to stop";
char * endMsg   = "DONE ! Press <ENTER> when ready.";
char * status = "S";

float * Xval;
float * Yval;
float * scale;
float * offset;
float ch;

float XminGraph, XmaxGraph;
float Ybottom[ MAX_GRAPHS ];
float Ytop[ MAX_GRAPHS ];
float Ymin[ MAX_GRAPHS ];
float Ymax[ MAX_GRAPHS ];
float Ybase[ MAX_GRAPHS ];

int crtState;
int clean = 0;

void Boundary();

void OpenWin()
{
int i;
Gcolr_rep colr;		

  gopen_gks( "stdout", 0 );
  gopen_ws( WS_ID, (char*)0, 8 );
  gactivate_ws( WS_ID );
   
  for( i = 0; i < MAX_COLORS; i++ ) {
    colr.rgb.red = colors[i].red;
    colr.rgb.green = colors[i].green;
    colr.rgb.blue = colors[i].blue;
    gset_colr_rep( 1, i, &colr );
  }  
}


int DefineGraph( char * label, float min, float max )
{
  graphName[ nGraphs ] = label;
  Ymin[ nGraphs ] = min;
  Ymax[ nGraphs ] = max;
  nGraphs++;
  return nGraphs;   
}

void SelectGraph( int i ) 
{
  CrtGraph = i;
  
  c_agsetf("GRID/BOTTOM.", Ybottom[i]);
  c_agsetf("GRID/TOP."   , Ytop[i]);

  c_agsetf("Y/MINIMUM.", Ymin[i]);
  c_agsetf("Y/MAXIMUM.", Ymax[i]);
/*
  c_agsetf("LEFT/MAJOR/BASE.", Ybase[i] );   
*/
  if( i == 0 ) 
    c_agsetf("BOTTOM/TYPE.", 3); 
  else
    c_agsetf("BOTTOM/TYPE.", 0);
}

void InitGraph( int n, float Xmin, float Xmax, char *title )
{
int i;
float step;
char buf[100];

  nMax = n;
  n = n + 1;
  XminGraph = Xmin;
  XmaxGraph = Xmax;
  graphTitle = title;
  
  step = (Xmax - Xmin) / nMax;

  Xval = (float*)malloc( n * sizeof(float) );
  for( i = 0; i < n; i++ )
    Xval[i] = Xmin + step*i;

  Yval = (float*)malloc( nGraphs * n * sizeof(float) );
  for( i = 0; i < nGraphs * n; i++ )
    Yval[i] = NULL/1;

  c_agseti("WINDOWING.",1);
  c_agseti("FRAME.", 2 );
  c_agseti("BACKGROUND.", 3 );

  c_agsetf("GRID/LEFT."  ,.15);
  c_agsetf("GRID/RIGHT." ,.90);
  
  for( i = 0; i < nGraphs; i++ ) {
    Ybottom[i] = .08 + 0.02 + i*0.84/nGraphs;
    Ytop[i] = .08 - 0.02 + (i+1)*0.84/nGraphs;
  }  
  ch = (Ytop[0] - Ybottom[0]);
  ch = .02/ch;

  c_agsetf("X/MINIMUM.", Xmin);
  c_agsetf("X/MAXIMUM.", Xmax);

  c_agsetc("LABEL/NAME.","T");
  c_agseti("LINE/NUMBER.",100);
  c_agsetf("LINE/CH.", 0.1 );

  c_agsetc("LABEL/NAME.","B");
  c_agseti("LINE/NUMBER.",-100);
  c_agsetc("LINE/TEXT.", " " );

  c_agsetf("BOTTOM/MAJOR/OUTWARD.", .02 ); 
  c_agsetf("BOTTOM/WIDTH/MA.", 0.20 );
  c_agsetf("BOTTOM/WIDTH/EX.", 0.15 );

  c_agsetc("LABEL/NAME.","L");
  c_agseti("LINE/NUMBER.",100);
  c_agsetc("LINE/TEXT.", " " );

  c_agseti("LEFT/MAJOR/TYPE.", 1 );
  c_agsetf("LEFT/MAJOR/OUTWARD.", .02 ); 
  c_agseti("LEFT/MINOR/SPACING.",4);
  c_agsetf("LEFT/WIDTH/MA.", .7*ch );
  c_agsetf("LEFT/WIDTH/EX.", .5*ch );
  
  c_agsetc("LABEL/NAME.", status );
  c_agsetf("LABEL/BASEPOINT/X.", 0.5);
  c_agsetf("LABEL/BASEPOINT/Y.", 1+2*ch);
  c_agseti("LABEL/ANGLE.", 0);
  c_agseti("LINE/NUMBER.", 0);
  c_agsetc("LINE/TEXT.", startMsg );
  c_agsetf("LINE/CH.", ch );      

  Boundary();
}

float Round( float x )
{
float p;
 
  if( x == 0 ) return x;
  p = (float)pow( 10.0, -3.0 + (int)(.5+log10( (double)x ) ) );
  return p * (int)(.5 + x/p); 
}


void UpdateGraph( float * val )
{
int i, j, n;
static int init = 1;
Gint err, oldcolor;
int start;
float v;

  if( nCrt >= nMax ) return;
  n = nMax+1; 

  for( i = 0; i < nGraphs; i++ )
    Yval[i*n+nCrt] = val[i];
  nCrt++;

  start = nGraphs-1;
  
  if( init ) {
    init = 0;
    ginq_text_colr_ind( &err, &oldcolor );

    c_pcloqu( 0.9, 0.03 , "TIME [hours]", -0.9, 0, 0 );
    c_pcloqu( 0.08,0.93, "CONC [ppb]", -0.8, 0, 0 );
      
    for( i = 0; i < nGraphs; i++ ) {
      v = val[i] == 0 ? .001 : val[i];  
      Ymin[i] = Round( v * (1 - Ymin[i]) );
      Ymax[i] = Round( v * (1 + Ymax[i]) );
/*
      Ybase[i] = Round((Ymax[i] - Ymin[i])/2);
      Ymin[i] = Ybase[i]*(int)(.5 + Ymin[i]/Ybase[i]);
      Ymax[i] = Ymin[i]+2*Ybase[i];
*/
      gset_text_colr_ind( i % MAX_COLORS + 2 );
      c_pcloqu( .86, Ytop[i]-0.01, graphName[i], -1.2, 0, -1 );
    }
    gupd_ws( WS_ID, GUPD_PEND );

    gset_text_colr_ind( oldcolor );

    SelectGraph(start);
    c_ezxy ( Xval, &Yval[start*n], nCrt, "" );  
    c_agsetc("LABEL/NAME.", status );
    c_agsetf("LABEL/SU.", 1.);
    start--;
  }
  
  for( i = start; i >=0; i-- ) {
    SelectGraph( i );
    c_ezxy ( Xval, &Yval[i*n], nCrt, "" );  
  }    
}

void CloseWin()
{
  c_agsetc("LABEL/NAME.", status );
  c_agsetf("LABEL/SU.", 0. );
  
  clean = 1;
  SelectGraph( nGraphs - 1 );
  c_ezxy ( Xval, &Yval[(nGraphs - 1)*(nMax+1)], nCrt, "" );   
  clean = 0;

  c_agsetc("LABEL/NAME.", status );
  c_agseti("LINE/NUMBER.", 0);
  c_agsetc("LINE/TEXT.", endMsg );

  c_ezxy ( Xval, &Yval[(nGraphs - 1)*(nMax+1)], nCrt, "" );   

  getchar();
  c_clsgks();
}

void Boundary()
{
  c_plotif(    0,    0,0);
  c_plotif(32767,    0,1);
  c_plotif(32767,32767,1);
  c_plotif(    0,32767,1);
  c_plotif(    0,    0,1);
}


void agchcu( int * iflag, int * n )
{
  c_plotif( 0., 0., 2 );
  if( *iflag == 0 ) 
    gset_line_colr_ind( CrtGraph % MAX_COLORS + 2 );
  else 
    gset_line_colr_ind( 1 );
}

int CmpLabelName( char * s1, char * s2 ) 
{
  while ( isspace( *s1 ) ) s1++;
  while( *s1 == *s2 ) {
    s1++; s2++;
  }  
  if( *s2 == '\0' )
    return 1;
  return 0;  
}


void agchil( int * iflag, char * lname, int * lnum ) 
{
  c_plotif( 0., 0., 2 );
  switch( *iflag ) {
    case 0:
            if( CmpLabelName( lname, status ) )
              gset_text_colr_ind( 1 - clean );
    	    break;
    case 1: gset_text_colr_ind( 1 );
            break;
  }          
}
