/*
 * Antisymmetric and symmetric biorthogonal quadrature filter coefficients.
 * 
 * Copyright (C) 1991--94 Wickerhauser Consulting.  All Rights Reserved.
 * May be freely copied for noncommercial use.  See
 * ``Adapted Wavelet Analysis from Theory to Software'' ISBN 1-56881-041-5
 * by Mladen Victor Wickerhauser [AK Peters, Ltd., Wellesley, Mass., 1994]
 */


#ifndef ASBQFS_HDR_ALREADY_INCLUDED
#define ASBQFS_HDR_ALREADY_INCLUDED

#include "real.h"

/*************************************************************************
 * Antisymmetric (or symmetric) biorthogonal conjugate quadrature filters.
 *
 * Source: 
 *  Ingrid Daubechies, "Ten Lectures on Wavelets", p.277
 *  SIAM Press, Philadelphia, (1992) ISBN 0-89871-274-2 
 */

#define SQ2    (1.414213562373095) /* sqrt(2.0) */
#define SQ2OVER2E17 (SQ2/131072.0) /* sqrt(2.0)/(1<<17) */
#define SQ2OVER2E15 (SQ2/32768.0) /* sqrt(2.0)/(1<<15) */
#define SQ2OVER2E14 (SQ2/16384.0) /* sqrt(2.0)/(1<<14) */

static real
  d1hbqf[2] = { 
    (1.0/2.0)*SQ2,
    
    (1.0/2.0)*SQ2
    };
static int
  d1halpha = -1,
  d1homega = 0;

static real
  d1gbqf[2] = {
    (1.0/2.0)*SQ2,
    
    (-1.0/2.0)*SQ2 
    };
static int 
  d1galpha = -1,
  d1gomega = 0;

static real
  d1h1bqf[2] = {
    (1.0/2.0)*SQ2,
    
    (1.0/2.0)*SQ2 
    };
static int
  d1h1alpha = -1,
  d1h1omega = 0;

static real
  d1g1bqf[2] = {
    (1.0/2.0)*SQ2,
    
    (-1.0/2.0)*SQ2
    };
static int
  d1g1alpha = -1,
  d1g1omega = 0;

static real
  d1h3bqf[6] = {
    (-1.0/16.0)*SQ2,
    (1.0/16.0)*SQ2,
    (1.0/2.0)*SQ2,
    
    (1.0/2.0)*SQ2,
    (1.0/16.0)*SQ2,
    (-1.0/16.0)*SQ2
    };
static int
  d1h3alpha = -3,
  d1h3omega = 2;

static real
  d1g3bqf[6] = {
    (-1.0/16.0)*SQ2,
    (-1.0/16.0)*SQ2,
    (1.0/2.0)*SQ2,
    
    (-1.0/2.0)*SQ2,
    (1.0/16.0)*SQ2,
    (1.0/16.0)*SQ2
    };
static int
  d1g3alpha = -3,
  d1g3omega = 2;

static real
  d1h5bqf[10] = { 
    (3.0/256.0)*SQ2,
    (-3.0/256.0)*SQ2,
    (-11.0/128.0)*SQ2,
    (11.0/128.0)*SQ2,
    (1.0/2.0)*SQ2,
    
    (1.0/2.0)*SQ2,
    (11.0/128.0)*SQ2,
    (-11.0/128.0)*SQ2,
    (-3.0/256.0)*SQ2,
    (3.0/256.0)*SQ2
    };
static int
  d1h5alpha = -5,
  d1h5omega = 4;

static real
  d1g5bqf[10] = { 
    (3.0/256.0)*SQ2,
    (3.0/256.0)*SQ2,
    (-11.0/128.0)*SQ2,
    (-11.0/128.0)*SQ2,
    (1.0/2.0)*SQ2,
    
    (-1.0/2.0)*SQ2,
    (11.0/128.0)*SQ2,
    (11.0/128.0)*SQ2,
    (-3.0/256.0)*SQ2,
    (-3.0/256.0)*SQ2
    };
static int
  d1g5alpha = -5,
  d1g5omega = 4;

static real
  d2hbqf[3] = {
    (1.0/4.0)*SQ2,
    (1.0/2.0)*SQ2,
    (1.0/4.0)*SQ2
    };
static int
  d2halpha = -1,
  d2homega = 1;

static real
  d2gbqf[3] = {
    (-1.0/4.0)*SQ2,
    (1.0/2.0)*SQ2,
    (-1.0/4.0)*SQ2
    };
static int
  d2galpha = 0,
  d2gomega = 2;

static real
  d2h2bqf[5] = {
    (-1.0/8.0)*SQ2,
    (1.0/4.0)*SQ2,
    (3.0/4.0)*SQ2,
    (1.0/4.0)*SQ2,
    (-1.0/8.0)*SQ2
    };
static int
  d2h2alpha = -2,
  d2h2omega = 2;

static real
  d2g2bqf[5] = {
    (-1.0/8.0)*SQ2,
    (-1.0/4.0)*SQ2,
    (3.0/4.0)*SQ2,
    (-1.0/4.0)*SQ2,
    (-1.0/8.0)*SQ2
    };
static int
  d2g2alpha = -1,
  d2g2omega = 3;

static real
  d2h4bqf[9] = {
    (3.0/128.0)*SQ2,
    (-3.0/64.0)*SQ2,
    (-1.0/8.0)*SQ2,
    (19.0/64.0)*SQ2,
    (45.0/64.0)*SQ2,
    (19.0/64.0)*SQ2,
    (-1.0/8.0)*SQ2,
    (-3.0/64.0)*SQ2,
    (3.0/128.0)*SQ2
    };
static int
  d2h4alpha = -4,
  d2h4omega = 4;

static real
  d2g4bqf[9] = {
    (3.0/128.0)*SQ2,
    (3.0/64.0)*SQ2,
    (-1.0/8.0)*SQ2,
    (-19.0/64.0)*SQ2,
    (45.0/64.0)*SQ2,
    (-19.0/64.0)*SQ2,
    (-1.0/8.0)*SQ2,
    (3.0/64.0)*SQ2,
    (3.0/128.0)*SQ2
    };
static int
  d2g4alpha = -3,
  d2g4omega = 5;

static real
  d2h6bqf[13] = {
    (-5.0/1024.0)*SQ2,
    (5.0/512.0)*SQ2,
    (17.0/512.0)*SQ2,
    (-39.0/512.0)*SQ2,
    (-123.0/1024.0)*SQ2,
    (81.0/256.0)*SQ2,
    
    (175.0/256.0)*SQ2,
    
    (81.0/256.0)*SQ2,
    (-123.0/1024.0)*SQ2,
    (-39.0/512.0)*SQ2,
    (17.0/512.0)*SQ2,
    (5.0/512.0)*SQ2,
    (-5.0/1024.0)*SQ2
    };
static int
  d2h6alpha = -6,
  d2h6omega = 6;

static real
  d2g6bqf[13] = {
    (-5.0/1024.0)*SQ2,
    (-5.0/512.0)*SQ2,
    (17.0/512.0)*SQ2,
    (39.0/512.0)*SQ2,
    (-123.0/1024.0)*SQ2,
    (-81.0/256.0)*SQ2,
    
    (175.0/256.0)*SQ2,
    
    (-81.0/256.0)*SQ2,
    (-123.0/1024.0)*SQ2,
    (39.0/512.0)*SQ2,
    (17.0/512.0)*SQ2,
    (-5.0/512.0)*SQ2,
    (-5.0/1024.0)*SQ2
    };
static int
  d2g6alpha = -5,
  d2g6omega = 7;

static real
  d2h8bqf[17] = {
    35.0*SQ2OVER2E15,
    -70.0*SQ2OVER2E15,
    -300.0*SQ2OVER2E15,
    670.0*SQ2OVER2E15,
    1228.0*SQ2OVER2E15,
    -3126.0*SQ2OVER2E15,
    -3796.0*SQ2OVER2E15,
    10718.0*SQ2OVER2E15,
    
    22050.0*SQ2OVER2E15,
    
    10718.0*SQ2OVER2E15,
    -3796.0*SQ2OVER2E15,
    -3126.0*SQ2OVER2E15,
    1228.0*SQ2OVER2E15,
    670.0*SQ2OVER2E15,
    -300.0*SQ2OVER2E15,
    -70.0*SQ2OVER2E15,
    35.0*SQ2OVER2E15
    };
static int
  d2h8alpha = -8,
  d2h8omega = 8;

static real
  d2g8bqf[17] = {
    35.0*SQ2OVER2E15,
    70.0*SQ2OVER2E15,
    -300.0*SQ2OVER2E15,
    -670.0*SQ2OVER2E15,
    1228.0*SQ2OVER2E15,
    3126.0*SQ2OVER2E15,
    -3796.0*SQ2OVER2E15,
    -10718.0*SQ2OVER2E15,
    
    22050.0*SQ2OVER2E15,
    
    -10718.0*SQ2OVER2E15,
    -3796.0*SQ2OVER2E15,
    3126.0*SQ2OVER2E15,
    1228.0*SQ2OVER2E15,
    -670.0*SQ2OVER2E15,
    -300.0*SQ2OVER2E15,
    70.0*SQ2OVER2E15,
    35.0*SQ2OVER2E15
    };
static int
  d2g8alpha = -7,
  d2g8omega = 9;

static real
  d3hbqf[4] = {
    (1.0/8.0)*SQ2,
    (3.0/8.0)*SQ2,
    
    (3.0/8.0)*SQ2,
    (1.0/8.0)*SQ2
    };
static int
  d3halpha = -2,
  d3homega = 1;

static real
  d3gbqf[4] = {
    (-1.0/8.0)*SQ2,
    (3.0/8.0)*SQ2,
    
    (-3.0/8.0)*SQ2,
    (1.0/8.0)*SQ2
    };
static int
  d3galpha = -2,
  d3gomega = 1;

static real
  d3h1bqf[4] = {
    (-1.0/4.0)*SQ2,
    (3.0/4.0)*SQ2,
    
    (3.0/4.0)*SQ2,
    (-1.0/4.0)*SQ2
    };
static int
  d3h1alpha = -2,
  d3h1omega = 1;

static real
  d3g1bqf[4] = {
    (1.0/4.0)*SQ2,
    (3.0/4.0)*SQ2,
    
    (-3.0/4.0)*SQ2,
    (-1.0/4.0)*SQ2
    };
static int
  d3g1alpha = -2,
  d3g1omega = 1;

static real
  d3h3bqf[8] = {
    (3.0/64.0)*SQ2,
    (-9.0/64.0)*SQ2,
    (-7.0/64.0)*SQ2,
    (45.0/64.0)*SQ2,
    
    (45.0/64.0)*SQ2,
    (-7.0/64.0)*SQ2,
    (-9.0/64.0)*SQ2,
    (3.0/64.0)*SQ2,
  };
static int
  d3h3alpha = -4,
  d3h3omega = 3;

static real
  d3g3bqf[8] = {
    (-3.0/64.0)*SQ2,
    (-9.0/64.0)*SQ2,
    (7.0/64.0)*SQ2,
    (45.0/64.0)*SQ2,
    
    (-45.0/64.0)*SQ2,
    (-7.0/64.0)*SQ2,
    (9.0/64.0)*SQ2,
    (3.0/64.0)*SQ2,
  };
static int
  d3g3alpha = -4,
  d3g3omega = 3;

static real
  d3h5bqf[12] = {
    (-5.0/512.0)*SQ2,
    (15.0/512.0)*SQ2,
    (19.0/512.0)*SQ2,
    (-97.0/512.0)*SQ2,
    (-13.0/256.0)*SQ2,
    (175.0/256.0)*SQ2,
    
    (175.0/256.0)*SQ2,
    (-13.0/256.0)*SQ2,
    (-97.0/512.0)*SQ2,
    (19.0/512.0)*SQ2,
    (15.0/512.0)*SQ2,
    (-5.0/512.0)*SQ2
    };
static int
  d3h5alpha = -6,
  d3h5omega = 5;

static real
  d3g5bqf[12] = {
    (5.0/512.0)*SQ2,
    (15.0/512.0)*SQ2,
    (-19.0/512.0)*SQ2,
    (-97.0/512.0)*SQ2,
    (13.0/256.0)*SQ2,
    (175.0/256.0)*SQ2,
    
    (-175.0/256.0)*SQ2,
    (-13.0/256.0)*SQ2,
    (97.0/512.0)*SQ2,
    (19.0/512.0)*SQ2,
    (-15.0/512.0)*SQ2,
    (-5.0/512.0)*SQ2
    };
static int
  d3g5alpha = -6,
  d3g5omega = 5;

static real
  d3h7bqf[16] = {
    (35.0)*SQ2OVER2E14,
    (-105.0)*SQ2OVER2E14,
    (-195.0)*SQ2OVER2E14,
    (865.0)*SQ2OVER2E14,
    (363.0)*SQ2OVER2E14,
    (-3489.0)*SQ2OVER2E14,
    (-307.0)*SQ2OVER2E14,
    (11025.0)*SQ2OVER2E14,
    
    (11025.0)*SQ2OVER2E14,
    (-307.0)*SQ2OVER2E14,
    (-3489.0)*SQ2OVER2E14,
    (363.0)*SQ2OVER2E14,
    (865.0)*SQ2OVER2E14,
    (-195.0)*SQ2OVER2E14,
    (-105.0)*SQ2OVER2E14,
    (35.0)*SQ2OVER2E14
    };
static int
  d3h7alpha = -8,
  d3h7omega = 7;

static real
  d3g7bqf[16] = {
    (-35.0)*SQ2OVER2E14,
    (-105.0)*SQ2OVER2E14,
    (195.0)*SQ2OVER2E14,
    (865.0)*SQ2OVER2E14,
    (-363.0)*SQ2OVER2E14,
    (-3489.0)*SQ2OVER2E14,
    (307.0)*SQ2OVER2E14,
    (11025.0)*SQ2OVER2E14,
    
    (-11025.0)*SQ2OVER2E14,
    (-307.0)*SQ2OVER2E14,
    (3489.0)*SQ2OVER2E14,
    (363.0)*SQ2OVER2E14,
    (-865.0)*SQ2OVER2E14,
    (-195.0)*SQ2OVER2E14,
    (105.0)*SQ2OVER2E14,
    (35.0)*SQ2OVER2E14
    };
static int
  d3g7alpha = -8,
  d3g7omega = 7;

static real
  d3h9bqf[20] = {
    (-63.0)*SQ2OVER2E17,
    (189.0)*SQ2OVER2E17,
    (469.0)*SQ2OVER2E17,
    (-1911.0)*SQ2OVER2E17,
    (-1308.0)*SQ2OVER2E17,
    (9188.0)*SQ2OVER2E17,
    (1140.0)*SQ2OVER2E17,
    (-29676.0)*SQ2OVER2E17,
    (190.0)*SQ2OVER2E17,
    (87318.0)*SQ2OVER2E17,
    
    (87318.0)*SQ2OVER2E17,
    (190.0)*SQ2OVER2E17,
    (-29676.0)*SQ2OVER2E17,
    (1140.0)*SQ2OVER2E17,
    (9188.0)*SQ2OVER2E17,
    (-1308.0)*SQ2OVER2E17,
    (-1911.0)*SQ2OVER2E17,
    (469.0)*SQ2OVER2E17,
    (189.0)*SQ2OVER2E17,
    (-63.0)*SQ2OVER2E17
    };
static int
  d3h9alpha = -10,
  d3h9omega = 9;

static real
  d3g9bqf[20] = {
    (63.0)*SQ2OVER2E17,
    (189.0)*SQ2OVER2E17,
    (-469.0)*SQ2OVER2E17,
    (-1911.0)*SQ2OVER2E17,
    (1308.0)*SQ2OVER2E17,
    (9188.0)*SQ2OVER2E17,
    (-1140.0)*SQ2OVER2E17,
    (-29676.0)*SQ2OVER2E17,
    (-190.0)*SQ2OVER2E17,
    (87318.0)*SQ2OVER2E17,
    
    (-87318.0)*SQ2OVER2E17,
    (190.0)*SQ2OVER2E17,
    (29676.0)*SQ2OVER2E17,
    (1140.0)*SQ2OVER2E17,
    (-9188.0)*SQ2OVER2E17,
    (-1308.0)*SQ2OVER2E17,
    (1911.0)*SQ2OVER2E17,
    (469.0)*SQ2OVER2E17,
    (-189.0)*SQ2OVER2E17,
    (-63.0)*SQ2OVER2E17
    };
static int
  d3g9alpha = -10,
  d3g9omega = 9;

#undef SQ2
#undef SQ2OVER2E14
#undef SQ2OVER2E15
#undef SQ2OVER2E17

#endif				/* ASBQFS_HDR_ALREADY_INCLUDED */
