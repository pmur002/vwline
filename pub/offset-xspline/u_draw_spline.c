/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985-1988 by Supoj Sutanthavibul
 * Parts Copyright (c) 1989-2007 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 *
 * Any party obtaining a copy of these files is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and documentation
 * files (the "Software"), including without limitation the rights to use,
 * copy, modify, merge, publish distribute, sublicense and/or sell copies of
 * the Software, and to permit persons who receive copies from any such
 * party to do so, with the only requirement being that the above copyright
 * and this permission notice remain intact.
 *
 */

/* THIS FILE IS #included FROM u_draw.c and u_geom.c */

#include "u_draw.h"
#include "u_list.h"

/********************* CURVES FOR SPLINES *****************************

 The following spline drawing routines are from

    "X-splines : A Spline Model Designed for the End User"

    by Carole BLANC and Christophe SCHLICK,
    in Proceedings of SIGGRAPH ' 95

***********************************************************************/

#define         HIGH_PRECISION    0.5
#define         LOW_PRECISION     1.0
#define         ZOOM_PRECISION    5.0
#define         ARROW_START       4
#define         MAX_SPLINE_STEP   0.2

/***********************************************************************/

#define Q(s)  (-(s))
#define EQN_NUMERATOR(dim) \
  (A_blend[0]*p0->dim+A_blend[1]*p1->dim+A_blend[2]*p2->dim+A_blend[3]*p3->dim)



static inline double
f_blend(double numerator, double denominator)
{
  double p = 2 * denominator * denominator;
  double u = numerator / denominator;
  double u2 = u * u;

  return (u * u2 * (10 - p + (2*p - 15)*u + (6 - p)*u2));
}

static inline double
g_blend(double u, double q)             /* p equals 2 */

{
  return(u*(q + u*(2*q + u*(8 - 12*q + u*(14*q - 11 + u*(4 - 5*q))))));
}

static inline double
h_blend(double u, double q)
{
  double u2=u*u;
   return (u * (q + u * (2 * q + u2 * (-2*q - u*q))));
}

static inline
void negative_s1_influence(double t, double s1, double *A0, double *A2)
{
  *A0 = h_blend(-t, Q(s1));
  *A2 = g_blend(t, Q(s1));
}

static inline
void negative_s2_influence(double t, double s2, double *A1, double *A3)
{
  *A1 = g_blend(1-t, Q(s2));
  *A3 = h_blend(t-1, Q(s2));
}

static inline
void positive_s1_influence(int k, double t, double s1, double *A0, double *A2)
{
  double Tk;

  Tk = k+1+s1;
  *A0 = (t+k+1<Tk) ? f_blend(t+k+1-Tk, k-Tk) : 0.0;

  Tk = k+1-s1;
  *A2 = f_blend(t+k+1-Tk, k+2-Tk);
}

static inline
void positive_s2_influence(int k, double t, double s2, double *A1, double *A3)
{
  double Tk;

  Tk = k+2+s2;
  *A1 = f_blend(t+k+1-Tk, k+1-Tk);

  Tk = k+2-s2;
  *A3 = (t+k+1>Tk) ? f_blend(t+k+1-Tk, k+3-Tk) : 0.0;
}

static inline
void point_adding(double *A_blend, F_point *p0, F_point *p1, F_point *p2, F_point *p3)
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];
  if (!add_point(round(EQN_NUMERATOR(x) / (weights_sum)),
		 round(EQN_NUMERATOR(y) / (weights_sum))))
      too_many_points();
}

static inline
void point_computing(double *A_blend, F_point *p0, F_point *p1, F_point *p2, F_point *p3, int *x, int *y)
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];

  *x = round(EQN_NUMERATOR(x) / (weights_sum));
  *y = round(EQN_NUMERATOR(y) / (weights_sum));
}

static float
step_computing(int k, F_point *p0, F_point *p1, F_point *p2, F_point *p3, double s1, double s2, float precision)
{
  double A_blend[4];
  int    xstart, ystart, xend, yend, xmid, ymid, xlength, ylength;
  int    start_to_end_dist, number_of_steps;
  float  step, angle_cos, scal_prod, xv1, xv2, yv1, yv2, sides_length_prod;

  /* This function computes the step used to draw the segment (p1, p2)
     (xv1, yv1) : coordinates of the vector from middle to origin
     (xv2, yv2) : coordinates of the vector from middle to extremity */

  if ((s1 == 0) && (s2 == 0))
    return(1.0);              /* only one step in case of linear segment */

  /* compute coordinates of the origin */
  if (s1>0) {
      if (s2<0) {
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  negative_s2_influence(0.0, s2, &A_blend[1], &A_blend[3]);
      } else {
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.0, s2, &A_blend[1], &A_blend[3]);
      }
      point_computing(A_blend, p0, p1, p2, p3, &xstart, &ystart);
  } else {
      xstart = p1->x;
      ystart = p1->y;
  }

  /* compute coordinates  of the extremity */
  if (s2>0) {
      if (s1<0) {
	  negative_s1_influence(1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]);
      } else {
	  positive_s1_influence(k, 1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]);
      }
      point_computing(A_blend, p0, p1, p2, p3, &xend, &yend);
  } else {
      xend = p2->x;
      yend = p2->y;
  }

  /* compute coordinates  of the middle */
  if (s2>0) {
      if (s1<0) {
	  negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]);
      } else {
	  positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]);
	}
  } else if (s1<0) {
      negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
  } else {
      positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
  }
  point_computing(A_blend, p0, p1, p2, p3, &xmid, &ymid);

  xv1 = xstart - xmid;
  yv1 = ystart - ymid;
  xv2 = xend - xmid;
  yv2 = yend - ymid;

  scal_prod = xv1*xv2 + yv1*yv2;

  sides_length_prod = sqrt((xv1*xv1 + yv1*yv1)*(xv2*xv2 + yv2*yv2));

  /* compute cosinus of origin-middle-extremity angle, which approximates the
     curve of the spline segment */
  if (sides_length_prod == 0.0)
    angle_cos = 0.0;
  else
    angle_cos = scal_prod/sides_length_prod;

  xlength = xend - xstart;
  ylength = yend - ystart;

  start_to_end_dist = sqrt((double)xlength*(double)xlength + (double)ylength*(double)ylength);

  /* more steps if segment's origin and extremity are remote */
  number_of_steps = sqrt(start_to_end_dist)/2;

  /* more steps if the curve is high */
  number_of_steps += (int)((1 + angle_cos)*10);

  if (number_of_steps == 0)
    step = 1;
  else
    step = precision/number_of_steps;

  if ((step > MAX_SPLINE_STEP) || (step == 0))
    step = MAX_SPLINE_STEP;
  return (step);
}

static void
spline_segment_computing(float step, int k, F_point *p0, F_point *p1, F_point *p2, F_point *p3, double s1, double s2)
{
  double A_blend[4];
  double t;

  if (s1<0) {
     if (s2<0) {
	 for (t=0.0 ; t<1 ; t+=step) {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	 }
     } else {
	 for (t=0.0 ; t<1 ; t+=step) {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	 }
     }
  } else if (s2<0) {
      for (t=0.0 ; t<1 ; t+=step) {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
      }
  } else {
      for (t=0.0 ; t<1 ; t+=step) {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
      }
  }
}

/********************* MAIN METHODS *************************************/

#define COPY_CONTROL_POINT(P0, S0, P1, S1) \
      P0 = P1; \
      S0 = S1

#define NEXT_CONTROL_POINTS(P0, S0, P1, S1, P2, S2, P3, S3) \
      COPY_CONTROL_POINT(P0, S0, P1, S1); \
      COPY_CONTROL_POINT(P1, S1, P2, S2); \
      COPY_CONTROL_POINT(P2, S2, P3, S3); \
      COPY_CONTROL_POINT(P3, S3, P3->next, S3->next)

#define INIT_CONTROL_POINTS(SPLINE, P0, S0, P1, S1, P2, S2, P3, S3) \
      COPY_CONTROL_POINT(P0, S0, SPLINE->points, SPLINE->sfactors); \
      COPY_CONTROL_POINT(P1, S1, P0->next, S0->next);               \
      COPY_CONTROL_POINT(P2, S2, P1->next, S1->next);               \
      COPY_CONTROL_POINT(P3, S3, P2->next, S2->next)

#define SPLINE_SEGMENT_LOOP(K, P0, P1, P2, P3, S1, S2, PREC) \
      step = step_computing(K, P0, P1, P2, P3, S1, S2, PREC);    \
      spline_segment_computing(step, K, P0, P1, P2, P3, S1, S2)

static Boolean DONE;

static Boolean
compute_open_spline(F_spline *spline, float precision)
{
  int       k;
  float     step;
  F_point   *p0, *p1, *p2, *p3;
  F_sfactor *s0, *s1, *s2, *s3;

  init_point_array();

  DONE = False;

  COPY_CONTROL_POINT(p0, s0, spline->points, spline->sfactors);
  COPY_CONTROL_POINT(p1, s1, p0, s0);
  /* first control point is needed twice for the first segment */
  COPY_CONTROL_POINT(p2, s2, p1->next, s1->next);

  if (p2->next == NULL) {
      COPY_CONTROL_POINT(p3, s3, p2, s2);
  } else {
      COPY_CONTROL_POINT(p3, s3, p2->next, s2->next);
  }


  for (k = 0 ;  ; k++) {
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
      if (DONE)
        break;
      if (p3->next == NULL)
	break;
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
  }
  /* last control point is needed twice for the last segment */
  if (!DONE) {
    COPY_CONTROL_POINT(p0, s0, p1, s1);
    COPY_CONTROL_POINT(p1, s1, p2, s2);
    COPY_CONTROL_POINT(p2, s2, p3, s3);
    SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
  }

  if (!add_point(p3->x, p3->y))
    too_many_points();

  return True;
}

static Boolean
compute_closed_spline(F_spline *spline, float precision)
{
  int k, i;
  float     step;
  F_point   *p0, *p1, *p2, *p3, *first;
  F_sfactor *s0, *s1, *s2, *s3, *s_first;

  init_point_array();

  DONE = False;

  INIT_CONTROL_POINTS(spline, p0, s0, p1, s1, p2, s2, p3, s3);
  COPY_CONTROL_POINT(first, s_first, p0, s0);

  for (k = 0 ; p3 != NULL ; k++) {
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
      if (DONE)
        break;
  }
  /* when we are at the end, join to the beginning */
  if (!DONE) {
    COPY_CONTROL_POINT(p3, s3, first, s_first);
    SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);

    for (i=0; i<2; i++) {
        if (DONE)
          break;
        k++;
        NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
        SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
    }
  }

  if (!add_closepoint())
    too_many_points();

  return True;
}




