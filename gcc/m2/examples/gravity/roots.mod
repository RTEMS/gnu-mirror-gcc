(* Copyright (C) 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE roots ;

FROM libc IMPORT printf ;
FROM libm IMPORT pow, sin, cos, atan2 ;
IMPORT libm ;
IMPORT ComplexMath ;
FROM ComplexMath IMPORT zero ;


CONST
   Epsilon = 0.000001 ;


(*
   nearZero - returns TRUE if, r, is close to 0.0
*)

PROCEDURE nearZero (r: REAL) : BOOLEAN ;
BEGIN
   IF r>=0.0
   THEN
      RETURN r<Epsilon
   ELSE
      RETURN (-r)<Epsilon
   END
END nearZero ;


(*
   cnearZero - returns TRUE if the magnitude of, c, is close to 0.
*)

PROCEDURE cnearZero (c: COMPLEX) : BOOLEAN ;
VAR
   r: REAL ;
BEGIN
   r := rsqrt(sqr(RE(c))+sqr(IM(c))) ;
   RETURN nearZero(r)
END cnearZero ;


(*
   isReal - returns TRUE if the imaginary component of, c, is near zero.
*)

PROCEDURE isReal (c: COMPLEX) : BOOLEAN ;
BEGIN
   RETURN nearZero(IM(c))
END isReal ;


(*
   rsqrt - useful for debugging
*)

PROCEDURE rsqrt (r: REAL) : REAL ;
BEGIN
   RETURN libm.sqrt(r)
END rsqrt ;


(*
   sqr - returns r^2
*)

PROCEDURE sqr (r: REAL) : REAL ;
BEGIN
   RETURN r*r
END sqr ;


(*
   cub - returns r^3
*)

PROCEDURE cub (r: REAL) : REAL ;
BEGIN
   RETURN r*r*r
END cub ;


(*
   cubr - returns r^(1/3)
*)

PROCEDURE cubr (r: REAL) : REAL ;
BEGIN
   IF r<0.0
   THEN
      RETURN -pow(-r, 1.0/3.0)
   ELSE
      RETURN pow(r, 1.0/3.0)
   END
END cubr ;


(*
   cpower - 
*)

PROCEDURE cpower (c: COMPLEX; r: REAL) : COMPLEX ;
VAR
   radiusr,
   radius ,
   angle  : REAL ;
BEGIN
   (*
    *  use Abraham de Moivre's formula which states that:
    *
    *  (cos(x) + i sin(x))^r = cos(rx) + i sin(rx) 
    *)

   radius := rsqrt(RE(c) * RE(c) + IM(c) * IM(c)) ;
   angle := atan2(IM(c), RE(c)) ;
   radiusr := pow(radius, r) ;

   RETURN CMPLX(radiusr * cos(angle*r),
                radiusr * sin(angle*r))
END cpower ;


(*
   csqrt - useful for debugging
*)

PROCEDURE csqrt (c: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cpower(c, 1.0/2.0)
END csqrt ;


(*
   csqr - returns c^2
*)

PROCEDURE csqr (c: COMPLEX) : COMPLEX ;
BEGIN
   RETURN c*c
END csqr ;


(*
   ccub - returns c^3
*)

PROCEDURE ccub (c: COMPLEX) : COMPLEX ;
BEGIN
   RETURN c*c*c
END ccub ;


(*
   ccubr - returns c^(1/3)
*)

PROCEDURE ccubr (c: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cpower(c, 1.0/3.0)
END ccubr ;


(*
   findQuadratic - returns TRUE if a scalar values of x can be found
                   for.

                     2
                   ax  +  bx + c == 0
*)

PROCEDURE findQuadratic (a, b, c: REAL; VAR x0, x1: REAL) : BOOLEAN ;
VAR
   q, discriminant: REAL ;
BEGIN
   IF a=0.0
   THEN
      RETURN FALSE
   ELSE
      discriminant := sqr(b) - 4.0*a*c ;
      IF discriminant>0.0
      THEN
         q := rsqrt(discriminant) ;
         x0 := (-b + q) / (2.0 * a) ;
         x1 := (-b - q) / (2.0 * a) ;
         RETURN TRUE
      ELSIF discriminant=0.0
      THEN
         x0 := -b / (2.0 * a) ;
         x1 := x0 ;
         RETURN TRUE
      ELSE
         RETURN FALSE
      END
   END
END findQuadratic ;


(*
   findCubic - returns TRUE if a root can be found for

                 3      2
               ax  +  bx  +  cx  +  d  ==  0

               if returns TRUE if it finds a root >= 0
               and will return the smallest root >= 0
*)

PROCEDURE findCubic (a, b, c, d: REAL; VAR x: REAL) : BOOLEAN ;
VAR
   i, j: CARDINAL ;
   X   : ARRAY [1..2] OF REAL ;
BEGIN
   i := 0 ;
   IF a=0.0
   THEN
      IF findQuadratic(b, c, d, X[1], X[2])
      THEN
         i := 2
      END
   ELSE
      i := 1 ;
      X[1] := -b/(3.0*a)
              -1.0/(3.0*a) * cubr((2.0*cub(b)-9.0*a*b*c+27.0*sqr(a)*d+rsqrt(sqr(2.0*cub(b)-9.0*a*b*c+27.0*sqr(a)*d)-4.0*cub(sqr(b)-3.0*a*c))))
              -1.0/(3.0*a) * cubr((2.0*cub(b)-9.0*a*b*c+27.0*sqr(a)*d-rsqrt(sqr(2.0*cub(b)-9.0*a*b*c+27.0*sqr(a)*d)-4.0*cub(sqr(b)-3.0*a*c))))
   END ;
   j := 1 ;
   x := -1.0 ;
   WHILE j<=i DO
      IF (X[j]>=0.0) AND ((X[j]<x) OR (x<0.0))
      THEN
         x := X[j]
      END ;
      INC(j)
   END ;
   RETURN x>=0.0
END findCubic ;


(*
   findQuartic - returns TRUE if a scalar root can be found for:

                   4      3      2
                 ax  +  bx  +  cx  +  dx +  e  == 0

                 TRUE is only returned if a positive value for
                 x is found and it returns the smallest value for x.
*)

PROCEDURE findQuartic (a, b, c, d, e: REAL; VAR x: REAL) : BOOLEAN ;
VAR
   i, j                  : CARDINAL ;
   f, p, q,
   alpha, beta, gamma    : REAL ;
   r, u, w, y, z         : COMPLEX ;
   X                     : ARRAY [1..4] OF COMPLEX ;
BEGIN
   IF a=0.0
   THEN
      RETURN findCubic (b, c, d, e, x)
   ELSE
      alpha := -((3.0 * sqr(b)) / (8.0 * sqr(a))) + c/a ;
      beta  := (cub(b) / (8.0*cub(a))) - ((b * c) / (2.0 * sqr(a))) + d/a ;
      gamma := -(3.0*sqr(b)*sqr(b)) / (256.0 * sqr(a)*sqr(a)) + (c*sqr(b)) / (16.0 * cub(a)) - (b*d)/(4.0*sqr(a)) + e/a ;
      IF beta=0.0
      THEN
         X[1] := CMPLX(-(b / (4.0 * a)), 0.0) + csqrt((CMPLX(-alpha, 0.0) + csqrt(CMPLX(sqr(alpha)-4.0*gamma, 0.0)))/CMPLX(2.0, 0.0)) ;
         X[2] := CMPLX(-(b / (4.0 * a)), 0.0) + csqrt((CMPLX(-alpha, 0.0) - csqrt(CMPLX(sqr(alpha)-4.0*gamma, 0.0)))/CMPLX(2.0, 0.0)) ;
         X[3] := CMPLX(-(b / (4.0 * a)), 0.0) - csqrt((CMPLX(-alpha, 0.0) + csqrt(CMPLX(sqr(alpha)-4.0*gamma, 0.0)))/CMPLX(2.0, 0.0)) ;
         X[4] := CMPLX(-(b / (4.0 * a)), 0.0) - csqrt((CMPLX(-alpha, 0.0) - csqrt(CMPLX(sqr(alpha)-4.0*gamma, 0.0)))/CMPLX(2.0, 0.0))
      ELSE
         p := -sqr(alpha)/12.0 - gamma ;
         q := -(cub(alpha) / 108.0) + (alpha*gamma)/3.0 - sqr(beta)/8.0 ;
         f := sqr(q)/4.0+cub(p)/27.0 ;
         (* as f can be negative we must use complex arithmetic *)
         r := -CMPLX(q/2.0, 0.0) + csqrt(CMPLX(f, 0.0)) ;
         u := ccubr(r) ;
         IF cnearZero(u)
         THEN
            y := -CMPLX((5.0/6.0)*alpha, 0.0) + u - ccubr(CMPLX(q, 0.0))
         ELSE
            y := -CMPLX((5.0/6.0)*alpha, 0.0) + u - CMPLX(p, 0.0)/(CMPLX(3.0, 0.0)*u)
         END ;
         w := csqrt(CMPLX(alpha, 0.0) + CMPLX(+2.0, 0.0) * y) ;
         
         z := -(CMPLX(3.0 * alpha, 0.0) + CMPLX(2.0, 0.0) * y + (CMPLX(2.0 * beta, 0.0) / w)) ;
         X[1] := -(CMPLX(b/(4.0*a), 0.0)) + (w - csqrt(z)) / CMPLX(2.0, 0.0) ;
         X[2] := -(CMPLX(b/(4.0*a), 0.0)) + (w + csqrt(z)) / CMPLX(2.0, 0.0) ;

         z := -(CMPLX(3.0 * alpha, 0.0) + CMPLX(2.0, 0.0) * y - (CMPLX(2.0 * beta, 0.0) / w)) ;
         X[3] := -(CMPLX(b/(4.0*a), 0.0)) + (-w - csqrt(z)) / CMPLX(2.0, 0.0) ;
         X[4] := -(CMPLX(b/(4.0*a), 0.0)) + (-w + csqrt(z)) / CMPLX(2.0, 0.0) ;
      END ;

      x := -1.0 ;
      FOR i := 1 TO 4 DO
         IF isReal(X[i]) AND (RE(X[i])>=0.0) AND ((RE(X[i])<x) OR (x<0.0))
         THEN
            x := RE(X[i])
         END
      END ;
      RETURN x>=0.0
   END
END findQuartic ;


(*
   Assert - 
*)

PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      printf("assert failed\n")
   END
END Assert ;


PROCEDURE test ;
VAR
   A, B, C, D, E, t, T: REAL ;
BEGIN
   A := 3.0 ;
   B := 6.0 ;
   C := -123.0 ;
   D := -126.0 ;
   E := 1080.0 ;
   IF findQuartic(A, B, C, D, E, t)
   THEN
      (* 5, 3, -4, -6 *)
      T := A*(sqr(t)*sqr(t))+B*(sqr(t)*t)+C*sqr(t)+D*t+E ;
      printf("%gt^4 + %gt^3 +%gt^2 + %gt + %g = %g    (t=%g)\n",
             A, B, C, D, E, T, t);
      Assert(t=3.0)
   ELSE
      printf("failed to find root\n")
   END ;

   A := 1.0 ;
   B := 0.0 ;
   C := -15.0 ;
   D := 10.0 ;
   E := 24.0 ;
   IF findQuartic(A, B, C, D, E, t)
   THEN
      (* (x-3)(x+4)(x+1)(x-2) *)
      T := A*(sqr(t)*sqr(t))+B*(sqr(t)*t)+C*sqr(t)+D*t+E ;
      printf("%gt^4 + %gt^3 +%gt^2 + %gt + %g = %g    (t=%g)\n",
             A, B, C, D, E, T, t);
      Assert(t=2.0)
   ELSE
      printf("failed to find root\n")
   END ;

   A := 1.0 ;
   B := 0.0 ;
   C := -5.0 ;
   D := 0.0 ;
   E := 4.0 ;
   IF findQuartic(A, B, C, D, E, t)
   THEN
      (* (x-1)(x+1)(x-2)(x+2) *)
      T := A*(sqr(t)*sqr(t))+B*(sqr(t)*t)+C*sqr(t)+D*t+E ;
      printf("%gt^4 + %gt^3 +%gt^2 + %gt + %g = %g    (t=%g)\n",
             A, B, C, D, E, T, t);
      Assert(t=1.0)
   ELSE
      printf("failed to find root\n")
   END
END test ;


END roots.
