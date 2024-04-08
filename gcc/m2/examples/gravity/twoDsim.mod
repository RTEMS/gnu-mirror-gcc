(* Copyright (C) 2008, 2009, 2010, 2011, 2012
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE twoDsim ;

FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, HighIndice ;
FROM libc IMPORT printf, exit ;
FROM deviceGnuPic IMPORT Coord, Colour, newFrame, renderFrame, circleFrame, polygonFrame, produceAVI ;
FROM libm IMPORT sqrt ;
FROM roots IMPORT findQuartic, nearZero ;


CONST
   MaxPolygonPoints       =  6 ;
   DefaultFramesPerSecond = 24.0 ;

TYPE
   ObjectType = (polygonOb, circleOb, pivotOb) ;

   Object = POINTER TO RECORD
                          id            : CARDINAL ;
                          fixed         : BOOLEAN ;
                          vx, vy, ax, ay: REAL ;
                          CASE object: ObjectType OF

                          polygonOb:  p: Polygon |
                          circleOb :  c: Circle |
                          pivotOb  :  v: Pivot

                          END
                       END ;

   Pivot = RECORD
              pos: Coord ;
              id1,
              id2: CARDINAL ;
           END ;

   Circle = RECORD
               pos : Coord ;
               r   : REAL ;
               mass: REAL ;
            END ;

   Polygon = RECORD
                pos    : Coord ;
                nPoints: CARDINAL ;
                points : ARRAY [0..MaxPolygonPoints] OF Coord ;
                mass   : REAL ;
             END ;

   eventProc = PROCEDURE (eventQueue) ;

   eventQueue = POINTER TO RECORD
                              time: REAL ;
                              p   : eventProc ;
                              id1,
                              id2 : CARDINAL ;
                              next: eventQueue ;
                           END ;

VAR
   objects         : Index ;
   maxId           : CARDINAL ;
   collisionTime,
   currentTime,
   replayPerSecond,
   framesPerSecond : REAL ;
   simulatedGravity: REAL ;
   eventQ,
   freeEvents      : eventQueue ;


(*
   Assert -
*)

PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      printf("error assert failed\n") ;
      HALT
   END
END Assert ;


(*
   AssertR -
*)

PROCEDURE AssertR (a, b: REAL) ;
BEGIN
   IF a#b
   THEN
      printf("error assert failed: %g should equal %g\n", a, b)
   END
END AssertR ;


(*
   dumpCircle -
*)

PROCEDURE dumpCircle (o: Object) ;
BEGIN
   WITH o^ DO
      printf("circle at (%g, %g) radius %g mass %g\n", c.pos.x, c.pos.y, c.r, c.mass)
   END
END dumpCircle ;


(*
   dumpPolygon -
*)

PROCEDURE dumpPolygon (o: Object) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH o^ DO
      i := 0 ;
      printf("polygon at (%g, %g) mass %g\n", p.pos.x, p.pos.y, p.mass) ;
      WHILE i<p.nPoints DO
         printf("  line (%g,%g)\n", p.points[i].x, p.points[i].y) ;
         INC(i)
      END
   END
END dumpPolygon ;


(*
   DumpObject -
*)

PROCEDURE DumpObject (o: Object) ;
BEGIN
   WITH o^ DO
      printf("object %d ", id) ;
      IF fixed
      THEN
         printf("is fixed ")
      ELSE
         printf("is movable ")
      END ;
      CASE object OF

      circleOb :  dumpCircle(o) |
      polygonOb:  dumpPolygon(o) |
      pivotOb  :  printf("pivot\n")

      ELSE
      END ;
      IF NOT fixed
      THEN
         printf("    velocity (%g, %g) acceleration (%g, %g)\n", vx, vy, ax, ay)
      END
   END
END DumpObject ;


(*
   gravity - turn on gravity at: g m^2
*)

PROCEDURE gravity (g: REAL) ;
BEGIN
   simulatedGravity := g
END gravity ;


(*
   newObject - creates an object of, type, and returns its, id.
*)

PROCEDURE newObject (type: ObjectType) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   INC(maxId) ;
   NEW(optr) ;
   WITH optr^ DO
      id     := maxId ;
      fixed  := FALSE ;
      object := type ;
      vx     := 0.0 ;
      vy     := 0.0 ;
      ax     := 0.0 ;
      ay     := 0.0
   END ;
   PutIndice(objects, maxId, optr) ;
   RETURN( maxId )
END newObject ;


(*
   box - place a box in the world at (x0,y0),(x0+i,y0+j)
*)

PROCEDURE box (x0, y0, i, j: REAL) : CARDINAL ;
VAR
   id: CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.pos.x := x0 ;
      p.pos.y := y0 ;
      p.nPoints := 4 ;
      p.points[0].x := i ;
      p.points[0].y := 0.0 ;
      p.points[1].x := 0.0 ;
      p.points[1].y := j ;
      p.points[2].x := -i ;
      p.points[2].y := 0.0 ;
      p.points[3].x := 0.0 ;
      p.points[3].y := -j ;
   END ;
   RETURN id
END box ;


(*
   poly3 - place a triangle in the world at:
           (x0,y0),(x1,y1),(x2,y2)
*)

PROCEDURE poly3 (x0, y0, x1, y1, x2, y2: REAL) : CARDINAL ;
VAR
   id: CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 3 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2
   END ;
   RETURN id
END poly3 ;


(*
   poly5 - place a pentagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4)
*)

PROCEDURE poly5 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 5 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2 ;
      p.points[3].x := x2 ;
      p.points[3].y := y2 ;
      p.points[4].x := x2 ;
      p.points[4].y := y2
   END ;
   RETURN id
END poly5 ;


(*
   poly6 - place a hexagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)
*)

PROCEDURE poly6 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(pivotOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 6 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2 ;
      p.points[3].x := x2 ;
      p.points[3].y := y2 ;
      p.points[4].x := x2 ;
      p.points[4].y := y2 ;
      p.points[5].x := x2 ;
      p.points[5].y := y2
   END ;
   RETURN id
END poly6 ;


(*
   mass - specify the mass of an object and return the, id.
          Only polygon (and box) and circle objects may have
          a mass.
*)

PROCEDURE mass (id: CARDINAL; m: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      CASE object OF

      polygonOb:  p.mass := m |
      circleOb:   c.mass := m

      ELSE
      END
   END ;
   RETURN id
END mass ;


(*
   fix - fix the object to the world.
*)

PROCEDURE fix (id: CARDINAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      fixed := TRUE
   END ;
   RETURN id
END fix ;


(*
   circle - adds a circle to the world.  Center
            defined by: x0, y0 radius, r.
*)

PROCEDURE circle (x0, y0, r: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(circleOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      c.pos.x := x0 ;
      c.pos.y := y0 ;
      c.r     := r ;
      c.mass  := 0.0
   END ;
   RETURN id
END circle ;


(*
   pivot - pivot an object to position, (x0, y0).
*)

PROCEDURE pivot (x0, y0: REAL; id1: CARDINAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(pivotOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      v.pos.x := x0 ;
      v.pos.y := y0 ;
      v.id1 := id1
   END ;
   RETURN id
END pivot ;


(*
   velocity - give an object, id, a velocity, vx, vy.
*)

PROCEDURE velocity (id: CARDINAL; vx, vy: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
   IF optr^.fixed
   THEN
      printf("object %d is fixed and therefore cannot be given a velocity\n",
             id)
   ELSE
      optr^.vx := vx ;
      optr^.vy := vy
   END ;
   RETURN id
END velocity ;


(*
   accel - give an object, id, an acceleration, ax, ay.
*)

PROCEDURE accel (id: CARDINAL; ax, ay: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
   IF optr^.fixed
   THEN
      printf("object %d is fixed and therefore cannot be given an acceleration\n",
             id)
   ELSE
      optr^.ax := ax ;
      optr^.ay := ay
   END ;
   RETURN id
END accel ;


(*
   fps - set frames per second.
*)

PROCEDURE fps (f: REAL) ;
BEGIN
   framesPerSecond := f
END fps ;


(*
   replayRate - set frames per second during replay.
*)

PROCEDURE replayRate (f: REAL) ;
BEGIN
   replayPerSecond := f
END replayRate ;


(*
   getColour -
*)

PROCEDURE getColour (i: CARDINAL; e: eventQueue) : Colour ;
VAR
   p   : eventProc ;
   optr: Object ;
BEGIN
   p := debugFrame ;
   IF (i=e^.id1) OR (i=e^.id2)
   THEN
      IF e^.p=p
      THEN
         RETURN blue
      ELSE
         RETURN red
      END
   ELSE
      optr := GetIndice(objects, i) ;
      WITH optr^ DO
         CASE object OF

         polygonOb:  IF optr^.fixed
                     THEN
                        RETURN black
                     ELSE
                        RETURN yellow
                     END |
         circleOb:   IF optr^.fixed
                     THEN
                        RETURN green
                     ELSE
                        RETURN brown
                     END
         ELSE
            RETURN black
         END
      END
   END
END getColour ;


(*
   drawFrame -
*)

PROCEDURE drawFrame (e: eventQueue) ;
VAR
   i, n: CARDINAL ;
   optr: Object ;
BEGIN
   newFrame ;
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice(objects, i) ;
      WITH optr^ DO
         CASE object OF

         circleOb :  circleFrame(c.pos, c.r, getColour(i, e)) |
         polygonOb:  polygonFrame(p.pos, p.nPoints, p.points, getColour(i, e)) |
         pivotOb  :

         END
      END ;
      INC(i)
   END ;
   renderFrame
END drawFrame ;


(*
   drawFrameEvent -
*)

PROCEDURE drawFrameEvent (e: eventQueue) ;
BEGIN
   drawFrame(e) ;
   addEvent(1.0/framesPerSecond, drawFrameEvent)
END drawFrameEvent ;


(*
   updatePolygon -
*)

PROCEDURE updatePolygon (optr: Object; dt: REAL) ;
VAR
   vn: REAL ;
BEGIN
   WITH optr^ DO
      (* update vx and pos.x *)
      vn := vx + ax*dt ;
      p.pos.x := c.pos.x+dt*(vx+vn)/2.0 ;
      vx := vn ;
      (* update vy and pos.y *)
      vn := vy + (ay+simulatedGravity)*dt ;
      p.pos.y := c.pos.y+dt*(vy+vn)/2.0 ;
      vy := vn
   END
END updatePolygon ;


(*
   updateCircle -
*)

PROCEDURE updateCircle (optr: Object; dt: REAL) ;
VAR
   vn: REAL ;
BEGIN
   WITH optr^ DO
      (* update vx and pos.x *)
      vn := vx + ax*dt ;
      c.pos.x := c.pos.x+dt*(vx+vn)/2.0 ;
      vx := vn ;
      (* update vy and pos.y *)
      vn := vy + (ay+simulatedGravity)*dt ;
      c.pos.y := c.pos.y+dt*(vy+vn)/2.0 ;
      vy := vn
   END
END updateCircle ;


(*
   updateOb -
*)

PROCEDURE updateOb (optr: Object; dt: REAL) ;
BEGIN
   WITH optr^ DO
      IF NOT fixed
      THEN
         CASE object OF

         polygonOb:  updatePolygon(optr, dt) |
         circleOb :  updateCircle(optr, dt) |
         pivotOb  :  |

         END
      END
   END
END updateOb ;


(*
   updatePhysics - updates all positions of objects based on the passing of
                   dt seconds.
*)

PROCEDURE updatePhysics (dt: REAL) ;
VAR
   i, n: CARDINAL ;
   optr: Object ;
BEGIN
   currentTime := currentTime + dt ;
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice(objects, i) ;
      updateOb(optr, dt) ;
      INC(i)
   END
END updatePhysics ;


(*
   displayEvent -
*)

PROCEDURE displayEvent (e: eventQueue) ;
BEGIN
   WITH e^ DO
      printf("%g %p ", time, p);
      IF p=VAL(eventProc, drawFrameEvent)
      THEN
         printf("drawFrameEvent")
      ELSIF p=VAL(eventProc, doCollision)
      THEN
         printf("doCollision")
      ELSE
         printf("unknown event")
      END ;
      printf("%d %d\n", id1, id2)
   END
END displayEvent ;


(*
   printEvent - prints out the first event on the event queue.
*)

PROCEDURE printEvent ;
VAR
   e: eventQueue ;
BEGIN
   printf("The event queue\n");
   printf("===============\n");
   e := eventQ ;
   WHILE e#NIL DO
      displayEvent(e) ;
      e := e^.next
   END
END printEvent ;


(*
   doNextEvent -
*)

PROCEDURE doNextEvent () : REAL ;
VAR
   e : eventQueue ;
   dt: REAL ;
   p : eventProc ;
BEGIN
   IF eventQ=NIL
   THEN
      printf("no more events on the event queue\n") ;
      HALT
   ELSE
      printEvent ;
      e := eventQ ;
      eventQ := eventQ^.next ;
      dt := e^.time ;
      p  := e^.p ;
      Assert((p=VAL(eventProc, drawFrameEvent)) OR
             (p=VAL(eventProc, doCollision))) ;
      updatePhysics(dt) ;
      Assert((p=VAL(eventProc, drawFrameEvent)) OR
             (p=VAL(eventProc, doCollision))) ;
      p(e) ;
      e^.next := freeEvents ;
      freeEvents := e ;
      RETURN( dt )
   END
END doNextEvent ;


(*
   checkObjects -
*)

PROCEDURE checkObjects ;
VAR
   i, n : CARDINAL ;
   optr : Object ;
   error: BOOLEAN ;
BEGIN
   error := FALSE ;
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice(objects, i) ;
      WITH optr^ DO
         IF (NOT (optr^.fixed)) AND (optr^.c.mass=0.0)
         THEN
            printf("object %d is not fixed and does not have a mass\n",
                   optr^.id)
         END
      END ;
      INC(i)
   END ;
   IF error
   THEN
      exit(1)
   END
END checkObjects ;


(*
   collideFixedCircles -
*)

PROCEDURE collideFixedCircles (movable, fixed: Object) ;
VAR
   r, j              : REAL ;
   c, normalCollision,
   relativeVelocity  : Coord ;
BEGIN
   (* calculate normal collision value *)
   c.x := movable^.c.pos.x - fixed^.c.pos.x ;
   c.y := movable^.c.pos.y - fixed^.c.pos.y ;
   r := sqrt(c.x*c.x+c.y*c.y) ;
   normalCollision.x := c.x/r ;
   normalCollision.y := c.y/r ;
   relativeVelocity.x := movable^.vx ;
   relativeVelocity.y := movable^.vy ;

   j := (-(1.0+1.0) *
         ((relativeVelocity.x * normalCollision.x) +
          (relativeVelocity.y * normalCollision.y)))/
        (((normalCollision.x*normalCollision.x) +
          (normalCollision.y*normalCollision.y)) *
         (1.0/movable^.c.mass)) ;

   movable^.vx := movable^.vx + (j * normalCollision.x) / movable^.c.mass ;
   movable^.vy := movable^.vy + (j * normalCollision.y) / movable^.c.mass
END collideFixedCircles ;


(*
   collideMovableCircles -
*)

PROCEDURE collideMovableCircles (iptr, jptr: Object) ;
VAR
   r, j              : REAL ;
   c, normalCollision,
   relativeVelocity  : Coord ;
BEGIN
   (* calculate normal collision value *)
   c.x := iptr^.c.pos.x - jptr^.c.pos.x ;
   c.y := iptr^.c.pos.y - jptr^.c.pos.y ;
   r := sqrt(c.x*c.x+c.y*c.y) ;
   normalCollision.x := c.x/r ;
   normalCollision.y := c.y/r ;
   relativeVelocity.x := iptr^.vx - jptr^.vx ;
   relativeVelocity.y := iptr^.vy - jptr^.vy ;
   j := (-(1.0+1.0) *
         ((relativeVelocity.x * normalCollision.x) +
          (relativeVelocity.y * normalCollision.y)))/
        (((normalCollision.x*normalCollision.x) +
          (normalCollision.y*normalCollision.y)) *
         (1.0/iptr^.c.mass + 1.0/jptr^.c.mass)) ;

   iptr^.vx := iptr^.vx + (j * normalCollision.x) / iptr^.c.mass ;
   iptr^.vy := iptr^.vy + (j * normalCollision.y) / iptr^.c.mass ;

   jptr^.vx := jptr^.vx - (j * normalCollision.x) / jptr^.c.mass ;
   jptr^.vy := jptr^.vy - (j * normalCollision.y) / jptr^.c.mass
END collideMovableCircles ;


(*
   circleCollision -
*)

PROCEDURE circleCollision (iptr, jptr: Object) ;
BEGIN
   IF nearZero(currentTime-collisionTime)
   THEN
      RETURN
   END ;
   collisionTime := currentTime ;

   IF iptr^.fixed
   THEN
      printf("collision with fixed\n");
      DumpObject(jptr) ;
      collideFixedCircles(jptr, iptr) ;
      DumpObject(jptr) ;
   ELSIF jptr^.fixed
   THEN
      printf("collision with fixed\n");
      DumpObject(iptr) ;
      collideFixedCircles(iptr, jptr) ;
      DumpObject(iptr) ;
   ELSE
      printf("collision with movable\n");
      DumpObject(iptr) ;
      DumpObject(jptr) ;
      collideMovableCircles(iptr, jptr) ;
      DumpObject(iptr) ;
      DumpObject(jptr)
   END
END circleCollision ;


(*
   physicsCollision - handle the physics of a collision between
                      the two objects defined in, e.
*)

PROCEDURE physicsCollision (e: eventQueue) ;
VAR
   iptr, jptr: Object ;
BEGIN
   iptr := GetIndice(objects, e^.id1) ;
   jptr := GetIndice(objects, e^.id2) ;
   IF (iptr^.object=circleOb) AND (jptr^.object=circleOb)
   THEN
      circleCollision(iptr, jptr)
   END
END physicsCollision ;


(*
   doCollision -
*)

PROCEDURE doCollision (e: eventQueue) ;
BEGIN
   drawFrame(e) ;
   physicsCollision(e) ;
   addNextCollisionEvent
END doCollision ;


(*
   sqr -
*)

PROCEDURE sqr (v: REAL) : REAL ;
BEGIN
   RETURN v*v
END sqr ;


(*
   findCollisionCircles -

   using:

   S = UT + (AT^2)/2
   compute xin and yin which are the new (x,y) positions of object i at time, t.
   compute xjn and yjn which are the new (x,y) positions of object j at time, t.
   now compute difference between objects and if they are ri+rj  (radius of circle, i, and, j)
   apart then we have a collision at time, t.

   xin = xi + vxi * t + (aix * t^2) / 2.0
   yin = yi + vyi * t + (aiy * t^2) / 2.0

   xjn = xj + vxj * t + (ajx * t^2) / 2.0
   yjn = yj + vyj * t + (ajy * t^2) / 2.0

   ri + rj == sqrt(abs(xin-xjn)^2 + abs(yin-yjn)^2)     for values of t

   ri + rj == sqrt(((xi + vxi * t + aix * t^2 / 2.0) - (xj + vxj * t + ajx * t^2 / 2.0))^2 +
                   ((yi + vyi * t + aiy * t^2 / 2.0) - (yj + vyj * t + ajy * t^2 / 2.0))^2)

   let:

   a = xi
   b = xj
   c = vxi
   d = vxj
   e = aix
   f = ajx
   g = yi
   h = yj
   k = vyi
   l = vyj
   m = aiy
   n = ajy
   o = ri
   p = rj
   t = t

   o  + p  == sqrt(((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
                   ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2)

   o  + p  == sqrt(((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
                   ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2)

   0       == ((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
              ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2 -
              (o  + p)^2

   now using wxmaxima
   expand ; factor ; ratsimp

   p+o    ==  (sqrt((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))/2

   2*(p+o) ==  (sqrt((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                    ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                    ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                    ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))

   (2*(p+o))^2 == ((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))

   0           ==  (n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+
                   4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2)-
                   ((2*(p+o))^2)

   solve polynomial:

   A := sqr(n)-2.0*m*n+sqr(m)+sqr(f)-2.0*e*f+sqr(e) ;
   B := (4.0*l-4.0*k)*n+(4.0*k-4.0*l)*m+(4.0*d-4.0*c)*f+(4.0*c-4.0*d)*e ;
   C := (4.0*h-4.0*g)*n+(4.0*g-4.0*h)*m+4.0*sqr(l)-8.0*k*l+4.0*sqr(k)+(4.0*b-4.0*a)*f+(4.0*a-4.0*b)*e+4.0*sqr(d)-8.0*c*d+4.0*sqr(c) ;
   D := (8.0*h-8.0*g)*l+(8.0*g-8.0*h)*k+(8.0*b-8.0*a)*d+(8.0*a-8.0*b)*c ;
   E := 4.0*sqr(h)-8.0*g*h+4.0*sqr(g)+4.0*sqr(b)-8.0*a*b+4.0*sqr(a)-sqr(2.0*(p+o)) ;
*)

PROCEDURE findCollisionCircles (iptr, jptr: Object; VAR ic, jc: CARDINAL; VAR tc: REAL) ;
VAR
   a, b, c, d, e,
   f, g, h, k, l,
   m, n, o, p, t,
   A, B, C, D, E: REAL ;
   i, j         : CARDINAL ;
   T            : REAL ;
BEGIN
   DumpObject(iptr) ;
   DumpObject(jptr) ;
   WITH iptr^ DO
      a := c.pos.x    (* xi *)
   END ;
   c := iptr^.vx ;    (* vxi *)
   WITH iptr^ DO
      IF fixed
      THEN
         e := 0.0 ;   (* aix *)
         m := 0.0     (* aiy *)
      ELSE
         e := ax ;    (* aix *)
         m := ay+simulatedGravity    (* aiy *)
      END ;
      g := c.pos.y ;  (* yi *)
      k := vy ;       (* vyi *)
      o := c.r        (* ri *)
   END ;

   WITH jptr^ DO
      b := c.pos.x ;  (* xj *)
      IF fixed
      THEN
         f := 0.0 ;  (* ajx *)
         n := 0.0    (* ajy *)
      ELSE
         f := ax ;   (* ajx *)
         n := ay+simulatedGravity  (* ajy *)
      END ;
      d := vx ;      (* vxj *)
      h := c.pos.y ; (* yj *)
      l := vy        (* vyj *)
   END ;
   p := jptr^.c.r ;  (* rj *)

   (* thanks to wxmaxima  (expand ; factor ; ratsimp) *)

   A := sqr(n)-2.0*m*n+sqr(m)+sqr(f)-2.0*e*f+sqr(e) ;
   B := (4.0*l-4.0*k)*n+(4.0*k-4.0*l)*m+(4.0*d-4.0*c)*f+(4.0*c-4.0*d)*e ;
   C := (4.0*h-4.0*g)*n+(4.0*g-4.0*h)*m+4.0*sqr(l)-8.0*k*l+4.0*sqr(k)+(4.0*b-4.0*a)*f+(4.0*a-4.0*b)*e+4.0*sqr(d)-8.0*c*d+4.0*sqr(c) ;
   D := (8.0*h-8.0*g)*l+(8.0*g-8.0*h)*k+(8.0*b-8.0*a)*d+(8.0*a-8.0*b)*c ;
   E := 4.0*sqr(h)-8.0*g*h+4.0*sqr(g)+4.0*sqr(b)-8.0*a*b+4.0*sqr(a)-sqr(2.0*(p+o)) ;

   (* now solve for values of t which satisfy   At^4 + Bt^3 + Ct^2 + Dt^1 + Et^0 = 0 *)
   IF findQuartic(A, B, C, D, E, t)
   THEN
      T := A*(sqr(t)*sqr(t))+B*(sqr(t)*t)+C*sqr(t)+D*t+E ;
      printf("%gt^4 + %gt^3 +%gt^2 + %gt + %g = %g    (t=%g)\n",
             A, B, C, D, E, T, t);
      (* remember tc is -1.0 initially, to force it to be set once *)
      IF ((tc<0.0) OR (t<tc)) AND (NOT nearZero(t))
      THEN
         tc := t ;
         ic := iptr^.id ;
         jc := jptr^.id
      END
   END
END findCollisionCircles ;


(*
   findCollision -
*)

PROCEDURE findCollision (iptr, jptr: Object; VAR ic, jc: CARDINAL; VAR tc: REAL) ;
BEGIN
   IF NOT ((iptr^.fixed) AND (jptr^.fixed))
   THEN
      IF (iptr^.object=circleOb) AND (jptr^.object=circleOb)
      THEN
         findCollisionCircles(iptr, jptr, ic, jc, tc)
      END
   END
END findCollision ;


(*
   debugFrame - debug frame at time, e.
*)

PROCEDURE debugFrame (e: eventQueue) ;
BEGIN
   drawFrame(e) ;
END debugFrame ;


(*
   addDebugging - add a debugging event at time, t, which colours objects,
                  a, and, b, blue.
*)

PROCEDURE addDebugging (t: REAL; a, b: CARDINAL) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := debugFrame ;
      id1 := a ;
      id2 := b ;
      next := NIL
   END ;
   addRelative(e)
END addDebugging ;


(*
   addNextCollisionEvent -
*)

PROCEDURE addNextCollisionEvent ;
VAR
   tc        : REAL ;
   ic, jc,
   i, j, n   : CARDINAL ;
   iptr, jptr: Object ;
BEGIN
   n := HighIndice(objects) ;
   i := 1 ;
   tc := -1.0 ;
   ic := n+1 ;
   jc := n+1 ;
   WHILE i<=n DO
      iptr := GetIndice(objects, i) ;
      j := i+1 ;
      WHILE j<=n DO
         jptr := GetIndice(objects, j) ;
         findCollision(iptr, jptr, ic, jc, tc) ;
         INC(j)
      END ;
      INC(i)
   END ;
   IF tc>=0.0
   THEN
      addCollisionEvent(tc, doCollision, ic, jc)
   ELSE
      printf("no more collisions found\n")
   END
END addNextCollisionEvent ;


(*
   simulateFor - render for, t, seconds.
*)

PROCEDURE simulateFor (t: REAL) ;
VAR
   s, dt: REAL ;
BEGIN
   s := 0.0 ;
   killQueue ;
   checkObjects ;
   addEvent(0.0, drawFrameEvent) ;
   addNextCollisionEvent ;
   WHILE s<t DO
      dt := doNextEvent() ;
      s := s + dt
   END ;
   IF replayPerSecond=0.0
   THEN
      produceAVI(TRUNC(framesPerSecond))
   ELSE
      produceAVI(TRUNC(replayPerSecond))
   END
END simulateFor ;


(*
   newEvent -
*)

PROCEDURE newEvent () : eventQueue ;
VAR
   e: eventQueue ;
BEGIN
   IF freeEvents=NIL
   THEN
      NEW(e)
   ELSE
      e := freeEvents ;
      freeEvents := freeEvents^.next
   END ;
   RETURN( e )
END newEvent ;


(*
   addRelative - adds event, e, into the relative event queue.
*)

PROCEDURE addRelative (e: eventQueue) ;
VAR
   before, after: eventQueue ;
BEGIN
   IF eventQ=NIL
   THEN
      eventQ := e
   ELSIF e^.time<eventQ^.time
   THEN
      eventQ^.time := eventQ^.time - e^.time ;
      e^.next := eventQ ;
      eventQ := e
   ELSE
      printEvent ;
      before := eventQ ;
      after := eventQ^.next ;
      WHILE (after#NIL) AND (after^.time<e^.time) DO
         e^.time := e^.time - before^.time ;
         before := after ;
         after := after^.next
      END ;
      IF after#NIL
      THEN
         after^.time := after^.time-e^.time
      END ;
      e^.time := e^.time-before^.time ;
      before^.next := e ;
      e^.next := after ;
      printEvent
   END
END addRelative ;


(*
   addEvent -
*)

PROCEDURE addEvent (t: REAL; dop: eventProc) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := dop ;
      id1 := 0 ;
      id2 := 0 ;
      next := NIL
   END ;
   addRelative(e)
END addEvent ;


(*
   addCollisionEvent -
*)

PROCEDURE addCollisionEvent (t: REAL; dop: eventProc; a, b: CARDINAL) ;
VAR
   e: eventQueue ;
BEGIN
   printf("collision will occur in %g simulated seconds\n", t) ;
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := dop ;
      id1 := a ;
      id2 := b ;
      next := NIL
   END ;
   addRelative(e)
END addCollisionEvent ;


(*
   killQueue - destroys the event queue and returns events to the free list.
*)

PROCEDURE killQueue ;
VAR
   e: eventQueue ;
BEGIN
   IF eventQ#NIL
   THEN
      e := eventQ ;
      WHILE e^.next#NIL DO
         e := e^.next
      END ;
      e^.next := freeEvents ;
      freeEvents := eventQ ;
      eventQ := NIL
   END
END killQueue ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   maxId := 0 ;
   objects := InitIndex(1) ;
   framesPerSecond := DefaultFramesPerSecond ;
   replayPerSecond := 0.0 ;
   simulatedGravity := 0.0 ;
   eventQ := NIL ;
   freeEvents := NIL ;
   currentTime := 0.0 ;
   collisionTime := 0.0
END Init ;


BEGIN
   Init
END twoDsim.
