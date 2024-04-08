.
.de DIA-BEGIN
.  sp 1.5
.  B1
..
.
.de DIA-END
.  B2
.  ce
.  nop \s[-2]\f[B]\\$1\f[R]:  \\$2\s[+2]\f[R]
.  sp 1.5
..
.
.de VEC
.  nop \s[-1]\f[I](\\$1)\f[P]\s[+1]
..
.
.de PRG
.  nop \s[-1]\f[C]\\$1\f[P]\s[+1]\\$2
..
.nr PS 14
.nr VS 16
.EQ
delim $$
.EN
.TL
.nh
.na
Collision prediction between objects in two dimensions
.ad 1
.AU
Gaius Mulley
.PRG <gaius@gnu.org>
.AB
This small document describes how to predict the time of collision
between circles and polygons in two dimensions.  It breaks the problem
into three components, firstly it covers prediction of a collision
between a circle and another circle, secondly between a circle and
polygon and lastly it gives a method for determining when a collision
occurs between two polygons.
.AE
.SH
Introduction
.LP
The motivation for this document is to show clearly how to determine
collision prediction between objects and also provide documentation to
the program
.PRG twoDsim .
To this end there will be many steps included which may appear very
obvious to the reader.
.PP
Polygons are considered as a sequence of lines and thus collision
detection between polygons and circles consists of testing whether
individual lines will collide with another line or a circle.
Each object will be considered to have a velocity and acceleration.
Polygon rotation is not consided.
.
.TL
Collision prediction between a circle and a circle
.LP
Assuming both have a velocity and acceleration vector.  Consider
circles, $c sub 0$, $c sub 1$, which have velocity vectors: $(v sub
0x , v sub 0y )$ and $(v sub 1x , v sub 1y )$ respectively.  Additionally
both circles have acceleration vectors $(a sub 0x , a sub 0y )$ and $(a
sub 1x , a sub 1y )$ and are at initial positions $[c sub 0x , c sub 0y ]$
and $[c sub 1x , c sub 1y ]$.  Their radii are $r sub 0$ and $r sub 1$.
.PP
Given the distance between the circles can be calculated as:
.EQ
sqrt{(c sub 0x - c sub 1x ) sup 2 + (c sub 0y -c sub 1y ) sup 2}
.EN
and this value will be equal to $r sub 0 + r sub 1$ if a collision
occurs.  We need to find the time, $t$, at which the distance between
these circles is: $r sub 0 + r sub 1$.  Using the formala for initial
position, velocity and acceleration:
.EQ
s = s sub 0 + ut + { a t sup 2 } over 2 
.EN
It is also known that the position circle $c sub 0$
at time, $t$, is:
.EQ
left [ {c sub 0x + v sub 0x t + { a sub 0x t sup 2 } over 2},
{c sub 0y + v sub 0y t + { a sub 0y t sup 2 } over 2} right ]
.EN
Correspondingly the position circle $c sub 1$
at time, $t$, is:
.EQ
left [ {c sub 1x + v sub 1x t + { a sub 1x t sup 2 } over 2},
{c sub 1y + v sub 1y t + { a sub 1y t sup 2 } over 2} right ]
.EN
Therefore we need to find the time at which the distance between
both circles is $r sub 0 +r sub 1$ which is:
.ps -4
.EQ
r sub 0 +r sub 1 = sqrt{ left [ {left ( {c sub 0x + v sub 0x t + { a sub 0x t sup 2 } over 2} right ) - { left ( { c sub 1x + v sub 1x t + { a sub 1x t sup 2 } over 2 } right ) } } right ] sup 2 +
left [ {left ( {c sub 0y + v sub 0y t + { a sub 0y t sup 2 } over 2} right ) - { left ( { c sub 1y + v sub 1y t + { a sub 1y t sup 2 } over 2 } right ) } } right ] sup 2 }
.EN
.ps +4
.LP
Which can be easily rearranged as:
.ps -4
.EQ
left ( r sub 0 +r sub 1 right ) sup 2 = left [ {left ( {c sub 0x + v sub 0x  t + { a sub 0x  t sup 2 } over 2} right ) - { left ( { c sub 1x + v sub 1x  t + { a sub 1x  t sup 2 } over 2 } right ) } } right ] sup 2 +
left [ {left ( {c sub 0y + v sub 0y t + { a sub 0y t sup 2 } over 2} right ) - { left ( { c sub 1y + v sub 1y t + { a sub 1y t sup 2 } over 2 } right ) } } right ] sup 2
.EN
and then to:
.ps -4
.EQ
0 = left [ {left ( {c sub 0x + v sub 0x  t + { a sub 0x t sup 2 } over 2} right ) - { left ( { c sub 1x + v sub 1x  t + { a sub 1x t sup 2 } over 2 } right ) } } right ] sup 2 +
left [ {left ( {c sub 0y + v sub 0y  t + { a sub 0y t sup 2 } over 2} right ) - { left ( { c sub 1y + v sub 1y t + { a sub 1y t sup 2 } over 2 } right ) } } right ] sup 2 - {left ( r sub 0 +r sub 1 right ) sup 2}
.EN
.ps +4
.LP
Now the right hand side can be expanded.
.PRG wxmaxima
is a useful tool
and can be used to expand the equation into a quartic equation taking
the form:
.EQ
0 = At sup 4 + Bt sup 3 + Ct sup 2 + Dt + E
.EN
The expanded equation will not be pleasant!  To use
.PRG wxmaxima
we use single letter variable names to represent circle, $c sub 0 , c
sub 1$, velocity, acceleration and initial position.
.LP
Let:
.TS
tab(!), expand;
l!c!r.
$a = c sub 0x$!$b = c sub 0y$!$c = v sub 0x$
$d = v sub 0y$!$e = a sub 0x$!$f = a sub 0y$
$g = c sub 1x$!$h = c sub 1y$!$k = v sub 1x$
$l = v sub 1y$!$m = a sub 1x$!$n = a sub 1y$
$o = r sub 0$!$p = r sub 1$!
.TE
After making these substitutions the equation becomes:
.
.DIA-BEGIN
.nf
.ft C
.ps -1
0 = ((a+c*t+e*t^2/2.0) - (b+d*t+f*t^2/2.0))^2 +
    ((g+k*t+m*t^2/2.0) - (h+l*t+n*t^2/2.0))^2 -
    (o+p)^2
.fi
.ps +1
.ft P
.DIA-END "Figure 1" "input to \f[C]wxmaxima\f[P]"
.
Here is the output from wxmaxima after requesting that it performs the following manipulations
(\c
.PRG "expand ; factor ; ratsimp" ):
.
.DIA-BEGIN
.nf
.ft C
.ps -1
0=((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*
     f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t-
4*p^2-8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2)
 /4
.fi
.ps +1
.ft P
.DIA-END "Figure 2" "output from \f[C]wxmaxima\f[R]"
.
.LP
After both sides of the equation in Figure 2 are multiplied by $4$ the result is:
.
.DIA-BEGIN
.nf
.ft C
.ps -1
0=((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*
     f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t-
4*p^2-8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2)
.fi
.ps +1
.ft P
.DIA-END "Figure 3" "output from \f[C]wxmaxima\f[R] after being multiplied by 4"
.
.LP
Now it is clear that the quartic values for $A$, $B$, $C$, $D$ and $E$ are:
.ps -2
.EQ
A = n sup 2 -2mn+m sup 2 +f sup 2 -2ef+e sup 2
.EN
.ps -2
.EQ
B = (4l-4k)n+(4k-4l)m+(4d-4c)f+(4c-4d)e
.EN
.ps -2
.EQ
C = (4h-4g)n+(4g-4h)m+4 l sup 2 -8kl+4 k sup 2 +
    (4b-4a)f+(4a-4b)e+4 d sup 2 -8cd+4 c sup 2
.EN
.ps -2
.EQ
D = (8h-8g)l+(8g-8h)k+(8b-8a)d+(8a-8b)c
.EN
.ps -2
.EQ
E = 4p sup 2 -8op-4o sup 2 +4h sup 2 -8gh+4g sup 2 +4b sup 2 -8ab+4a sup 2
.EN
And all that remains is to find the values of $t$ when:
.EQ
0 = At sup 4 + Bt sup 3 + Ct sup 2 + Dt + E
.EN
using Ferrai's solution.  The smallest positive value of, $t$, is the
time at the next collision.
.
.SH
Collision prediction between a circle and a line
.LP
Assuming both have a velocity and acceleration vector.
.
.DIA-BEGIN
.so circle-line.pic
.DIA-END "Figure 4" "circle and line with their initial parameters"
.
.LP
The circle has a velocity $(C sub vx, C sub vy)$ vector and an
acceleration vector $(C sub ax , C sub ay )$.  The center of the
circle is at location $[C sub x , C sub y ]$.
.sp
The line has a velocity $(L sub vx , L sub vy )$ vector and an
acceleration vector $(L sub ax , L sub ay )$.  The line runs between
$[L sub 0x , L sub 0y ]$ and $[L sub 1x , L sub 2y ]$.
.SH
The problem
.LP
The problem is to find the time at which a collision between the line
and circle will take place (or even if a collision will take place at all).
.SH
Step 1
.LP
The first step is to view the line as being stationary and only view the
circle as moving.  This is easily achieved by subtracting the line's velocity
and acceleration components from the circle and line.
.
.DIA-BEGIN
.so circle-line2.pic
.DIA-END "Figure 5" "circle and stationary line"
.
.LP
Now the velocity vector for the circle is:
.EQ
(C sub vx -L sub vx , C sub vy -L sub vy )
.EN
and the acceleration vector for the circle is:
.EQ
(C sub ax -L sub ax, C sub ay -L sub ay )
.EN
.SH
Step 2
.LP
The second step is to translate and rotate both objects such that one
of the end points of the line is now at the origin and the remainder
is positioned along $y=0$.  We can further redraw our diagram as:
.
.DIA-BEGIN
.so circle-line3.pic
.DIA-END "Figure 6" "line objects translated and rotated"
.
.LP
All objects need to be translated by:
.EQ
(-L sub 1x , -L sub 1y )
.EN
.LP
All objects also need to be rotated by:
.EQ
theta = arc cos { L sub 1x -L sub 0x } over sqrt { ((L sub 1x -L sub 0x ) sup 2 + (L sub 1y -L sub 0y ) sup 2 }
.EN
.sp
A point can be rotated about the origin by:
.EQ
x' = cos( theta ) x - sin( theta ) y
.EN
.EQ
y' = sin( theta ) x + cos( theta ) y
.EN
Define two rotation functions:
.EQ
R sub x (x, y) = cos( theta ) x - sin( theta ) y
.EN
.EQ
R sub y (x, y) = sin( theta ) x + cos( theta ) y
.EN
.sp
The position of the new line is defined as:
.EQ
L sub 0x ' = R sub x (L sub 0x -L sub 1x , L sub 0y -L sub 1y )
.EN
.EQ
L sub 0y ' = R sub y (L sub 0x -L sub 1x , L sub 0y -L sub 1y )
.EN
The new velocity vector for the circle is:
.EQ
C sub vx ' = R sub x (C sub vx -L sub vx , C sub vy -L sub vy )
.EN
.EQ
C sub vy ' = R sub y (C sub vx -L sub vx , C sub vy -L sub vy )
.EN
The new acceleration vector for the circle is:
.EQ
C sub ax ' = R sub x (C sub ax -L sub ax , C sub ay -L sub ay )
.EN
.EQ
C sub ay ' = R sub y (C sub ax -L sub ax , C sub ay -L sub ay )
.EN
The center point of the circle is:
.EQ
C sub x ' = R sub x (C sub x -L sub 1x , C sub y -L sub 1y )
.EN
.EQ
C sub y' = R sub y (C sub x -L sub 1x , C sub y -L sub 1y )
.EN
.SH
Step 3
.LP
The next step is to redraw the diagram of Figure 3 as Figure 4.  It
will be easier to consider the time when the point at the center of
our original circle hits either of the three lines A, B or F.  Or if
this point hits the two circles at D and E.  The earliest time at
which this occurs will be the time the original circle either hits the
original line or misses it.
.PP
If the point collides with F then the circle will hit the original
line.  If the point collides with D or E then it will also hit the
line (actually clip either end of the line).
.PP
Whereas if the point hits A or B then the original circle
will miss the original line.  The equations which need to be
solved for their values of, t, are:
.EQ "crosses F"
0 = C sub y ' + C sub vy ' t + { C sub ay ' t sup 2 } over 2
.EN
.EQ "crosses A"
0 = C sub y ' + C sub vy ' t + { C sub ay ' t sup 2 } over 2
.EN
.EQ "crosses B"
L sub 0x ' = C sub y ' + C sub vy ' t + { C sub ay ' t sup 2 } over 2
.EN
.PP
We can treat point $[C sub x ',C sub y ']$ as being a circle with
radius of $0$.  Therefore we need to know when this circle collides
with circles D or E.  The smallest positive value for, $t$ for any of
these equations indicates the next collision time.
.PP
Let $f sub cc$ be a function which returns the smallest positive value
of $t$ when two circles collide:
.EQ
t = f sub cc (p sub 0x ,p sub 0y ,r sub 0 ,v sub 0x ,v sub 0 y ,a sub 0x ,a sub 0y ,p sub 1x ,p sub 1y ,r sub 1 ,v sub 1x ,v sub 1y ,a sub 1x ,a sub 1y )
.EN
Where:
.sp
.IP $p sub ix$ 4w
initial x position of circle i
.IP $p sub iy$ 4w
initial y position of circle i
.IP $r sub i$ 4w
radius of circle i
.IP $v sub ix$ 4w
initial velocity of circle i along x-axis
.IP $v sub iy$ 4w
initial velocity of circle i along y-axis
.IP $a sub ix$ 4w
accelation of circle i along x-axis
.IP $a sub iy$ 4w
accelation of circle i along y-axis
.PP
Now using $f sub cc$ the time at collision between $[C sub x ',C sub y ']$ and D or E can
be expressed thus:
.EQ ".. D"
t = f sub cc (C sub x ',C sub y ',0,v sub 0x ',v sub 0y ',a sub 0x ',a sub 0y ',0,0,r,0,0,0,0)
.EN
.EQ ".. E"
t = f sub cc (C sub x ',C sub y ',0,v sub 0x ',v sub 0y ',a sub 0x ',a sub 0y ',L sub 0x ',0,r,0,0,0,0)
.EN
.
.DIA-BEGIN
.so circle-line4.pic
.DIA-END "Figure 4" "one point, two circles and three line objects"
.
.TL
Collision prediction between a line and a line
.LP
