#!/usr/bin/env python3

import pgeif
import pygame
import sys
import struct
import time
import math
from pygame.locals import *


colour_t, box_t, circle_t, fb_box_t, fb_circle_t, fb_text_t = list(range(6))
id2ob = {}
ob2id = {}
batch_d, pyg_d = list(range(2))
device = None
opened = False
output = None
lastDelay = 0.0
debugging = False
foreground= []
background= []
colours = []
levels = {}
resolution = None
fullscreen = False
screen_initialised = False
pyevent2func = {}
call = {}
idTOcol = {}
nextFrame = 1
screen = None
backcanvas = None
program_name = 'pge'
version_number = '0.3'
Black = (0, 0, 0)
framesPerSecond = 100.0
slow_down_factor = 1.0
allowed_events = [USEREVENT+1]
id2func = {}
idcount = 0
_record = False
font_size = None
font = None


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print(str (format) % args, end=' ')


def debugf (format, *args):
    global debugging

    if debugging:
        print(str (format) % args, end=' ')

def errorf (format, *args):
    m = str (format) % args
    sys.stdout.write ("pge: " + m)
    raise Exception (m)
    sys.exit (1)

def internalf (format, *args):
    m = str (format) % args
    sys.stdout.write ("internal error in pge: " + m)
    raise Exception (m)
    sys.exit (1)

class myfile:
    def __init__ (self, c):
        self.contents = c
        self.length = len (self.contents)
        self.pos = 0
        debugf ("length %d bytes", self.length)
    def read (self, n):
        # print "read called for", n, "bytes", self.length-self.pos, "available"
        if self.pos + n <= self.length:
            b = self.contents[self.pos:self.pos+n]
            self.pos += n
            return b
        else:
            printf ("unexpected eof reached in frame %d\n", nextFrame)
            sys.exit (1)
    def close (self):
        pass
    def left (self):
        return self.length-self.pos

def _emit_short (s):
    global output

    output.write (struct.pack ('!H', s))

def _emit_card (c):
    global output

    output.write (struct.pack ('!I', c))

def _emit_double (d):
    global output

    d = (float) (d)
    output.write (struct.pack ('d', d))

def _emit_fract (f):
    global output

    if f == 0:
        output.write (struct.pack ('B', 0))
    elif f == 1:
        output.write (struct.pack ('B', 1))
    elif f < 1.0:
        output.write (struct.pack ('B', 2))
        output.write (struct.pack ('!QQ', f*10000.0, 10000.0))
    else:
        w = int (f)
        f -= w
        output.write (struct.pack ('B', 3))
        output.write (struct.pack ('!QQQ', w, f*10000.0, 10000.0))


class object:
    def __init__ (self, t, o, c = None, level = None):
        self.deleted = False
        self.type = t
        self.o = o
        self.fixed = False
        self.param = None
        self.kg = None
        self.collisionWith = []
        self.collisionp = None
        self.w = None
        if c == None:
            self.c = self
        else:
            self.c = c
        self.level = level

    def _id (self):
        return self.o

    def _get_3_colour (self):
        global idTOcol

        self._check_colour ()
        i = pgeif.h2l (self._get_pgeif_colour ())
        if i in idTOcol:
            return idTOcol[i]
        internalf ("3 colour triple should have been defined")

    def _get_pgeif_colour (self):
        self._check_colour ()
        return self.o[-1]

    def _draw (self):
        global screen, device
        if self.type == fb_box_t:
            if device == pyg_d:
                c = self.get_colour ()._get_3_colour ()
                # print c
                x = (int) (self.o[0] * resolution[0])
                y = (int) (self.o[1] * resolution[1])
                w = (int) ((self.o[2] - self.o[0]) * resolution[0])
                h = (int) ((self.o[5] - self.o[1]) * resolution[1])
                pygame.draw.rect (screen, c, (x, flip (y), w, h))
            else:
                self._emit_fill_polygon ()
        elif self.type == fb_circle_t:
            if device == pyg_d:
                c = self.get_colour ()._get_3_colour ()
                # print c
                x = (int) (self.o[0] * resolution[0])
                y = (int) (self.o[1] * resolution[1])
                r = (int) (self.o[2] * resolution[0])
                pygame.draw.circle (screen, c, (x, flip (y)), r, 0)
            else:
                self._emit_fill_circle ()
        elif self.type == colour_t:
            pass
        elif self.type == fb_text_t:
            if device == pyg_d:
                c = self.get_colour ()._get_3_colour ()
                font = get_font (self.o[3])
                t = font.render (self.o[2], True, c)
                textpos = t.get_rect ()
                textpos.top = screen.get_rect ().bottom - (int) (resolution[1] * self.o[1])
                textpos.left = screen.get_rect ().left + (int) (resolution[0] * self.o[0])
                screen.blit (t, textpos)

    def _emit_fill_circle (self):
        output.write (struct.pack ("3s", "dC"))
        _emit_fract (self.o [0])  #  x pos
        _emit_fract (self.o [1])  #  y pos
        _emit_fract (self.o [2])  #  radius
        _emit_short (self.o [3])  #  colour
        print("_emit_fill_circle, colour is ", self.o [3], self.o [0], self.o [1], self.o [2])

    def _emit_fill_polygon (self):
        output.write (struct.pack ("3s", "dP"))
        n = (len (self.o)-1)/2
        _emit_short (n)
        ier = iter (self.o[:-1])
        # print self.o
        for x in ier:
            _emit_fract (x)
            _emit_fract (next (ier))
        _emit_short (self.o [-1])

    def _name (self):
        if self.type == colour_t:
            return "colour"
        elif self.type == box_t:
            return "box"
        elif self.type == circle_t:
            return "circle"
        else:
            printf ("fatal error, object not recognised\n")
            sys.exit (1)

    def velocity (self, vx, vy):
        self._check_type ([box_t, circle_t], "assign a velocity to a")
        self._check_not_fixed ("assign a velocity")
        self._check_not_deleted ("a velocity")
        print("velocity for object", self.o, vx, vy)
        self.o = self._check_same (pgeif.velocity (self.o, vx, vy))
        return self

    def accel (self, ax, ay):
        self._check_type ([box_t, circle_t], "assign an acceleration to a")
        self._check_not_fixed ("assign an acceleration")
        self._check_not_deleted ("an acceleration")
        self.o = self._check_same (pgeif.accel (self.o, ax, ay))
        return self

    def fix (self):
        self._check_type ([box_t, circle_t], "fix a")
        self._check_not_deleted (" a fixed position")
        self._check_no_mass ("cannot fix " + self._name () + " as it has a mass")
        self.fixed = True
        self.o = self._check_same (pgeif.fix (self.o))
        print("fix", self.o)
        return self

    def mass (self, m):
        self._check_type ([box_t, circle_t], "assign a mass to a")
        self._check_not_fixed ("assign a mass")
        self._check_not_deleted (" a mass")
        if m is None:
            errorf ("cannot give value None as the mass\n")
        self.kg = m
        self.o = self._check_same (pgeif.mass (self.o, m))
        print("mass", self.o)
        return self

    def apply_impulse (self, unit_vec, magnitude):
        self._check_type ([box_t, circle_t], "assign an impulse to a")
        self._check_not_fixed ("assign an impulse")
        self._check_not_deleted (" an impulse")
        if (magnitude is None) or (unit_vec is None):
            return
        print("magnitude", magnitude, "vector", unit_vec)
        pgeif.apply_impulse (self.o, unit_vec[0], unit_vec[1], magnitude)
        return self

    def on_collision_with (self, another, p):
        if debugging:
            print("ok registering call back", p, another)
        self.collisionp = p
        self.collisionWith = another
        return self

    def on_collision (self, p):
        self.on_collision_with ([], p)
        return self

    def _check_type (self, legal, message):
        if not self.type in legal:
            errorf ("you cannot %s %s object\n", message, self._type_name ())

    def _check_not_fixed (self, message):
        if self.fixed:
            errorf ("object %s is fixed, you cannot %s\n", self._type_name (), message)

    def _check_colour (self):
        if self.type != colour_t:
            errorf ("object is expected to be a colour\n")

    def _param_colour (self, message):
        if self.type != colour_t:
            errorf (message)

    def rm (self):
        if not self.deleted:
            printf ("calling pgeif.rm\n")
            if self.level == 0:
                self.o = pgeif.rm (self.o)
            else:
                _sub (self, self.level)
            self.deleted = True
            printf ("returned from pgeif.rm\n")

    def _check_not_deleted (self, message):
        if self.deleted:
            errorf ("object has been deleted and now it is being given " + message)

    def _check_no_mass (self, message):
        if self.kg != None:
            errorf (message + "\n")

    def _check_same (self, o):
        if o == self.o:
            return o
        errorf ("internal error, object %d does not match self.o = %d\n", o, self.o)

    def _collision (self, between, e):
        if debugging:
            print("collision seen, between:", between)
        if self.collisionWith == []:
            if self.collisionp != None:
                if debugging:
                    print("before collisionp")
                self.collisionp (self, e)
                if debugging:
                    print("after collisionp")
        else:
            for c in self.collisionWith:
                for b in between:
                    if c == b:
                        break
            else:
                return
            if self.collisionp != None:
                self.collisionp (self, e)

    def get_param (self):
        return self.param

    def set_param (self, value):
        self.param = value
        return self

    def set_width (self, value):
        self.w = value

    def get_width (self):
        self._check_type ([box_t, circle_t], "get the width")
        return self.w

    def get_mass (self):
        return self.kg

    def get_colour (self):
        return self.c

    def set_colour (self, c):
        self._check_type ([box_t, circle_t, fb_box_t, fb_circle_t], "set_colour")
        c._param_colour ("first parameter to set_colour is expected to be a colour")
        if self.type in [box_t, circle_t]:
            pgeif.set_colour (self.o, c._get_pgeif_colour ())
        else:
            self.o = self.o [:-1]
            self.o += [c._get_pgeif_colour ()]
        self.c = c
        return self

    def get_unit_coord (self):
        self._check_type ([circle_t], "get the unit coordinate")
        return [self.get_xpos (), self.get_ypos ()]

    def get_xpos (self):
        self._check_type ([box_t, circle_t], "get the xpos")
        return pgeif.get_xpos (self.o)

    def get_ypos (self):
        self._check_type ([box_t, circle_t], "get the ypos")
        return pgeif.get_ypos (self.o)

    def get_xvel (self):
        self._check_type ([box_t, circle_t], "get the xvel")
        return pgeif.get_xvel (self.o)

    def get_yvel (self):
        self._check_type ([box_t, circle_t], "get the yvel")
        return pgeif.get_yvel (self.o)

    def get_xaccel (self):
        self._check_type ([box_t, circle_t], "get the xaccel")
        return pgeif.get_xaccel (self.o)

    def get_yaccel (self):
        self._check_type ([box_t, circle_t], "get the yaccel")
        return pgeif.get_yaccel (self.o)

    def put_xvel (self, f):
        print("put_xvel on a", self._name ())
        self._check_type ([box_t, circle_t], "put the xvel")
        return pgeif.put_xvel (self.o, f)

    def put_yvel (self, f):
        self._check_type ([box_t, circle_t], "put the yvel")
        return pgeif.put_yvel (self.o, f)

    def put_xaccel (self, f):
        self._check_type ([box_t, circle_t], "put the xaccel")
        return pgeif.put_xaccel (self.o, f)

    def put_yaccel (self, f):
        self._check_type ([box_t, circle_t], "put the yaccel")
        return pgeif.put_yaccel (self.o, f)

    def moving_towards (self, x, y):
        self._check_type ([box_t, circle_t], "test moving_towards")
        if self.fixed:
            return False
        return pgeif.moving_towards (self.o, x, y)

def _colspace (f):
    return (int)(f * 255.0)


def rgb (r, g, b):
    global idTOcol

    print("in rgb (", r, g, b, ")")
    c = pgeif.rgb (float(r), float(g), float(b))
    print("after pgeif.rgb ->", c)
    o = object (colour_t, [float(r), float(g), float(b), c])
    o._check_colour ()
    c = pgeif.h2l (c)
    idTOcol[c] = (_colspace (r), _colspace (g), _colspace (b))
    print("define colour triple as:", idTOcol[c])
    return o

def white ():
    c = pgeif.white ()
    o = object (colour_t, [1.0, 1.0, 1.0, c])
    c = pgeif.h2l (c)
    idTOcol[c] = (255, 255, 255)
    return o

def _register (id, ob):
    global id2ob, od2id

    debugf ("registering %d\n", id)
    id2ob[id] = ob
    ob2id[ob] = id


def text (x, y, s, c, size, level):
    global device, screen
    c._param_colour ("fourth parameter to text is expected to be a colour")
    if level == 0:
        errorf ("not allowed to place in level 0")
    else:
        ob = object (fb_text_t, [x, y, s, size, c._get_pgeif_colour ()], c, level)
        _add (ob, level)
    return ob


def box (x, y, w, h, c, level = 0):
    print("box:", x, y, w, h, c, level)
    c._param_colour ("fifth parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.box (x, y, w, h, c._get_pgeif_colour ())
        print("box colour =", c, c._get_pgeif_colour ())
        ob = object (box_t, id, c, level)
        ob.set_width (w)
        debugf ("box ")
        _register (id, ob)
    else:
        ob = object (fb_box_t, [x, y, x+w, y, x+w, y+h, x+w, y+h, x, y+h, c._get_pgeif_colour ()], c, level)
        print("box colour =", c, c._get_pgeif_colour ())
        _add (ob, level)
    return ob


def poly3 (x0, y0, x1, y1, x2, y2, c, level = 0):
    c._param_colour ("seventh parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.poly3 (x0, y0, x1, y1, x2, y2, c._get_pgeif_colour ())
        ob = object (box_t, id, c, level)
        debugf ("poly3 ")
        _register (id, ob)
    else:
        ob = object (fb_box_t, [x0, y0, x1, y1, x2, y2, c._get_pgeif_colour ()], c, level)
        _add (ob, level)
    return ob


def poly4 (x0, y0, x1, y1, x2, y2, x3, y3, c, level = 0):
    c._param_colour ("seventh parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.poly4 (x0, y0, x1, y1, x2, y2, x3, y3, c._get_pgeif_colour ())
        ob = object (box_t, id, c, level)
        debugf ("poly3 ")
        _register (id, ob)
    else:
        ob = object (fb_box_t, [x0, y0, x1, y1, x2, y2, x3, y3, c._get_pgeif_colour ()], c, level)
        _add (ob, level)
    return ob


def _add (ob, level):
    global foreground, background, levels

    if level > 0:
        if not (level in foreground):
            foreground += [level]
            foreground.sort ()
    else:
        if not (level in background):
            background += [level]
            background.sort ()

    if level in levels:
        levels[level] += [ob]
    else:
        levels[level] = [ob]
    print(levels[level])


def _sub (ob, level):
    global foreground, background

    if level in levels:
        levels[level].remove (ob)

    if level > 0:
        f = []
        for l in foreground:
            if l in levels:
                f += [l]
        foreground = f
        foreground.sort ()
    else:
        b = []
        for l in background:
            if l in levels:
                b += [l]
        background = b
        background.sort ()


def circle (x, y, r, c, level = 0):
    c._param_colour ("fourth parameter to box is expected to be a colour")
    if level == 0:
        id = pgeif.circle (x, y, r, c._get_pgeif_colour ())
        print("circle id =", id)
        debugf ("circle ")
        ob = object (circle_t, id, c, level)
        _register (id, ob)
    else:
        print("circle, colour =", c)
        print("pge: colour", c._get_pgeif_colour ())
        ob = object (fb_circle_t, [x, y, r, c._get_pgeif_colour ()], c, level)
        _add (ob, level)
    return ob


#
#  unpackFract - returns three integers:  w, n, d
#                representing fraction.
#

def unpackFract (s):
    b = s[0]
    v = struct.unpack ("B", b)[0]

    if v == 0:
        return (0, 0, 0)
    elif v == 1:
        return (1, 0, 0)
    elif v == 2:
        b = s[1:17]
        r = struct.unpack('!QQ', b)
        return (0, r[0], r[1])
    else:
        b = s[1:33]
        return struct.unpack('!QQQ', b)


#
#  unpackReal
#

def unpackReal (s):
    if len (s) >= 8:
        return struct.unpack ('d', s[:8])[0]
    else:
        printf ("insufficient data passed to unpackReal\n")


def unpackCard (s):
    if len (s) >= 4:
        return struct.unpack ('!I', s[:4])[0]
    else:
        printf ("insufficient data passed to unpackCard\n")


def unpackCardPair (s):
    if len (s) >= 8:
        return [struct.unpack ('!I', s[:4])[0],
                struct.unpack ('!I', s[4:8])[0]]
    else:
        printf ("insufficient data passed to unpackCardPair (%d bytes)\n", len (s))

def unpackIdPair (s):
    p = unpackCardPair (s)
    p[0] = pgeif.l2h (p[0])
    p[1] = pgeif.l2h (p[1])
    return p

def unpackPoint (s):
    if len (s) >= 16:
        return [unpackReal (s[:8]), unpackReal (s[8:])]
    else:
        printf ("insufficient data passed to unpackPoint\n")

def draw_foreground ():
    # print "draw foreground", foreground
    if foreground != []:
        for l in foreground:
            # print "drawing level", l
            for o in levels[l]:
                o._draw ()

def draw_background ():
    # print "draw background", background
    if background != []:
        for l in background:
            # print "drawing level", l
            for o in levels[l]:
                o._draw ()

no_event, frame_event, collision_event, function_event, final_event = list(range(5))

class event:
    def __init__ (self, t, d, l):
        debugf ("creating event (data is %d bytes)\n", l)
        self._type = t
        self._edata = d
        self._elength = l
        self._fdata = None
        self._flength = 0
        self._cData = None
        self._clength = 0
        self._cancelled = False
        # the following are the event data values
        self.__point = None
        self.__between = None
        self.__etime = 0.0
        self.__etype = 0
        self.__kind = 0
        self.__id = None
        if self._edata == None:
            debugf ("final or timer event\n")
        else:
            debugf ("*********** current time is %f ***********\n", pgeif.get_time ())
            self.__etime = unpackReal (self._edata) # 8 bytes REAL
            debugf ("*********** event time is %f *************\n", self.__etime)
            if t == collision_event:
                self.__etype = unpackCard (self._edata[8:12]) # 4 bytes etype
                self.__point = unpackPoint (self._edata[12:])
                self.__between = unpackIdPair (self._edata[28:])
                # print "assigning between values", self.__between

                # etype == 0 is a draw frame event
                # etype == 1 two circles colliding
                if self.__etype == 2 or self.__etype == 3:
                    # circle/polygon collision or polygon/polygon collision
                    self.__kind = unpackCard (self._edata[36:])
                debugf ("collision event created which indicates a collision in %f seconds\n", self.__etime)
            elif t == frame_event:
                debugf ("frame event %d in %f seconds\n", t, self.__etime)
            elif t == function_event:
                debugf ("function event %d in %f seconds\n", t, self.__etime)
                self.__etype = unpackCard (self._edata[8:12]) # 4 bytes etype
                self.__id = unpackCard (self._edata[12:16]) # 4 bytes id
            else:
                printf ("unknown event %d in %f seconds\n", t, self.__etime)
    def _set_frame_contents (self, data, length):
        self._fData = data
        self._flength = length
    def _set_colour_contents (self, data, length):
        self._cData = data
        self._clength = length
    def _process (self):
        global id2func
        # printf ("current time %f, moving time forward until this event: %f\n", pgeif.get_time (), self.__etime)
        pgeif.skip_until (self.__etime)
        # printf ("current time is now %f\n", pgeif.get_time ())
        debugf ("_flush_delay\n")
        _flush_delay ()
        debugf ("about to call process_event\n")
        pgeif.process_event ()
        debugf ("find out which event\n")
        if self._type == frame_event:
            self._handle_frame_buffer ()
        elif self._type == collision_event:
            debugf ("collision event seen, in %f seconds\n", self.__etime)
            # pgeif.skip_until (self.__etime)
            self._handle_frame_buffer ()
            _collision (self._between (), self)
        elif self._type == function_event:
            print("_process found timer_event", self.__id)
            i = self.__id
            if i in id2func:
                print("function", i, "about to be called")
                id2func [i] ()
                print("function", i, "finished")
            else:
                print("function", i, "has been cancelled")
    def _handle_frame_buffer (self):
        cData = pgeif.get_cbuf ()
        debugf ("cData len = %d\n", len (cData))
        self._set_colour_contents (cData, len (cData))
        fData = pgeif.get_fbuf ()
        debugf ("fData len = %d\n", len (fData))
        self._set_frame_contents (fData, len (fData))
        draw_frame (self._cData, self._clength,
                    self._fData, self._flength)
        pgeif.empty_fbuffer ()
        pgeif.empty_cbuffer ()
    def _check (self, et):
        if self._type != et:
            printf ("fatal error, unexpected event type\n")
            sys.exit (1)
    def _between (self):
        global id2ob

        self._check (collision_event)
        # returns the two object ids of the colliding objects
        debugf ("id0 = %d, id1 = %d\n", self.__between[0], self.__between[1])
        ob1 = id2ob[self.__between[0]]
        ob2 = id2ob[self.__between[1]]
        return [ob1, ob2]
    def _get_time (self):
        return self.__etime
    def collision_between (self):
        print(self._between ())
        return self._between ()
    def cancel (self):
        self._cancelled = True
    def was_cancelled (self):
        return self._cancelled

def _get_next_event ():
    global device
    debugf ("_get_next_event\n")
    setDefaultDevice ()
    if pgeif.is_collision ():
        debugf ("pgeif.is_collision\n")
        debugf ("pgeif.get_ebuf\n")
        eData = pgeif.get_ebuf ()
        debugf ("event (...\n")
        return event (collision_event, eData, len (eData))
    elif pgeif.is_frame ():
        debugf ("pgeif.is_frame\n")
        debugf ("pgeif.get_ebuf\n")
        eData = pgeif.get_ebuf ()
        # print "testing -> ", unpackReal (eData)
        return event (frame_event, eData, len (eData))
    elif pgeif.is_function ():
        printf ("pgeif.is_function\n")
        eData = pgeif.get_ebuf ()
        return event (function_event, eData, len (eData))
    else:
        printf ("fatal error: unknown event type (terminating simulation)\n")
        sys.exit (1)
        return event (no_event, None, 0)

def _collision (between, event):
    for o in between:
        o._collision (between, event)

def _process (pe):
    pe._process ()

#
#  _add_relative - adds, r a list [time, event]
#                  to the pge_event_queue.  It maintain
#                  their position using a relative ordered queue.
#

def _add_relative (r):
    global pge_event_queue

    if pge_event_queue == []:
        # catch the easy case early
        pge_event_queue = [r]
        return

    abs_time = r[0]
    acc_time = pge_event_queue[0][0]
    if abs_time < acc_time:
        # at the front of the queue
        pge_event_queue[0][0] -= abs_time
        pge_event_queue = [r] + pge_event_queue
        return

    i = 0
    while True:
        if i == len (pge_event_queue)-1:
            # end of queue
            abs_time -= acc_time
            pge_event_queue += [[abs_time, r[1]]]
            return
        i += 1
        if acc_time + pge_event_queue[i][0] < abs_time:
            acc_time += pge_event_queue[i][0]
        else:
            # r needs to be before i
            r = [abs_time - acc_time, r[1]]
            pge_event_queue = pge_event_queue[:i] + [r] + pge_event_queue[i:]
            # and alter relative value of, i
            acc_time += pge_event_queue[i][0]
            if i < len (pge_event_queue)-1:
                pge_event_queue = pge_event_queue[:i] + [[acc_time-abs_time, pge_event_queue[i][1]]] + pge_event_queue[i+1:]
            else:
                pge_event_queue = pge_event_queue[:i] + [[acc_time-abs_time, pge_event_queue[i][1]]]
            return

pge_event_queue = []

def display_element (e, t):
    print("[", e[0], "ms ", end=' ')
    if e[1]._type == frame_event:
        print("displayframe", end=' ')
    elif e[1]._type == collision_event:
        print("collision", end=' ')
    elif e[1]._type == function_event:
        print("timer", end=' ')
    else:
        print("final", end=' ')
    print(" at", e[0] + (int) (t * 1000.0), "ms", end=' ')
    print("], ", end=' ')

def display_event_queue (q):
    if q == []:
        print("event queue is empty")
    else:
        print("event queue: ")
        t = pgeif.get_time ()
        for e in q:
            display_element (e, t)
            t += e[1]._get_time ()
        print("")

prev_event_time = 0.0

def _post_event (e, t):
    global pge_event_queue, debugging, prev_event_time
    if t != -1:
        t = (int) (t * 1000.0)
        _add_relative ([t, e])
        if debugging:
            display_event_queue (pge_event_queue)
        c = pge_event_queue[0][0] + (int) (pgeif.get_time () * 1000.0)
        if c < prev_event_time:
            printf ("clock skew detected\n")
        prev_event_time = c
    return e

def _wait_for_event ():
    global pge_event_queue, slow_down_factor, device, _record, debugging

    if debugging:
        print("_wait_for_event, pge_event_queue =")
        display_event_queue (pge_event_queue)
    if device == pyg_d:
        pygame.event.set_allowed (None)
        pygame.event.set_allowed (allowed_events)

    if pge_event_queue == []:
        return [pygame.event.wait()] + pygame.event.get()

    ms = pge_event_queue[0][0]
    if _record:
        record_time (ms)
    if ms == 0:
        # adding [None] to the list indicates an immediate pge event
        # we obtain all pygame events to avoid starvation of input
        return pygame.event.get() + [None]

    # printf ("setting timer in pygame to %d ms\n", ms)
    pygame.time.set_timer (USEREVENT+1, (int) (ms * slow_down_factor))
    return [pygame.event.wait()] + pygame.event.get()


def _finish_event (t):
    return event (final_event, None, 0)

def at_time (t, p):
    global idcount, id2func, slow_down_factor
    idcount += 1
    pgeif.create_function_event (t / slow_down_factor, idcount)
    id2func[idcount] = p
    return idcount

def at_cancel (i):
    global id2func
    if i in id2func:
        del id2func[i]
    else:
        error ("at_cancel cannot delete function %d as it no longer exists\n", i)

def record ():
    global _record
    _record = True

def draw_frame (cdata, clength, fdata, flength):
    global device

    if device == pyg_d:
        pyg_draw_frame (cdata, clength, fdata, flength)
    else:
        batch_draw_frame (cdata, clength, fdata, flength)

def pyg_draw_frame (cdata, clength, fdata, flength):
    global nextFrame, call, _record

    # printf ("enter pyg_draw_frame: in frame %d  (length = %d bytes)\n", nextFrame, flength+clength)
    # printf ("drawing background\n")
    if _record:
        begin_record_frame (cdata, clength, fdata, flength)
    elif flength > 0:
        draw_background ()
    f = myfile (cdata + fdata)
    while f.left () >= 3:
        header = struct.unpack ("3s", f.read (3))[0]
        header = header[:2]
        if header in call:
            f = call[header] (f)
        else:
            print("not understood header =", header)
            sys.exit (1)
    # printf ("drawing foreground\n")
    if flength > 0:
        draw_foreground ()
    if _record:
        end_record_frame ()
    if flength > 0:
        doFlipBuffer ()  # flipping the buffer for an empty frame looks ugly

    # print "end of draw"
    nextFrame += 1
    debugf ("moving onto frame %d\n", nextFrame)


def _check_opened ():
    global opened, output

    if not opened:
        opened = True
        output = open ("output.raw", "w")


def begin_record_frame (cdata, clength, fdata, flength):
    global opened, output, nextFrame

    _check_opened ()
    output.write (struct.pack ("3s", "fn")) # frame note
    _emit_card (nextFrame)
    if clength > 0:
        debugf ("writing colour data length = %d bytes\n", clength)
        output.write (cdata)
    if flength > 0:
        draw_background ()
        debugf ("writing frame data length = %d bytes\n", flength)
        output.write (fdata)
    else:
        printf ("length of zero!!\n")
        # sys.exit (2)


def end_record_frame ():
    output.write (struct.pack ("3s", "fb")) # flip buffer


def record_time (ms):
    global output, slow_down_factor
    ms = ((float) (ms)) * slow_down_factor / 1000.0
    # print "recording time", ms
    output.write (struct.pack ("3s", "sl")) # sleep
    _emit_double (ms)

def batch_draw_frame (cdata, clength, fdata, flength):
    global opened, output, nextFrame

    debugf ("batch_draw_frame\n")
    if fdata is None:
        printf ("no data in the frame!\n")
        sys.exit (1)
    if not opened:
        opened = True
        output = open ("output.raw", "w")
        nextFrame = 1
    begin_record_frame (cdata, clength, fdata, flength)
    end_record_frame ()
    nextFrame += 1


def gravity (value=-9.81):
    pgeif.gravity (value)

def get_font (size):
    global font, font_size
    if size == font_size:
        return font
    else:
        font_size = size
        font = pygame.font.Font (None, font_size)
        return font

def _init_screen ():
    global resolution, fullscreen, screen_initialised, backcanvas, screen, program_name, version_number

    if not screen_initialised:
        pygame.init ()
        if fullscreen:
            screen = pygame.display.set_mode (resolution, FULLSCREEN)
        else:
            screen = pygame.display.set_mode (resolution)
        screen_initialised = True
        pygame.display.set_caption (program_name + ' ' + version_number)
        backcanvas = pygame.Surface (screen.get_size ())
        backcanvas = backcanvas.convert ()
        backcanvas.fill (Black)


#
#  register_handler - call function if any event in pyevent2func
#                     occurs.
#

def register_handler (function, pyeventlist):
    global pyevent2func, allowed_events
    for e in pyeventlist:
        pyevent2func[e] = function
    allowed_events += pyeventlist

def deregister_handler (pyeventlist):
    global pyevent2func, allowed_events
    for e in pyeventlist:
        del pyevent2func[e]
        allowed_events.remove (e)


#
# runpy - runs pge for time, t, seconds and also
#         process the pygame events.  Each event is
#         passed to procedure, ep.
#

def runpy (t=-1, ep=None):
    global pge_event_queue

    _init_screen ()
    pgeif.use_time_delay (False)
    cData = pgeif.get_cbuf ()
    fData = pgeif.get_fbuf ()
    _check_opened ()
    draw_frame (cData, len (cData), fData, len (fData))
    pgeif.empty_fbuffer ()
    pgeif.empty_cbuffer ()
    if pge_event_queue == []:
        # no events yet, so collect the next from the physics engine
        ev = _get_next_event ()
        nev = _post_event (ev, ev._get_time ())
    # always add the a final event which is the only way to finish the while loop
    fin = _post_event (_finish_event (t), t)
    while True:
        for e in _wait_for_event ():
            if (e == None) or (e.type == USEREVENT+1):
                # immediate pge event pending or user event has fired
                # take the event off queue
                pe = pge_event_queue [0][1]
                pge_event_queue = pge_event_queue [1:]
                if pe == fin:
                    # the finish event, we're done and out of here
                    return
                _process (pe)
                ev = _get_next_event ()
                nev = _post_event (ev, ev._get_time ())
            elif e.type in pyevent2func:
                pyevent2func[e.type] (e)
        # what does this code do??
        # elif ep != None:
        #   if nev._get_time () >= cur_time ():
        #      pgeif.advance_time (cur_time ())
        #   ep (e)


#
#  runbatch - runs pge for time, t.  If t < 0.0 then simulate for 30.0 seconds max.
#

def runbatch (t):
    if t < 0.0:
        t = 30.0
    debugf ("runbatch (%f)\n", t)
    pgeif.check_objects ()
    cData = pgeif.get_cbuf ()
    fData = pgeif.get_fbuf ()
    draw_frame (cData, len (cData), fData, len (fData))
    pgeif.empty_fbuffer ()
    pgeif.empty_cbuffer ()
    nev = _get_next_event ()
    acc = 0.0
    while acc+nev._get_time () < t:
        old = acc
        acc = acc + nev._get_time ()
        delay (nev._get_time ())
        if int(acc) != int(old):
            printf ("%d/%d seconds completed %d%%\n", int (acc), int (t), int (acc*100.0/t))
        _process (nev)
        nev = _get_next_event ()

def display_set_mode (r):
    global resolution
    resolution = r

def display_fullscreen (b):
    global fullscreen
    fullscreen = b

def fps (f):
    global framesPerSecond
    framesPerSecond = f

#
#  run - runs pge for time, t, seconds and also
#        process the pygame events
#

def run (t=-1, ep=None):
    global device

    setDefaultDevice ()
    pgeif.fps (framesPerSecond)
    if device == pyg_d:
        runpy (t, ep)
    else:
        runbatch (t)


def setDevice (d):
    global device

    if device == None:
        device = d
        if device == pyg_d:
            pygame.init ()
        pgeif.use_buffer ()
    else:
        printf ("cannot change device once pge has started\n")


def setDefaultDevice ():
    global device

    if device == None:
        device = pyg_d
        pgeif.use_buffer ()


def batch ():
    setDevice (batch_d)


def interactive ():
    setDevice (pyg_d)


def finish ():
    global output, opened
    if opened:
        output.close ()
        opened = False

def load_sound (name):
    class NoneSound:
        def play(self):
            pass
    if not pygame.mixer or not pygame.mixer.get_init():
        return NoneSound()
    try:
        sound = pygame.mixer.Sound(name)
    except pygame.error as message:
        print('cannot load sound file:', name)
        return NoneSound()
    return sound


def play (name):
    global output

    _flush_delay ()
    if device == pyg_d:
        s = load_sound (name)
        s.play ()
    else:
        output.write (struct.pack ("3s", "ps"))
        output.write (name)
        output.write ('\0')


#
#  message - write out text to the output.
#

def message (text):
    output.write (struct.pack ("3s", "ms"))
    output.write (text)


#
#  turn the drawing of collision frames on or off.
#
#        actual:   determines whether an extra frame is generated
#                  at the time of actual collision.
#        predict:  draws a frame predicting the next collision.
#                  It will show the points predicted to collide.
#

def draw_collision (actual, predict):
    pgeif.draw_collision (actual, predict)


#
#  collision_colour - if draw_collision is called and
#                     with its first parameter, actual = True, then
#                     the objects in collision will have colour, c.
#

def collision_colour (c):
    c._param_colour ("first parameter to collision_colour is expected to be a colour")
    pgeif.set_collision_colour (c._get_pgeif_colour ())


def dump_world ():
    pgeif.dump_world ()


def _draw (l):
    if l != []:
        for i in l:
            for o in levels[i]:
                o._draw ()


#
#  _flush_delay - write out or implement the collected delay time.
#

def _flush_delay ():
    global lastDelay

    if lastDelay > 0.0:
        debugf ("delay of %f\n", lastDelay)
        if device == pyg_d:
            time.sleep (lastDelay)
        else:
            output.write (struct.pack ("3s", "sl"))
            output.write (struct.pack ("d", lastDelay))
        lastDelay = 0.0


#
#  delay - introduce a delay for, t.
#

def delay (t):
    global lastDelay
    lastDelay += t

def slow_down (t):
    global slow_down_factor
    slow_down_factor = t

def process_event ():
    pgeif.process_event ()


def readShort (f):
    b = f.read (2)
    c = struct.unpack ('!H', b)[0]
    return f, c


def toCol (f):
    return toFloat (f)*255

#
#  readFract - returns three integers:  w, n, d
#              representing fraction.
#

def readFract (f):
    b = f.read (1)
    v = struct.unpack ("B", b)[0]

    if v == 0:
        return f, (0, 0, 0)
    elif v == 1:
        return f, (1, 0, 0)
    elif v == 2:
        b = f.read (8*2)
        r = struct.unpack('!QQ', b)
        return f, (0, r[0], r[1])
    else:
        b = f.read (8*3)
        return f, struct.unpack('!QQQ', b)

#
#  mults -
#

def mults (s, f):
    if s == 0:
        return 0
    if f[1] == 0 or f[2] == 0:
        return f[0]*s
    return f[0]+f[1]*s/f[2]


def toFloat (f):
    if f[1] == 0 or f[2] == 0:
        return float(f[0])
    return float(f[0]) + float(f[1])/float(f[2])


def doRegisterColour (f):
    global idTOcol, debugging

    f, c = readShort (f)
    f, rf = readFract (f)
    f, gf = readFract (f)
    f, bf = readFract (f)
    if debugging:
        print(rf, gf, bf)
    r = toCol (rf)
    g = toCol (gf)
    b = toCol (bf)
    idTOcol[c] = (r, g, b)
    return f

def doExit (f):
    debugf ("doExit called\n")
    sys.exit (0)
    return f

#
#  readColourRaw - returns the file and colour id (short).
#

def readColourRaw (f):
    f, c = readShort (f)
    return f, c


#
#  readColour - returns the file and colour triple.
#

def readColour (f):
    f, c = readColourRaw (f)
    debugf ("colour value %d\n", c)
    col = idTOcol[c]
    return f, col


#
#  drawFillPolygon -
#

def doDrawFillPolygon (f):
    global screen, debugging

    debugf ("doDrawFillPolygon\n")
    f, n = readShort (f)
    l = []
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print(xf, yf, end=' ')
        x = mults (resolution[0], xf)
        y = mults (resolution[1], yf)
        l += [[x, flip (y)]]

    f, c = readColour (f)
    if debugging:
        print("drawFillPolygon (colour =", c, " l =", l, ")")
    pygame.draw.polygon (screen, c, l, 0)
    return f


#
#  flip - returns the y value flipped against the resolution.
#

def flip (y):
    global resolution
    return min (resolution[0], resolution[1])-y


#
#  doDrawFillCircle -
#

def doDrawFillCircle (f):
    global screen, debugging

    f, xf = readFract (f)
    f, yf = readFract (f)
    f, rf = readFract (f)
    x = mults (resolution[0], xf)
    y = mults (resolution[1], yf)
    r = mults (resolution[0], rf)

    f, c = readColour (f)
    debugf("circle  x = %d  y = %d,  r = %d\n", x, y, r)
    if debugging:
        print("  colour =", c)
    pygame.draw.circle (screen, c, (x, flip (y)), r, 0)
    return f


def wait_for_n ():
    printf ("press 'n' to continue\n")
    while True:
        e = pygame.event.wait ()
        # print e
        if e.type == KEYDOWN and e.key == K_n:
            printf (" ... continuing\n")
            return

#
#  flipBuffer - flips the screen buffer.
#

def doFlipBuffer ():
    global background, screen, nextFrame, backcanvas, program_name

    # printf ("doFlipBuffer called for frame (%d)\n", nextFrame)
    pygame.display.set_caption (program_name + ' ' + version_number + ' (%d)' % (nextFrame))
    pygame.display.flip ()
    screen.blit (backcanvas, (0, 0))
    # wait_for_n ()

def doSleep (f):
    global lastDelay
    f, t = readReal (f)
    # lastDelay += t
    return f


def readReal (f):
    b = f.read (8)
    return f, struct.unpack ("d", b)[0]


#
#  doDrawPolygon -
#

def doDrawPolygon (f):
    global debugging, screen

    f, n = readShort (f)
    l = []
    debugf ("drawPolygon: %d", n)
    for i in range (n):
        f, xf = readFract (f)
        f, yf = readFract (f)
        if debugging:
            print(xf, yf, end=' ')
        x = mults (resolution[0], xf)
        y = mults (resolution[1], yf)
        l += [[x, flip(y)]]

    f, t = readFract (f)
    if debugging:
        print("draw polygon", l, "thickness", t)
    # pygame.draw.polygon (screen, c, l, 0)
    return f

def doMessage (f):
    text = ""
    b = f.read (1)
    while int(b) != 0:
        text += b
        b = f.read (1)
    printf ("Frame [%d]: %s\n", nextFrame, text)


def doPass (f):
    return f


call['rc'] = doRegisterColour
call['dp'] = doDrawPolygon
call['dP'] = doDrawFillPolygon
# call['dc'] = doDrawCircle
call['dC'] = doDrawFillCircle
call['fb'] = doPass
# call['fr'] = doFramesPerSecond
call['ex'] = doExit
call['sl'] = doSleep
# call['ps'] = doPlay
# call['fn'] = doFrameNote
call['ms'] = doMessage


#
#  coordinate geometry utilities
#

#
#  pyg_to_unit_coord - inputs :  v  a list of two integers in the range 0..xresolution, 0..yresolution.
#                      returns:  a list of two floating point numbers between 0.0 and 1.0

def pyg_to_unit_coord (v):
    global resolution

    if resolution == None:
        errorf ("you must assign the screen resolution with a call to 'display_set_mode' before calling 'pyg_to_unit_coord'\n")
    if len (v) == 2:
        return [(float) (v[0]) / (float) (resolution[0]),
                (float) (resolution[1] - v[1]) / (float) (resolution[1])]
    else:
        errorf ("'pyg_to_unit_coord' expects a list of two integers\n")


#
#  normalise - input a vector [x, y]
#              return the vector after it has been normalised.
#

def normalise (v):
    x = (float) (v[0])
    y = (float) (v[1])
    l = magnitude ([x, y])
    return [x/l, y/l]


#
#  magnitude - return the modulus or magnitude of a vector or
#              the Pythagorean value of the vector.
#

def magnitude (v):
    return math.sqrt (v[0]*v[0] + v[1]*v[1])

#
#  sub_coord - returns the vector a - b.
#

def sub_coord (a, b):
    return [a[0]-b[0], a[1]-b[1]]
