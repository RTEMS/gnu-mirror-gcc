PYTHON_VERSION=3

all: force
	swig -python MyStrLib.i
	gcc -c -fPIC MyStrLib_wrap.c -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fshared -c -fPIC -g MyStrLib.mod
	gm2 -fmakeall -I. -fPIC -g -shared -fshared MyStrLib.mod MyStrLib_wrap.o -o _MyStrLib.so
	python$(PYTHON_VERSION) testlen.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* MyStrLib.py *.lst *_m2.cpp

force:
