

all:  force
	gm2 -g -c -flibs=min -fno-exceptions minhello.mod
	gm2 -g -flibs=min -fno-exceptions minhello.mod
	size a.out

clean:  force
	$(RM) a.out a.exe *.cpp *.o *.obj

force:
