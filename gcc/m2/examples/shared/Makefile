prefix = ${HOME}/opt


all: clean
	libtool --tag=CC --mode=compile gm2 -g -c a.mod -o a.lo
	libtool --tag=CC --mode=compile gm2 -g -c b.mod -o b.lo
	libtool --tag=CC --mode=compile gm2 -g -c c.mod -o c.lo

	gm2 -c -g -fmakelist c.mod
	gm2 -c -g -fmakeinit -fshared c.mod
	libtool --tag=CC --mode=compile g++ -g -c _m2_c.cpp -o _m2_c.lo

	libtool --mode=link gcc -g _m2_c.lo a.lo b.lo c.lo \
          -L$(prefix)/lib64 \
          -rpath `pwd` -lgm2 -lstdc++ -lpth -lm -o libabc.la 

clean: force
	$(RM) *.so *.la *.[oa] *.cpp *.lo *.lst *~
	$(RM) -rf .libs

force:
