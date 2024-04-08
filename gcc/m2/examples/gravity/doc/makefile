
all: circle-line.pic circle-line2.pic circle-line3.pic circle-line4.pic collision.ps

%.pic: %.fig
	fig2dev -Lpic $< > $@

collision.ps: collision.ms
	groff -spet -ms $< > $@

develop: force
	./wysiwyg groff -spet -ms collision.ms

clean: force
	$(RM) *.pic *.ps

force:
