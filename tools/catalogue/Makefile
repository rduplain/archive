TARGET = Catalogue

all: $(TARGET)

clean:
	$(RM) $(TARGET) *.chi *.hi *.o

force:
	touch Catalogue.lhs
	$(MAKE)

%.hs: %.chs
	c2hs $<

%: %.lhs
	ghc --make -fglasgow-exts -ffi $^ CloseFile.c -lcfitsio -lwcs -L/export/data_2/usr/lib

$(TARGET): FitsIO.hs WCS.hs
