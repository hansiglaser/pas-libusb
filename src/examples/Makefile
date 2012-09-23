
EXECUTABLES = test1library test2controlsync test3controlasync testfirmware
FPC_OPTS = -Fu..


all: $(EXECUTABLES)

%: %.pas
	fpc $(FPC_OPTS) $<

clean:
	rm -f *.o *.ppu *~ *.bak $(EXECUTABLES)
