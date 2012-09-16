
EXECUTABLES = test1library test2controlsync testfirmware
FPC_OPTS = -Fu..


all: $(EXECUTABLES)

%: %.pas
	fpc $(FPC_OPTS) $<

clean:
	rm -f *.o *.ppu *~ *.bak $(EXECUTABLES)
