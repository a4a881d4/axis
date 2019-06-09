root = ../../../..
cc = $(root)/bin/zcc -I$(root)/src/zinclude

ALL: $(root)/.work/ledrom_romonly.vhd
loop:$(root)/.work/loop_romonly.vhd

# led	
$(root)/.work/led.psm : led.c
	$(cc) led.c > $(root)/.work/led.psm
$(root)/.work/ledrom_romonly.vhd : $(root)/.work/led.psm
	cd $(root)/.work && ../bin/zas ledrom led.psm
	
# loop	
$(root)/.work/loop.psm : loop.c
	$(cc) loop.c > $(root)/.work/loop.psm
$(root)/.work/loop_romonly.vhd : $(root)/.work/loop.psm
	cd $(root)/.work && ../bin/zas loop loop.psm
