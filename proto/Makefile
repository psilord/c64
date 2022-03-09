64TASS_FLAGS = -C -a -Wall
64TASS = 64tass $(64TASS_FLAGS)
C1541 = c1541
TARGET = proto
TARGET_PRG = $(TARGET).prg
TARGET_SRC = $(TARGET).s
TARGET_LST = $(TARGET).lst
TARGET_D64 = $(TARGET).d64
X64SC_FLAGS = +sound -autostart $(TARGET_D64)
X64SC = x64sc

$(TARGET_D64): $(TARGET_PRG)
	$(C1541) -format diskname,id d64 $(TARGET_D64) -attach $(TARGET_D64) -write $(TARGET_PRG) $(TARGET)

$(TARGET_PRG): $(TARGET_SRC)
	$(64TASS) -L $(TARGET_LST) --verbose-list --cbm-prg -o $(TARGET_PRG) $(TARGET_SRC)

test: $(TARGET_D64)
	$(X64SC) $(X64SC_FLAGS)

list: $(TARGET_PRG)
	less $(TARGET_LST)

clean:
	rm -f $(TARGET_PRG) $(TARGET_LST) $(TARGET_D64)

