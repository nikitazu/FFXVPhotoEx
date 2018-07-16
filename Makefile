BIN_DIR=bin
SRC_DIR=src

TARGET=$(BIN_DIR)/ffxvphotoex

SRC_UNITS=$(SRC_DIR)/*.pas
SRC_PROJECT=$(SRC_DIR)/ffxvphotoex.lpr


# Build targets
# =============

all: $(TARGET)

$(TARGET): $(SRC_UNITS) $(SRC_PROJECT)
	lazbuild $(SRC_PROJECT)

clean:
	rm -rf $(BIN_DIR)

