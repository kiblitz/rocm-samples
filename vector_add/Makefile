CC = hipcc
CFLAGS = -Wall -Wextra -fPIC -shared

SRC = vectoradd_hip.cpp
HDR = vectoradd_hip.hpp
OUT = vectoradd_hip.so

all: $(OUT)

$(OUT): $(SRC) $(HDR)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(OUT)
