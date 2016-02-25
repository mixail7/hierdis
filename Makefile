.PHONY: all clean

all:
	./rebar get-deps compile

clean:
	./rebar clean
