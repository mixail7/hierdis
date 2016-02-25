.PHONY: all clean docker_build docker_run

all:
	./rebar get-deps compile

clean:
	./rebar clean

CONTAINER_NAME=hierdis

docker_build:
	docker build -t $(CONTAINER_NAME) .

docker_run:
	docker run -it --rm -v "$(CURDIR)":/mylib -w /mylib $(CONTAINER_NAME) /bin/bash
