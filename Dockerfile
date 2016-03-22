FROM erlang:latest

RUN mkdir /mylib
WORKDIR /mylib
ADD . /mylib

RUN ./rebar3 compile
