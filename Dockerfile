FROM ubuntu:latest

RUN apt-get update -qq && apt-get install -y wget git-core

RUN wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update -qq && apt-get install -y erlang=1:18.2 build-essential

RUN mkdir /mylib
WORKDIR /mylib
ADD . /mylib

RUN ./rebar get-deps
