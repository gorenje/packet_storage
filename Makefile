# compile our erlang code automagically
.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<
.yrl.erl:
	erlc -W $<

all: compile

ERL = erl -boot start_clean
PL_HOSTS=$(shell cat ~/.csshrc | ruby parse_csshrc.rb)
MODS = network \
       networkcreate \
       networkexamples \
       networkhelpers \
       networkpacket \
       networkroutingtable \
       networkrubyengine \
       environmenthelpers

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump *~

erlang.tgz: Makefile $(wildcard *.erl) $(wildcard *.rb)
	rm -fr erlang.tgz
	tar cfz erlang.tgz Makefile *.erl *.rb

update-servers: erlang.tgz
	scp $< open-source-consultants.de:~/erlang

update-planetlab: erlang.tgz
	V=$(foreach HOST,${PL_HOSTS}, echo copying to ${HOST} ...; scp $< ${HOST}:~/erlang ; )
	$(shell ${V})
	scp $< 5o0.de:~/erlang
#
# following are done on the nodes themselves
#
build:
	tar xfz erlang.tgz
	make

ERL_COOKIE=cb8ec82545e6179c2cd46c876c15b479
start-erl:
	erl -name $(shell hostname -s)@$(shell hostname) -setcookie ${ERL_COOKIE}
	killall epmd

start-erl-bf:
	erl -name dog@5o0.de -setcookie ${ERL_COOKIE}
	killall epmd

start-erl-applepie:
	erl -name dog@ososos.dyndns.de -setcookie ${ERL_COOKIE}
	killall epmd
