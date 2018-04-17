
all:
	erlc hello.erl

run: all
	erl -boot start_clean -pa mochiweb/ebin -s hello start

prepare:
	git clone git://github.com/mochi/mochiweb.git
	cd mochiweb
	make
