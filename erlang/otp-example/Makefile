
compile:
	erl -make

start:
	make compile
	erl -pa ebin/ -eval 'application:load(hello_app), application:start(hello_app), observer:start().'
