
compile:
	erl -make

start: compile
	erl -pa ebin/ -eval 'application:load(hello_app), application:start(hello_app), observer:start().'

testing:
	concuerror -f test/hello_test.erl -m hello_test -t test --pa src
