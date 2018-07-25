
compile:
	erl -make

start: compile
	erl -pa ebin/ -eval 'application:load(hello_app), application:start(hello_app), observer:start().'

testing:
	concuerror -f test/hello_test.erl -m hello_test -t test --pa src

dialyze:
	dialyzer ebin/

xref:
	erl -pa ebin/ -eval '\
		xref:start(s), xref:add_application(s, "./ebin"), \
		xref:analyze(s, undefined_function_calls), \
		xref:q(s, "(Fun) hello : Mod").'
