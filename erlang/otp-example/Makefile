
compile:
	erl -make

start: compile
	erl -pa ebin/ -eval '\
		application:load(hello_app),\
		application:start(hello_app),\
		observer:start()'

client_start: compile
	erl -pa ebin/ -eval '\
		application:load(hello_app), application:start(hello_app),\
		hello_client:main().'

testing:
	concuerror -m hello_test -t test --pa ebin

dialyze: compile
	dialyzer\
		ebin\
		--no_check_plt\
		-Wunmatched_returns -Werror_handling\
		-Wrace_conditions\
		-Wunderspecs -Woverspecs

xref:
	erl -pa ebin/ -eval '\
		xref:start(s), xref:add_application(s, "./ebin"), \
		xref:analyze(s, undefined_function_calls), \
		xref:q(s, "(Fun) hello : Mod").'
