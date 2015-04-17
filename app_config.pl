:- module(app_config, [ load_config/1
		      , config/2
		      , config_values/2
		  ]).


:-use_module(library(optparse)).

:- dynamic config_module/1.
load_config(F):-
	absolute_file_name(F, FPath, [file_type(prolog)]),
	(   exists_file(FPath)
	->  load_files(FPath, []),
	    source_file_property(FPath, module(Mod)),
	    assertz(config_module(Mod))
	;   throw(config_file_missing(F))
	).

prolog:message(config_file_missing(F)) -->
	{ format(string(S), 'Configuration file is missing: ~q', [F]) },
	[S].



:- dynamic production_level/1,
	config_file/1,
	positional_args/1,
	show_help/1.

arg_spec([
    [ opt(production_level), type(atom)
    , default(dev)
    , shortflags([p])
    , longflags([production_level])
    , help([ 'Configured production level one of'
	   | LevelDescs
	   ])
    ],
    [ opt(config_file), type(atom)
    , default('config')
    , shortflags([c])
    , longflags([config_file])
    , help('Configuration file to load with app_config')
    ],
    [ opt(show_help), type(boolean)
    , default(false)
    , shortflags([h])
    , longflags([help])
    , help('Show this help menu')
    ]
    | MoreSpecs
	 ]) :-
	findall(LevelDesc,
		( config_values(env(Level), Desc)
		,  atomic_list_concat([Level, Desc], ': ', LevelDesc)
		), LevelDescs),
	findall(Spec, config_values(arg_spec, Spec), MoreSpecs).

load_argv :-
	arg_spec(ArgSpec),
	opt_arguments(ArgSpec, Opts, PA),
	maplist(assertz, Opts),
	assertz(positional_args(PA)).

print_help :-
	show_help(true),
	!,
	arg_spec(AS),
	opt_help(AS, Output),
	writeln(Output),
	halt.
print_help.


:- multifile config/3.
config(K, V) :-
	config_values(K, V),
	!. %only one response by default
config_values(K, V) :-
	(   production_level(ProdLevel),
	    once(config_module(M))
	->  M:config(K, ProdLevel, V)
	).
config_values(K, V) :-
	(   production_level(ProdLevel)
	->  config(K, ProdLevel, V)
	).




%%	RUNTIME LOAD ARGS
%
:- load_argv.
:- config_file(F), load_config(F).
:- print_help.



%%	Provide some global settings based on global config
config(production_level, P, P).
config(config_file, _, F) :- config_file(F).
config(config_module, _, M) :- config_module(M).
config(env_desc, E, Desc) :- config(env(E), Desc).
