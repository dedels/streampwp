:- module(llama_compile,
	  [ op(1100, fx, def)
	  , op(1000, xfy, (++))
	  , llama:def_data/2
	  , llama/0, llama_end/0
	  , llama_expand/2
	  ]).

:- multifile llama:def_data/2, llama:def_data/3.
:- discontiguous llama:def_data/2, llama:def_data/3.


throw_llama_parse(Term) :-
    (   Term=(def _ ++_)
    ;   Term=(def _)
    ),
    throw(llama_parse('Invalid Signature for def data', Term)).

llama_expand(V, V) :-
    var(V), !. %dont expand vars!
llama_expand((def data(Type, Shape)),Tout) :-
    (   llama_expand(Type, Shape, [], Tout)
    ->  true
    ;   throw_llama_parse((def data(Type,Shape)))
    ).
llama_expand((def data(Type, Shape) ++ Accessors), Transformed) :-
    (   defopts_to_list(Accessors,AccsList),
        llama_expand(Type, Shape, AccsList, PredsOut),
	post_processing(AccsList, PredsOut, Transformed)
    ->  true
    ;   throw_llama_parse((def data(Type,Shape) ++Accessors))
    ).


llama_expand([], []).
llama_expand([A|R], [Aex|Rex]) :-
    !,
    llama_expand(A, Aex),
    llama_expand(R, Rex).
llama_expand({{O}}, {O}). %escape llama with double brackets
llama_expand({O}, Obj) :-
    build_llama_object({O}, _, Obj).
llama_expand(Term, Tout) :-
    Term=..[Func | Args],
    llama_expand(Args, ArgsExpand),
    Tout=..[Func | ArgsExpand].



llama:typeof(Type, Obj) :-
    var(Type), !,
    llama:def_data(Type, Obj).
llama:typeof(Type, Obj) :-
    prolog_load_context(module,Mod),
    Mod:call(Type, Obj).
llama:typeof(Type, Obj, Obj) :-
    llama:typeof(Type, Obj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Expand def data
%   def data(type(T), shape(A,B)) ++ accessor=A.
%   accessor/2 as getter
%   accessor/3 as setter
%   type(T, shape(A,B)) as constructor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
llama_expand(Type, Shape, OptsList, [llama:def_data(ModuleType, ModuleShape), Ctor | Accessors]) :-
    Type=..TypeArr,
    llama_module_shape(Shape, ModuleShape),
    llama_module_shape(Type, ModuleType),

    append(TypeArr, [ModuleShape], CtorArr),
    Ctor=..CtorArr,
    build_accessors(ModuleShape, OptsList, Accessors).

%make add module to the shape if it is not presnt
llama_module_shape(Module:Shape, Module:Shape) :- !.
llama_module_shape(Shape, Module:Shape) :-
    prolog_load_context(module,Module).



build_accessor_key(M:Key, M, Key) :- !.
build_accessor_key(Key, M, Key) :-
	prolog_load_context(module, M).

build_accessors(_, [options(_)], []).
build_accessors(_, [], []).
build_accessors(Shape, [Key=Val | RestOpts],
		[ M:Getter
		, M:Setter
		| RestAccessors]) :-
    build_accessor_key(Key, M, KeyNormal),
    remove_var(Val, Shape, ShapeHole),
    Getter=..[KeyNormal, Val, Shape],
    Setter=..[KeyNormal, Val, ShapeHole, Shape],
    compile_aux_clauses([ (:- discontiguous KeyNormal/2)
			, (:- discontiguous KeyNormal/3)
			]),
    build_accessors(Shape, RestOpts, RestAccessors).

%traverse Shape and if we see any Val var inside shape, replace with _
remove_var(Val, Shape, _) :-
    Val==Shape, !.
remove_var(_, Shape, Shape) :-
    var(Shape), !. %not equal in last pred, just stop here.
remove_var(Val, Shape, ShapeHole) :-
    Shape=..[F|Args],
    maplist(remove_var(Val), Args, ArgsRem),
    %remove_var(Val, Args, ArgsRem),
    ShapeHole=..[F|ArgsRem].

%%  defopts_to_list(DefOpts, List)
%   turn a cons-list of ++(A, ++(B, C)) to [A, B, C]
defopts_to_list(A++B,[A | Bl]) :-
    !,
    defopts_to_list(B, Bl).
defopts_to_list(A, [A]).


%%  post_processing(AccsList, PredsOut, Transformed)
%
%   make one more pass through PredsOut and output transformed
post_processing(AccsList, PredsIn, PredsOut) :-
	post_processing_prefix(AccsList, PredsIn, Preds0) ,
	post_processing_export(AccsList, Preds0, PredsOut).
post_processing_prefix(AccsList, PredsIn, [ Def2, ExtraDef, Ctor
					  | AccessorsOut
					  ]) :-
	(   memberchk(options(Opts), AccsList),
	    memberchk(prefix(Pfx0), Opts),
	    nonvar(Pfx0),
	    (	sub_atom(Pfx0, _, _, 0, '_') %make sure an underscore is added
	    ->	Pfx=Pfx0
	    ;	atom_concat(Pfx0, '_', Pfx)
	    )
	->  true
	;   Pfx=''
	),
	PredsIn=[Def2,Ctor|Accessors],
	Def2=(llama:def_data(Type,Shape)),
	ExtraDef=[llama:def_data(Type,Shape,Lookup)],
	maplist(apply_prefix(Pfx), Accessors, AccessorsOut, LookupUnsort),
	sort(LookupUnsort, Lookup).
apply_prefix(Pfx, M:PredIn, M:PredOut, F-(M:Fnew)) :-
	apply_prefix(Pfx,PredIn,PredOut,F-Fnew).
apply_prefix(Pfx, PredIn, PredOut, F-Fnew) :-
	PredIn=..[F|Args],
	atom_concat(Pfx, F, Fnew),
	PredOut=..[Fnew|Args].



post_processing_export(AccsList, PredsOut, PredsOut) :-
	memberchk(options(Opts), AccsList),
	memberchk(export, Opts),
	!,
	prolog_load_context(module,Mod),
	forall( member(Pred, PredsOut)
	      , llama_export(Mod, Pred)).
post_processing_export(_, PredsOut, PredsOut).

llama_export(Mod, (Head:-_)) :-
        llama_export(Mod, Head).
llama_export(Mod, (Mod:Pred)) :-
	!, llama_export(Mod,Pred).
llama_export(_, (_:_)). %ignore preds in other modules!
llama_export(Mod, Pred) :-
	functor(Pred, F, A),
	(   module_property(Mod, exports(Exports)),
	    \+ memberchk(F/A, Exports)
        ->  Mod:export(F/A)
	;   true
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Expand predicates
%   {type, a=b, c=d}
%   {a=b, c=d}
%   {X is {NESTED}}
%       {X is {type, a=b, c={Y={abc=1}} }}
%   {X+{NESTED}}
%   {X-accessor}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%  ensure that Type is ok to be inside the type arg of a llamaobj
llama_type_sig(Type) :-
    functor(Type, F, _),
    \+ member(F, [(+), (is), (,), (=)]).


llama_call_accessor(DoAccess) :-
	(   DoAccess=call(Accessor, Val, Obj)
	->  DoAccessPfx=call(PrefixedAccessor,Val,Obj)
	;   DoAccess=call(Accessor, Val, Obj, Obj2)
	->  DoAccessPfx=call(PrefixedAccessor,Val,Obj,Obj2)
	),
	prolog_load_context(module, Mod),

	(   llama:def_data(_, Obj, AccessorLookup),
	    memberchk(Accessor-PrefixedAccessor, AccessorLookup)
	->  true
	;   PrefixedAccessor=Accessor %don't prefix
	),
	Mod:DoAccessPfx.



%%  build_llama_object(LLamaObject, VarStart, BindVar)
%   Take a llamaobject in and give back a bound var, binding
%   subobjects properly.
%   param LLamaObject - unparsed source decl
%   param VarStart - object to apply updates to
%   param BindVar - object to put back into source
%
%%{X as {NESTED}}
build_llama_object({BindVar is {Args}}, Start, BindVar) :-
    !,
    build_llama_object({Args}, is(Start), BindVar).
build_llama_object({BindVar is Type}, Start, BindVar) :-
    !,
    build_llama_object({Type}, is(Start), BindVar).

%%{X+{NESTED}}
build_llama_object({BindVar+{Args}}, Start, ObjOut) :-
    !,
    BindVar=Start,
    build_llama_object({Args}, +BindVar, ObjOut).

%%{X-accessor}
build_llama_object({SourceVar-Accessor}, _, ObjOut) :-
	!,
	(   SourceVar=(_-_) %multilevel deep, expand inside
	->  llama_expand({SourceVar}, SourceVarExp)
	;   SourceVarExp=SourceVar
	),
	llama_call_accessor(call(Accessor, ObjOut, SourceVarExp)).





%%{type}
build_llama_object({Type}, Start, ObjOut) :-
    llama_type_sig(Type),
    !,
    build_llama_object(Type, Start, [llama:typeof=Type], ObjOut).

%%{type, a=b, c=d}
build_llama_object({Type, Args}, Start, ObjOut) :-
    llama_type_sig(Type),
    !,
    flatten_cons(Args, ArgsList),
    build_llama_object(Type, Start, [llama:typeof=Type | ArgsList], ObjOut).

%%(a=b, c=d}
build_llama_object({Args}, Start, ObjOut) :-
    flatten_cons(Args, ArgsList),
    build_llama_object(_, Start, ArgsList, ObjOut).


flatten_cons((A,B),[A|Bf]) :-
	!,
	flatten_cons(B, Bf).
flatten_cons(T, [T]).

fix_llama_value(Value, ValueE) :-
    nonvar(Value),
    Value={_},
    !,
    llama_expand(Value, ValueE).
fix_llama_value(Val, Val).

%%  build_llama_object(+Type, +Start, +[Args], -OutVar)
%   use the provided information to build an outvar
%   if start is +(Bind) then apply key/3
%   if start is is(Bind) then apply key/2
build_llama_object(_Type, Obj, [], BindV) :-
    arg(1, Obj, BindV). %pull of the functor on Obj
build_llama_object(Type, is(BindVar), [Key=Value|RestArgs], BindVar) :-
    fix_llama_value(Value, ValueE),
    llama_call_accessor(call(Key, ValueE, BindVar)),
    build_llama_object(Type, is(BindVar), RestArgs, BindVar).
build_llama_object(Type, +(Start), [Key=Value|RestArgs], ObjOut) :-
    fix_llama_value(Value, ValueE),
    llama_call_accessor(call(Key, ValueE, Start, Intermediate)),
    build_llama_object(Type, +Intermediate, RestArgs, ObjOut).









%test
user:a(a(_, _)).
user:first(A, a(A, _)).
user:first(A, a(_, S), a(A, S)).
user:second(A, a(_, A)).
user:second(A, a(F, _), a(F, A)).

user:testpred(E) :-
 gtrace, llama_expand((my({a, first=A}):-A=10), E).
user:testpred2(E):-
 gtrace, llama_expand((my({A is {a, first=A1val}}, {A+{second=100}}):- A1val=10 ), E).
user:testpred3(E):-
 gtrace, llama_expand((my({A is {second=2}}, {A+{first=1}})), E).
user:testpred4(E):-
 gtrace, llama_expand((my({first=1, second=2})), E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Setup term_expansion at the very last step!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
llama :- throw(not_supported).
llama_end :- throw(not_supported).

:- dynamic llama:llama_on/1.
:- multifile llama:llama_on/1.
llama:llama_on(_) :- fail.
llama:llama_on :-
    prolog_load_context(module,Mod),
    llama:assert(llama_on(Mod)).

llama:llama_off :-
    prolog_load_context(module,Mod),
    llama:retractall(llama_on(Mod)).


user:term_expansion((:- llama), []) :-
    llama:llama_on.
user:term_expansion((:- llama_end), []) :-
    llama:llama_off.
user:term_expansion(Term, TermOut) :-
    Term \= end_of_file,
    prolog_load_context(module,Mod),
    llama:llama_on(Mod),
    (   functor(Term, '-->', _)
    ->  dcg_translate_rule(Term,Term0)
    ;   Term=Term0
    ),
    llama_expand(Term0, TermOut).
user:term_expansion(end_of_file, _) :-
    llama:llama_off,
    fail.
