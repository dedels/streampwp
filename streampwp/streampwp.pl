/** <module> Streaming PWP

This module uses PWP (Prolog-WellFormed-Pages) syntax
and compiles predicates that print that page.

@author David Edelstein dbe2@alum.lehigh.edu

*/
:- module(streampwp, [ pwp_compile_file/1  % File
		     , pwp_compile_file/2  % File, Options
		     , pwp_compile_files/1 % Directory
		     , pwp_compile_files/2 % Directory, Options
		     ]).

:- use_module(library(sgml),       [load_xml_file/2]).
:- use_module(library(gensym),     [gensym/2]).
:- use_module(library(debug)).

:- use_module(llama/llama_compile).

:- llama.

def data(compilation_obj,
         co(Module, CurrentPred-Derived, Ctx, Binding))
    ++module=Module
    ++curpred=CurrentPred
    ++derived=Derived
    ++context=Ctx
    ++binding=Binding
    ++options([prefix(co)]).

def data(pwp_attrs,
	 pwp_attrs(Ask, Use, How, Att, Tag))
    ++ask=Ask
    ++use=Use
    ++how=How
    ++att=Att
    ++tag=Tag
    ++options([prefix(pwp_attrs)]).



%%  pwp_compile_file(:File, +Options)
%
%   Compile File and assert into calling module
%
:- meta_predicate pwp_compile_file(:).
:- meta_predicate pwp_compile_file(:, +).
pwp_compile_file(M:In) :- pwp_compile_file(M:In, []).
pwp_compile_file(M:In, Options) :-
    debug(streampwp, 'Compiling ~q', [M:In]),
    fix_html_extension(In, Base, File),
    load_xml_file(File, Contents),
    Co={co_module=M, co_curpred=Base,
        co_derived=false, co_binding=['CONTEXT'=Ctx],
        co_context=Ctx},

    pwp_compile_list(Contents, Co, Code, true),
    make_predicate(Co, Code, Stmt),
    !,
    process_options(Options, M, Stmt).

process_options(Options, Mod, Stmt) :-
    memberchk(export, Options),
    functor(Stmt, F,A),
    Mod:export(F/A).


%%  pwp_compile_files(+Directory)
%
%   Compile all the html files in this directory.
%
%   Change working directory while running so that
%   we can invoke pwp_compile_file without specifying
%   the whole path.
%
:- meta_predicate pwp_compile_files(:).
:- meta_predicate pwp_compile_files(:, +).
pwp_compile_files(Mod:Spec) :- pwp_compile_files(Mod:Spec, []).
pwp_compile_files(Mod:Spec, Options) :-
    (	Spec=relative(RelDir)
    ->	module_property(Mod, file(ModFile)),
	file_directory_name(ModFile, ModuleDirectory),
	atomic_list_concat([ModuleDirectory, RelDir], '/', Directory)
    ;	Directory=Spec
    ),


    directory_files(Directory, AllFiles),

    setup_call_cleanup(
	working_directory(OldWD, Directory),
	forall(member(File, AllFiles),
	       ignore((
		      atom_concat(_, html, File),
		      pwp_compile_file(Mod:File, Options)
		      ))),
	working_directory(_, OldWD) %reset
		      ).






%%  fix_html_extension(?In, ?Stripped, ?FileName).
%
%   /a/abc.html -> abc, /a/abc.html
%   ./abc       -> abc, ./abc.html
%
fix_html_extension(In, Stripped, FileName) :-
    file_name_extension(In, html, FileName),
    file_base_name(In, BaseFileName),
    (   In=FileName
    ->  atom_concat(Stripped, '.html', BaseFileName)
    ;   Stripped=BaseFileName
    ).


%%	pwp_compile_list(+Kids, +Coin, ?Code, -Tail).
%
%	Go through each kid and emit code according to Coin
%	If the item is not an element, just emit it.
%
pwp_compile_list([], _Co, Code, Code).
pwp_compile_list( [ element(Tag0,Atts0,Kids0)
                  | Xs ]
                , {Coin is {compilation_obj, module=M}}
                , Code, Tail
                ) :-
    pwp_attributes(Atts0, {PWP is {pwp_attrs}}, Atts1),
    nonvar({PWP-ask}), {PWP-ask} \== '', {PWP-ask} \== 'true',
    !,

    (   nonvar({PWP-tag}),
	{PWP-tag} \== ''
    ->  Tag = {PWP-tag}
    ;   Tag = Tag0
    ),


    atom_to_term({PWP-ask}, Query, BindingQ),
    unite_bindings({Coin-binding}, BindingQ, Binding1),

    ElementCo={Coin+{co_binding=Binding1, co_derived=true}},
    pwp_compile_element( Tag, Atts1, Kids0, PWP
               , ElementCo, ElementCode, true),
    make_predicate(ElementCo, ElementCode, CallThis),

    Code=(forall(M:Query, M:CallThis), Code0), %emit the looping dcg code
    pwp_compile_list(Xs, Coin, Code0, Tail).

pwp_compile_list( [ element(Tag0,Atts0,Kids0)
                  | Xs ]
                , {Coin is {compilation_obj}}
                , Code, Tail
                ) :-
	!,
	pwp_attributes(Atts0, {PWP is {pwp_attrs}}, Atts1),
	(   nonvar({PWP-tag}),
	    {PWP-tag} \== ''
	->  Tag={PWP-tag}
	;   Tag=Tag0
	),
	pwp_compile_element(Tag, Atts1, Kids0, PWP, Coin, Code, Code0),
	pwp_compile_list(Xs, Coin, Code0, Tail).
pwp_compile_list( [X|Xs], Coin, Code, Tail) :-
    Code=(write(X), Code0),
    pwp_compile_list(Xs, Coin, Code0, Tail).




pwp_attributes([], _, []).
pwp_attributes([AV|AVs], PWPAttrs, New_Atts1) :-
	AV = (Name=Value),
	(   strip_pwp_attr(Name, PWPName)
	->  (   pwp_attr(PWPName, Value, PWPAttrs)
	    ->	New_Atts1 = New_Atts2
	    ;	New_Atts1 = New_Atts2
	    )
	;   New_Atts1 = [AV|New_Atts2]
	),
	pwp_attributes(AVs, PWPAttrs, New_Atts2).


pwp_attr(ask, Value, {pwp_attrs, ask=Value}).
pwp_attr(use, Value, {pwp_attrs, use=Value}).
pwp_attr(how, Value, {pwp_attrs, how=Value}).
pwp_attr(att, Value, {pwp_attrs, att=Value}).
pwp_attr(tag, Value, {pwp_attrs, tag=Value}).

strip_pwp_attr(Atom, PWP) :-
	atom(Atom),
	atom_concat('pwp:', PWP, Atom), !.
strip_pwp_attr('http://www.cs.otago.ac.nz/staffpriv/ok/pwp.pl':PWP, PWP) :- !.
strip_pwp_attr('pwp':PWP, PWP) :- !.
strip_pwp_attr('xmlns:pwp', -).


%%	pwp_compile_element(+Tag, +Attrs, +Kids
%                           +PWP, +Coin, ?Code, -CodeTail).
%
%	Tag to use
%	Atts with pwp removed
pwp_compile_element('-', _Atts1, Kids, PWP, Coin, Code, CodeTail) :-
	%ignore attributes
	pwp_compile_use(Kids, PWP, Coin, Code, CodeTail).
pwp_compile_element(Tag, Atts, Kids, PWP, Coin, Code, CodeTail) :-
	atom_concat('<', Tag, TagStart),
	Code=(write(TagStart), Code0),
	emit_attrs(Atts, PWP, Coin, Code0, Code1),
	Code1=(write('>'), Code2),
	pwp_compile_use(Kids, PWP, Coin, Code2, Code3),
	format(atom(TagEnd), '</~w>', [Tag]),
	Code3=(write(TagEnd), CodeTail).

%%	emit_attrs(+KeyVals, +PWP, +Coin, ?Code, -Tail).
%
%	Emit code that writes all key values encountered.
emit_attrs([], _PWP, _Coin, Code, Code).
emit_attrs([K=V|Rest], {PWP is {pwp_attrs}}, Coin, Code, Tail) :-
	var({PWP-att}),
	!,
	format(atom(Attr), ' ~w="~w"', [K,V]),
	Code=(write(Attr), Code0),
	emit_attrs(Rest, PWP, Coin, Code0, Tail).
emit_attrs([K=V|Rest], {PWP is {pwp_attrs}}, Coin, Code, Tail) :-
	format(atom(KeyEq), ' ~w="', [K]),
	Code=(write(KeyEq), Code0),
	write_attr_substitute(V, PWP, Coin, Code0, Code1),
	Code1=(write('"'), Code2),
	emit_attrs(Rest, PWP, Coin, Code2, Tail).


%%	write_attr_substitute(+Value, +PWP, +Coin, ?Body, -Tail).
%
%   split input Value on Magic.  then evaluate every other item and
%   emit code that concats this list
%
write_attr_substitute(Value, {PWP is {pwp_attrs}},
		      {Coin is {compilation_obj}}, Body, Tail) :-
    sub_atom(Value, 0, _, _, {PWP-att}), %value starts with magic chars
    !,
    atomic_list_concat(L, {PWP-att}, Value),
    eval_attr_yes(L, {Coin-binding}, Body, Tail).

write_attr_substitute(Value, {PWP is {pwp_attrs}},
		      {Coin is {compilation_obj}}, Body, Tail) :-
    atomic_list_concat(L, {PWP-att}, Value),
    eval_attr_no(L, {Coin-binding}, Body, Tail).


%%  eval_evens(+List, +Bindings, -EvaluatedList).
%
%   Evaluate every second element in list
%
eval_attr_yes([], _, Code, Code).
eval_attr_yes([X|Rest], ContextIn, Code, Tail) :-
	atom_to_term(X, Xeval, Context0),
	unite_bindings(ContextIn, Context0, Context1),
	Code=(xml_quote_attribute(Xeval, XevalQ), %must escape quotes
	      write(XevalQ),
	      Code0),
	eval_attr_no(Rest, Context1, Code0, Tail).
eval_attr_no([], _, Code, Code).
eval_attr_no([Part|Rest], Context, Code, Tail) :-
	Code=(write(Part), Code0),
	eval_attr_yes(Rest, Context, Code0, Tail).




%%pwp_compile_use(+Kids, +PWP, +Coin, ?Code, -Tail),
pwp_compile_use(Kids, {pwp_attrs, use=''}, Coin, Code, Tail) :-
	!, %no use, just process kids
	pwp_compile_list(Kids, Coin, Code, Tail).
pwp_compile_use( _, {PWP is {pwp_attrs}},
		{Coin is {compilation_obj}}, Code, Tail) :-

    atom_to_term({PWP-use}, Term, BindingsT),
    unite_bindings({Coin-binding}, BindingsT, _),
    pwp_compile_how({PWP-how}, Term, Code, Tail).


%%	pwp_compile_how(+How, +Term, ?Code, -Tail).
%
%	Emit code based on How
pwp_compile_how(escaped, Term, Code, Tail) :-
	!,%assume escaped if it's not set already
	Code=(xml_quote_cdata(Term, TermQ), %escape <>&
	      write(TermQ),
	      Tail).
pwp_compile_how(text, Term, Code, Tail) :-
	Code=(write(Term), Tail).
pwp_compile_how(call, Term, Code, Tail) :-
	Code=(Term, Tail).
pwp_compile_how(How, Term, Code, Tail) :-
	Code=(call(How,Term), Code, Tail).







%%	make_predicate(+CompilationObj, +Code, -StatementHead)
%
%	compile the current code into a new predicate and return
%	StatementHead as instructions for calling this new
%	code
make_predicate({Co is {compilation_obj}}, Code, StmtHead) :-
    (   {Co-derived}=true
    ->  gensym({Co-curpred}, CP),
        binding_to_arg({Co-binding}, BindingTerm)
    ;   {Co-curpred}=CP,
        BindingTerm={Co-context}
    ),
    StmtHead=..[CP, BindingTerm],

    simplify_code(Code, SimpleCode),
    expand_term(({Co-module}:StmtHead :- SimpleCode), NewCode),

    (	tracing
    ->	writeln(NewCode),
	nl
    ;	compile_aux_clauses(NewCode)
    ).

combine_writes((write(Head)), [Head], true) :-
	ground(Head).
combine_writes((write(Head), true), [Head], true) :-
	ground(Head), !.
combine_writes((write(Head), CodeRest), [Head|Rest], CodeTail) :-
	ground(Head), !,
	combine_writes(CodeRest, Rest, CodeTail).
combine_writes(CodeTail, [], CodeTail).


simplify_code(true, true).
simplify_code((Cde, true), Cde).
simplify_code(CodeIn, CodeOut) :-
	combine_writes(CodeIn, WriteList, CodeIn0),
	WriteList \= [],
	!,
	atomic_list_concat(WriteList, NewTerm),
	(   CodeIn0=true
	->  CodeOut=write(NewTerm)
	;   CodeOut=(write(NewTerm), CodeOut0),
	    simplify_code(CodeIn0, CodeOut0)
	).
simplify_code((Term, CodeIn), CodeOut) :-
	CodeOut=(Term, CodeOut0),
	simplify_code(CodeIn, CodeOut0).



get_kv(A=B, A, B).
zip_kv(List, Keys, Values):-
    append(List,[],List), %seal the list for now!
    maplist(get_kv, List, Keys, Values).

%%  binding_to_arg(+ContextList, -BindingTerm)
%
%   context is a list of K=V, convert to a single term
%   in the format bind(V0, V1, ... VN).
%
binding_to_arg(ContextList, BindingTerm) :-
    zip_kv(ContextList, _, Vals),
    BindingTerm=..[bind | Vals].



%%  unite_bindings(?Bindings, ?Context, -Context).
%
%   For every member in binding, search for it in context
%   allowing binding of vars.  merge list into out Context
%
unite_bindings(Bindings, Context0, Context) :-
	unite_bindings(Bindings, Context0, Context0, Context).
unite_bindings([], _, Context, Context).
unite_bindings([Binding|Bindings], Context0, Context1, Context) :-
	memberchk(Binding, Context0), !,
	unite_bindings(Bindings, Context0, Context1, Context).
unite_bindings([Binding|Bindings], Context0, Context1, Context) :-
	unite_bindings(Bindings, Context0, [Binding|Context1], Context).











