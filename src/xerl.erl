%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(xerl).

-export([compile/1]).

-record(env, {
	filename,
	modules = []
}).

compile(Filename) ->
	io:format("Compiling ~s...~n", [Filename]),
	{ok, Src} = file:read_file(Filename),
	{ok, Tokens, _} = xerl_lexer:string(binary_to_list(Src)),
	{ok, Exprs} = xerl_parser:parse(Tokens),
	execute(Exprs, #env{filename=Filename}).

execute([], #env{modules=Modules}) ->
	io:format("Done...~n"),
	{ok, lists:reverse(Modules)};
execute([Expr = {mod, _, {atom, _, Name}, []}|Tail],
		Env=#env{filename=Filename, modules=Modules}) ->
	{ok, [Core]} = xerl_codegen:exprs([Expr]),
	{ok, [{Name, []}]} = core_lint:module(Core),
	io:format("~s~n", [core_pp:format(Core)]),
	{ok, _, Beam} = compile:forms(Core,
		[binary, from_core, return_errors, {source, Filename}]),
	{module, Name} = code:load_binary(Name, Filename, Beam),
	execute(Tail, Env#env{modules=[Name|Modules]});
execute([{integer, _, Int}|Tail], Env) ->
	io:format("integer ~p~n", [Int]),
	execute(Tail, Env).
