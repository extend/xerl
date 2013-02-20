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

-module(xerl_codegen).

-export([exprs/1]).
-export([intermediate_module/2]).

exprs(Exprs) ->
	exprs(Exprs, []).
exprs([], Acc) ->
	{ok, lists:reverse(Acc)};
exprs([Expr|Tail], Acc) ->
	exprs(Tail, [expr(Expr)|Acc]).

intermediate_module(AtomName, Exprs) ->
	Name = cerl:c_atom(AtomName),
	{ok, [Exprs2]} = exprs(Exprs),
	RunName = cerl:c_fname(run, 0),
	Run = {RunName, cerl:c_fun([], Exprs2)},
	{ok, cerl:c_module(Name, [RunName], [Run])}.

%% Create the module Name with the functions module_info/{0,1}.
expr({mod, _L, {atom, _, AtomName}, []}) ->
	Name = cerl:c_atom(AtomName),
	cerl:c_module(
		Name,
		[cerl:c_fname(module_info, 0), cerl:c_fname(module_info, 1)],
		mod_info(Name));
expr({integer, _L, Int}) ->
	cerl:c_int(Int).

mod_info(Name) ->
	M = cerl:c_atom(erlang),
	F = cerl:c_atom(get_module_info),
	Info0 = {cerl:c_fname(module_info, 0),
		cerl:c_fun([], cerl:c_call(M, F, [Name]))},
	Key = cerl:c_var('Key'),
	Info1 = {cerl:c_fname(module_info, 1),
		cerl:c_fun([Key], cerl:c_call(M, F, [Name, Key]))},
	[Info0, Info1].
