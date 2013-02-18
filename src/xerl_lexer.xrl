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

Definitions.

D = [0-9]
L = [a-z]
U = [A-Z]
A = ({D}|{L}|{U}|_|@)
WS = (\s|\t|\r|\n)

Rules.

{L}{A}* :
	Atom = list_to_atom(TokenChars),
	{token, case reserved_word(Atom) of
		true -> {Atom, TokenLine};
		false -> {atom, TokenLine, Atom}
	end}.
{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{WS}+ : skip_token.

Erlang code.

-export([reserved_word/1]).

reserved_word('begin') -> true;
reserved_word('end') -> true;
reserved_word('mod') -> true;
reserved_word(_) -> false.
