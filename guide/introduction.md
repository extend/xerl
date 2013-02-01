Introduction
============

Purpose
-------

Xerl today is a learning exercise, an experiment, a toy.

It is also a tentative to improve the Erlang language while still
keeping all the good parts, giving more power more conveniently
to the programmer.

But unlike Erlang, in Xerl everything is an expression.

Getting started
---------------

You can compile a Xerl source file using the `xerl:compile/1`
function. Beware though! Xerl will not nicely tell you about
any error in your source code at this time.

``` erlang
xerl:compile("my_module.xerl").
```

Resources
---------

A series of articles is being written explaining each step
in the construction of the Xerl programming language.

 *  [Xerl: empty modules](http://ninenines.eu/articles/xerl-0.1-empty-modules)
