Modules
=======

Purpose
-------

Modules in Xerl have the same meaning as in Erlang.

Syntax
------

Modules are created with the `mod` expression. They have at
the very least a name and a body.

Currently the body must always be empty.

``` erlang
mod my_module
begin
end
```

It can also be written as follow.

``` erlang
mod my_module begin end
```

The keywords `begin` and `end` denote a block of expressions.
This is where you will later on put your function declarations
or any other expressions you need.
