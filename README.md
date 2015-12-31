# elixir.mk

A (very) experimental plugins for [erlang.mk](http://erlang.mk) to use [Elixir](http://elixir-lang.org/) modules in Erlang.

## Example

```
PROJECT = test
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEP_PLUGINS = elixir.mk
BUILD_DEPS = elixir.mk

dep_elixir.mk = git https://github.com/botsunit/elixir.mk.git master

ELIXIR_MIX_ENV = test
ELIXIR_DEPS = httpoison 

dep_httpoison = hex 0.8.0

include erlang.mk
```

You can also look in the [sample](sample) directory for a complete example :

```
git clone https://github.com/botsunit/elixir.mk.git
cd elixir.mk/sample
make
make dev

Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.2  (abort with ^G)
1> application:ensure_all_started(test).
{ok,[compiler,elixir,test]}
2> test:sample().
#{<<"people">> => [#{<<"age">> => 27,<<"name">> => <<"Devin Torres">>}]}
<<"{\"people\":[{\"name\":\"Devin Torres\",\"age\":27}]}">>
ok
3>

```

## Licence

Copyright (c) 2015, Bots Unit<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


