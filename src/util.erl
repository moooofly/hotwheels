%%% Copyright (c) 2009 Oortle, Inc

%%% Permission is hereby granted, free of charge, to any person 
%%% obtaining a copy of this software and associated documentation 
%%% files (the "Software"), to deal in the Software without restriction, 
%%% including without limitation the rights to use, copy, modify, merge, 
%%% publish, distribute, sublicense, and/or sell copies of the Software, 
%%% and to permit persons to whom the Software is furnished to do so, 
%%% subject to the following conditions:

%%% The above copyright notice and this permission notice shall be included 
%%% in all copies or substantial portions of the Software.

%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
%%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
%%% DEALINGS IN THE SOFTWARE.

-module(util).

-export([is_process_alive/1,
         get_random_pid/1
        ]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

get_random_pid(Name) ->
    L = case pg2:get_members(Name) of
            {error, _} ->   %% 没有找到名为 Name 的进程组
                timer:sleep(100),
                %% [Warning] 这里若再次返回 {error, {no_such_group, Name}} 将会导致问题
                pg2:get_members(Name);
            Other when is_list(Other) ->
                Other
        end,
    if 
        L == [] ->  %% 名为 Name 的进程组中没有成员进程
            {error, {no_process, Name}};
        true -> %% [Warnging] 无法处理 {error, {no_such_group, Name}} 的情况
            %% 从成员进程中随机取一个
            {_,_,X} = erlang:now(),
            {ok, lists:nth((X rem length(L)) + 1, L)}
    end.

