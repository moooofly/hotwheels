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

-module(bin).

-export([split/2]).

%%
%% Sep -> "\\000" | "\\001"
%%
split(Sep, Bin) 
  when is_list(Sep),
       is_binary(Bin) ->
    %% 通过正则表达式（RE）来找到截取标记，然后把数据（Subject）截取分开
    %% split(Subject,RE,Options) -> SplitList
    case re:split(Bin, Sep, [{return, binary}, {parts, 2}]) of
        [Bin1, Bin2] when size(Bin1) < size(Bin) andalso size(Bin2) < size(Bin) ->
            
            {ok, Bin1, Bin2};
        [Bin1] ->	%% 此时 Bin 中尚未包含 Sep
            {more, Bin1}
    end.
