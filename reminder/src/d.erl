%%%-------------------------------------------------------------------
%%% @author dp <>
%%% @copyright (C) 2012, dp
%%% @doc
%%% 项目的tracer  ，用于运行时排错
%%% @end
%%% Created : 16 Oct 2012 by dp <>
%%%-------------------------------------------------------------------
-module(d).
%% API
-export([t/3,t/2,t/1,stop/0,p/1]).
 
%%%===================================================================
%%% API
%%%===================================================================
 
%%  指定要监控的模块，函数，函数的参数个数
t(Mod)->
    dbg:tpl(Mod,[{'_', [], [{return_trace}]}]).
 
 
%%  指定要监控的模块，函数
t(Mod,Fun)->
    dbg:tpl(Mod,Fun,[{'_', [], [{return_trace}]}]).
 
 
%%  指定要监控的模块，函数，函数的参数个数
t(Mod,Fun,Ari)->
    dbg:tpl(Mod,Fun,Ari,[{'_', [], [{return_trace}]}]).
 
 
 
%%开启tracer。Max是记录多少条数据
p(Max)->
    FuncStopTracer =
        fun
            (_, N) when N =:= Max-> % 记录累计到上限值，追踪器自动关闭
                dbg:stop_clear(),
                io:format("#WARNING >>>>>> dbg tracer stopped <<<<<<~n~n",[]);
            (Msg, N) ->
                case Msg of
                    {trace, _Pid, call, Trace} ->
                        {M, F, A} = Trace,
                           io:format("###################~n",[]),
                           io:format("call [~p:~p,(~p)]~n", [M, F, A]),
                           io:format("###################~n",[]);
                    {trace, _Pid, return_from, Trace, ReturnVal} ->
                        {M, F, A} = Trace,
                        io:format("===================~n",[]),
                        io:format("return [~p:~p(~p)] =====>~p~n", [M, F, A, ReturnVal]),
                        io:format("===================~n",[]);
                    _ -> skip
                end,
                N + 1
        end,
    case dbg:tracer(process, {FuncStopTracer, 0}) of
      {ok, _Pid} ->
        dbg:p(all, [all]);  
      {error, already_started} ->
        skip         
    end.
 
stop()->
     dbg:stop_clear().
 
%%%===================================================================
%%% Internal functions
%%%===================================================================