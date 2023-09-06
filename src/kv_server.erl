-module(kv_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Name) ->
    Return = gen_server:start_link({local, Name}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    State = #{},
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    Return.

handle_call({get, {Key, Default}}, _From, State) ->
    {reply, {ok,maps:get(Key, State, Default)}, State};

handle_call(_Msg, _From, State) ->
    Return = {reply, {error, unknown_msg}, State},
    io:format("handle_call: ~p~n", [Return]),
    Return.
    
handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info({put, {Key, Value}}, State) ->
    {noreply, State#{Key => Value}};

handle_info(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.