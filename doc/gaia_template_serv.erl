-module(gaia_audio_source_serv).
-export([start_link/0, stop/0]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").

%%
%% Exported: start_link
%%

start_link() ->
    ?spawn_server(fun(Parent) ->
                          init(Parent)
                  end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent) ->
    ?LOG_INFO("Gaia audio source server has been started"),
    {ok, #{parent => Parent}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent}) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.
