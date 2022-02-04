-module(gaia_command_serv).
-export([start_link/1, stop/0]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").

%%
%% Exported: start_link
%%

-spec start_link(string()) ->
          serv:spawn_server_result().

start_link(CapturePcmName) ->
    ?spawn_server(fun(Parent) -> init(Parent, CapturePcmName) end,
                  fun initial_message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent, _CapturePcmName) ->
    ?LOG_INFO("Gaia command server has been started"),
    {ok, #{parent => Parent}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent}) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.
