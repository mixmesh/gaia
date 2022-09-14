-module(gaia_pa_serv).
-export([start_link/0, stop/0]).
-export([message_handler/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/serv.hrl").

%%
%% Exported: start_link
%%

-spec start_link() ->
          serv:spawn_server_result().

start_link() ->
    ?spawn_server(
       fun init/1,
       fun message_handler/1,
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

init(Parent) ->
    Port = open_port({spawn, "/usr/bin/pactl subscribe"},
                     [stream, {line, 1024}, in]),
    ?LOG_INFO("Gaia pulseaudio server has been started"),
    {ok, #{parent => Parent, port => Port}}.

message_handler(#{parent := Parent, port := Port} = _State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {Port, {data, {eol, "Event 'new' on card #" ++ N} = Data}} ->
            ?LOG_DEBUG(#{data => Data}),
            Lines = os:cmd("/usr/bin/pactl list short cards"),
            ok = switch(N, string:tokens(Lines, "\n")),
            noreply;
        {Port, {data, Data}} ->
            ?LOG_DEBUG(#{skip_data => Data}),
            noreply;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

switch(_N, []) ->
    ok;
switch(N, [Line|Rest]) ->
    case string:tokens(Line, "\t") of
        [N, "bluez_card." ++ _ = Card, _] ->
            Result =
                os:cmd("/usr/bin/pactl set-card-profile " ++ Card ++
                           " headset_head_unit 2>&1"),
            ?LOG_INFO(#{switch_to => Card, result => Result}),
            ok;
        _ ->
            switch(N, Rest)
    end.
