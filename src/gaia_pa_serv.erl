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
            %?LOG_DEBUG(#{skip_data => Data}),
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
            set_card_profile(Card);
        _ ->
            switch(N, Rest)
    end.

set_card_profile(Card) ->
    set_card_profile("/usr/bin/pactl set-card-profile " ++ Card,
                     ["headset_head_unit", "handsfree_head_unit"]).

set_card_profile(_Command, []) ->
    ok;
set_card_profile(Command, [Profile|Rest]) ->
    FinalCommand = Command ++ " " ++ Profile ++ " 2>&1",
    case os:cmd(FinalCommand) of
        "" ->
            ?LOG_INFO(#{command_success => FinalCommand}),
            ok;
        Failure ->
            ?LOG_INFO(#{command_failure => FinalCommand, reason => Failure}),
            set_card_profile(Command, Rest)
    end.
