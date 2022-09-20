-module(gaia_pa_serv).
-export([start_link/0, stop/0, subscribe/0, unsubscribe/0]).
-export([message_handler/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("inpevt/include/inpevt.hrl").

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
%% Exported: subscribe
%%

-spec subscribe() -> ok | {error, already_subscribed}.

subscribe() ->
    serv:call(?MODULE, {subscribe, self()}).

%%
%% Exported: unsubscribe
%%

-spec unsubscribe() -> ok | {error, not_subscribed}.

unsubscribe() ->
    serv:call(?MODULE, {unsubscribe, self()}).

%%
%% Server
%%

init(Parent) ->
    Port = open_port({spawn, "/usr/bin/pactl subscribe"},
                     [stream, {line, 1024}, in]),
    Devices = refresh_devices([]),
    ?LOG_INFO("Gaia pulseaudio server has been started"),
    {ok, #{parent => Parent,
           port => Port,
           devices => Devices,
           subscribers => []}}.

message_handler(#{parent := Parent,
                  port := Port,
                  devices := Devices,
                  subscribers := Subscribers} = State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {call, From, {subscribe, Pid} = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            case lists:keymember(Pid, 1, Subscribers) of
                true ->
                    {reply, From, {error, already_subscribed}};
                false ->
                    MonitorRef = monitor(process, Pid),
                    {reply, From, ok,
                     State#{subscribers => [{Pid, MonitorRef}|Subscribers]}}
            end;
        {call, From, {unsubscribe, Pid} = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            case lists:keytake(Pid, 1, Subscribers) of
                {value, {_, MonitorRef}, PurgedSubscribers} ->
                    true = demonitor(MonitorRef),
                    {reply, From, ok,
                     State#{subscribers => PurgedSubscribers}};
                false ->
                    {reply, From, {error, not_subsccribed}}
            end;
        {Port, {data, {eol, "Event 'new' on card #" ++ N} = Data}} ->
            ?LOG_DEBUG(#{data => Data}),
            ok = maybe_set_card_profile(N),
            UpdatedDevices = refresh_devices(Devices),
            ?LOG_INFO(#{devices => UpdatedDevices}),
            {noreply, State#{devices => UpdatedDevices}};
        {Port, {data, {eol, "Event 'remove' on card #" ++ _} = Data}} ->
            ?LOG_DEBUG(#{data => Data}),
            UpdatedDevices = refresh_devices(Devices),
            ?LOG_INFO(#{devices => UpdatedDevices}),
            {noreply, State#{devices => UpdatedDevices}};
        {Port, {data, _Data}} ->
            %?LOG_DEBUG(#{skip_data => Data}),
            noreply;
        #input_event{code_sym = playcd, value = 0} = InputEvent ->
            ?LOG_DEBUG(#{event => InputEvent}),
            lists:foreach(fun({Pid, _MonitorRef}) ->
                                  Pid ! {input_event, playcd}
                          end, Subscribers),
            noreply;
        #input_event{} ->
            noreply;
        {'DOWN', _Ref, process, Pid, Info} ->
            ?LOG_DEBUG(#{subscriber_down => Info}),
            UpdatedSubscribers = lists:keydelete(Pid, 1, Subscribers),
            {noreply, State#{subscribers => UpdatedSubscribers}};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

%%
%% Card profile handling
%%

maybe_set_card_profile(N) ->
    Lines = os:cmd("/usr/bin/pactl list short cards"),
    maybe_set_card_profile(N, string:tokens(Lines, "\n")).

maybe_set_card_profile(_N, []) ->
    ok;
maybe_set_card_profile(N, [Line|Rest]) ->
    case string:tokens(Line, "\t") of
        [N, "bluez_card." ++ _ = Card, _] ->
            set_card_profile(Card);
        _ ->
            maybe_set_card_profile(N, Rest)
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

%%
%% Device handling
%%

refresh_devices(Devices) ->
    timer:sleep(2000), % ehhh!
    ok = delete_devices(inpevt:list_devices(), Devices),
    _ = inpevt:add_matched_devices([{name, "OpenMove"}]),
    {ok, UpdatedDevices} = inpevt:get_devices(),
    %% Unsubscribe
    lists:foreach(
      fun({_Name, #{device := DeviceFilename, port := Port}}) ->
              case device_exists(DeviceFilename, UpdatedDevices) of
                  true ->
                      ok;
                  false ->
                      ?LOG_DEBUG(#{unsubscribe => DeviceFilename}),
                      inpevt:unsubscribe(Port, self())
              end
      end, Devices),
    %% Subscribe
    lists:foreach(
      fun({_Name, #{device := DeviceFilename, port := Port}}) ->
              case device_exists(DeviceFilename, Devices) of
                  true ->
                      ok;
                  false ->
                      ?LOG_DEBUG(#{subscribe => DeviceFilename}),
                      inpevt:subscribe(Port, self())
              end
      end, UpdatedDevices),
    UpdatedDevices.

delete_devices(_AllDevices, []) ->
    ok;
delete_devices(AllDevices, [{_Name, #{device := DeviceFilename}}|Rest]) ->
    case device_exists(DeviceFilename, AllDevices) of
        true ->
            delete_devices(AllDevices, Rest);
        false ->
            ?LOG_DEBUG(#{delete_device => DeviceFilename}),
            ok = inpevt:delete_device(DeviceFilename),
            delete_devices(AllDevices, Rest)
    end.

device_exists(_DeviceFilename, []) ->
    false;
device_exists(DeviceFilename, [{_Name, #{device := DeviceFilename}}|_]) ->
    true;
device_exists(DeviceFilename, [_|Rest]) ->
    device_exists(DeviceFilename, Rest).
