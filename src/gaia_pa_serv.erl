-module(gaia_pa_serv).
-export([start_link/0, stop/0, subscribe/0, unsubscribe/0, playcd/1]).
-export([message_handler/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("inpevt/include/inpevt.hrl").
-include_lib("dbus/include/dbus.hrl").

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
%% Exported: playcd
%%

-spec playcd(pid() | atom()) -> ok.

playcd(PidOrName) ->
    PidOrName ! {input_event, playcd},
    ok.

%%
%% Server
%%

init(Parent) ->
%%    Port = open_port({spawn, "/usr/bin/pactl subscribe"},
%%                     [stream, {line, 1024}, in]),
    UserId = string:strip(os:cmd("id --user"), right, $\n),
    UserDbusSocketPath =
        filename:join(["run", "user", UserId, "pulse", "dbus-socket"]),
    PulseAddress =
        case filelib:is_file(UserDbusSocketPath) of
            true ->
                dbus:pulse_address();
            false ->
                try
                    dbus:pulse_address()
                catch error:_ ->
                        %% We assume that pulseaudio run as a system daemon
                        "unix:path=/var/run/pulse/dbus-socket"
                end
        end,
    {ok,Connection} = dbus_connection:open(PulseAddress, external, false),
    Signals = ["org.PulseAudio.Core1.NewCard",
               "org.PulseAudio.Core1.CardRemoved" ],
    Fs = [{path, "/org/pulseaudio/core1"},{destination, "org.PulseAudio1"}],
    lists:foreach(
      fun(Sig) ->
	      %% filter objects (paths) may be given as list
	      {ok,_Ref} = dbus_pulse:listen_for_signal(Connection,Fs,Sig,[])
      end, Signals),

    %% Setup udev to look for input/power-switch devices
    Udev = udev:new(),
    Umon = udev:monitor_new_from_netlink(Udev, udev),
    SubSys = "input",
    DevType = null,


%    ok = udev:monitor_filter_add_match_subsystem_devtype(Umon,SubSys,DevType),
%    ok = udev:monitor_filter_add_match_tag(Umon,"power-switch"),


    ok = udev:monitor_enable_receiving(Umon),
    Uref = erlang:make_ref(),
    select = udev:select(Umon, Uref),
    %% Now enumerate and add all existing devices
    Enum = udev:enumerate_new(Udev),
    udev:enumerate_add_match_subsystem(Enum, "input"),
    udev:enumerate_add_match_tag(Enum, "power-switch"),


    %% fixme! support usb cards (think jabra eveolve dongle..)
    udev:enumerate_add_match_property(Enum, "ID_BUS", "bluetooth"),
    State0 = #{parent => Parent,
	       %% port => Port,
	       dbus => Connection,
	       %% udev => Udev,
	       udev_mon => Umon,
	       udev_ref => Uref,
	       devices => #{},
	       subscribers => []},
    State1 = add_existing_devices(Connection, Udev, Enum, State0),
    %% Devices = refresh_devices([]),
    ?LOG_INFO("Gaia pulseaudio server has been started"),
    {ok, State1}.

message_handler(#{parent := Parent,
                  %% port := Port,
		  dbus := Connection,
		  %% udev := Udev,
		  udev_mon := Umon,
		  udev_ref := Uref,
                  %% devices := Devices,
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
	{signal, _Ref, Header, Message} ->
	    Fds = Header#dbus_header.fields,
	    case {Fds#dbus_field.interface,Fds#dbus_field.member} of
		{"org.PulseAudio.Core1", "NewCard"} ->
		    [Card|_] = Message,
		    ?LOG_DEBUG(#{card=>Card}),
		    new_dbus_card(Connection, Card),
		    %% UpdatedDevices = refresh_devices(Devices),
		    {noreply, State};
		{"org.PulseAudio.Core1", "CardRemoved"} ->
		    [Card|_] = Message,
		    ?LOG_DEBUG(#{card => Card}),
		    %%timer:sleep(2000), % ehhh! try fnotify?
		    %% UpdatedDevices = refresh_devices(Devices),
		    {noreply, State};
		_ ->
		    {noreply, State}
	    end;
	{select, Umon, Uref, ready_input} ->
	    Recv = udev:monitor_receive_device(Umon),
	    %% reselect should be ok to reuse the Uref...
	    select = udev:select(Umon, Uref),
	    case Recv of
		undefined ->
		    {noreply, State};
		Dev ->
		    case udev:device_get_action(Dev) of
			"add" ->
			    {noreply, add_udev_card(Dev, State)};
			"remove" ->
			    {noreply,remove_udev_card(Dev, State)}
		    end
	    end;

%%        {Port, {data, {eol, "Event 'new' on card #" ++ N} = Data}} ->
%%            ?LOG_DEBUG(#{data => Data}),
%%            ok = maybe_set_card_profile(N),
%%            UpdatedDevices = refresh_devices(Devices),
%%            ?LOG_INFO(#{devices => UpdatedDevices}),
%%            {noreply, State#{devices => UpdatedDevices}};

%%        {Port, {data, {eol, "Event 'remove' on card #" ++ _} = Data}} ->
%%            ?LOG_DEBUG(#{data => Data}),
%%            UpdatedDevices = refresh_devices(Devices),
%%            ?LOG_INFO(#{devices => UpdatedDevices}),
%%            {noreply, State#{devices => UpdatedDevices}};
%%        {Port, {data, _Data}} ->
%%            %?LOG_DEBUG(#{skip_data => Data}),
%%            noreply;
        #input_event{code_sym = playcd, value = 0} = InputEvent ->
            ?LOG_DEBUG(#{event => InputEvent}),
            lists:foreach(fun({Pid, _MonitorRef}) ->
                                  playcd(Pid)
                          end, Subscribers),
            noreply;
        #input_event{} = InputEvent ->
            ?LOG_DEBUG(#{skipped_event => InputEvent}),
            noreply;
        {'DOWN', _Ref, process, Pid, Info} ->
            ?LOG_DEBUG(#{subscriber_down => Info}),
            UpdatedSubscribers = lists:keydelete(Pid, 1, Subscribers),
            {noreply, State#{subscribers => UpdatedSubscribers}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

%% call at start to add subscriptions to existing cards
add_existing_devices(Connection, Udev, Enum, State) ->
    {ok,Cards} = dbus_pulse:get_cards(Connection),
    lists:foreach(
      fun(Card) ->
              new_dbus_card(Connection, Card)
      end, Cards),
    lists:foldl(
      fun(Path, Si) ->
	      Dev = udev:device_new_from_syspath(Udev, Path),
	      Si2 = add_udev_card(Dev, Si),
              ?LOG_DEBUG(#{add_existing_devices => {Dev, Si, Si2}}),
              Si2
      end, State, udev:enumerate_get_devices(Enum)).

add_udev_card(Dev, State) ->
    Prop = udev:device_get_properties(Dev),
    DevNode = udev:device_get_devnode(Dev),
    NAME = proplists:get_value("NAME",Prop,undefined),
    if is_list(DevNode), NAME =:= undefined ->
	    %% check&match NAME in parent
	    Parent = udev:device_get_parent(Dev),
	    PProp = udev:device_get_properties(Parent),
	    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
	    ?LOG_DEBUG(#{add_udev_card => [{add, stripq(PNAME)}, {devnode, DevNode}]}),
	    case inpevt:add_device(#{device => DevNode}) of
		[] ->
                    ?LOG_DEBUG(#{add_udev_card => not_added}),
		    State;
		[Added] ->
                    ?LOG_DEBUG(#{add_udev_card => {added, Added}}),
                    inpevt:subscribe(Added),
                    Devices = maps:get(devices, State, #{}),
		    Devices1 = maps:put(DevNode, Added, Devices),
                    ?LOG_DEBUG(#{add_udev_card => {devices, Devices1}}),
		    State#{ devices => Devices1 }
	    end;
       true ->
	    State
    end.

remove_udev_card(Dev, State) ->
    Prop = udev:device_get_properties(Dev),
    DevNode = udev:device_get_devnode(Dev),
    NAME = proplists:get_value("NAME",Prop,undefined),
    if is_list(DevNode), NAME =:= undefined ->
	    case udev:device_get_parent(Dev) of
		false -> ok;
		Parent ->
		    PProp = udev:device_get_properties(Parent),
		    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
		    io:format("~s: PNAME=~p\n", ["remove",stripq(PNAME)]),
		    io:format("    devnode=~p\n", [DevNode])
	    end,
	    Devices = maps:get(devices, State, #{}),
	    case maps:take(DevNode, Devices) of
		error -> State;
		{D, Devices1} ->
		    inpevt:delete_device(D),
		    State#{ devices => Devices1 }
	    end;
       true ->
	    State
    end.

%%
%% Card profile handling
%%
new_dbus_card(Connection,Card) ->
    case dbus_pulse:get_card_profiles(Connection, Card) of
	{ok,Profiles} ->
	    lists:foreach(
	      fun(Profile) ->
		      case dbus_pulse:get_card_profile_name(Connection,Profile) of
			  {ok,Name="handsfree_head_unit"} ->
			      io:format("Set Active Profile: ~p\n", [Name]),
			      dbus_pulse:set_card_active_profile(Connection, Card, Profile);
			  {ok,Name="headset_head_unit"} ->
			      io:format("Set Active Profile: ~p\n", [Name]),
			      dbus_pulse:set_card_active_profile(Connection, Card, Profile);
			  {ok,Name} ->
			      io:format("Profile: ~p\n", [Name]);
			  _Error ->
			      ignore
		      end
	      end, Profiles);
	_Error ->
	    ignore
    end.

%% strip double quotes
stripq(Atom) when is_atom(Atom) -> Atom;
stripq(String) when is_list(String) ->
    case String of
	[$"|String0] ->
	    case lists:reverse(String0) of
		[$"|String1] -> lists:reverse(String1);
		_ -> String0
	    end;
	_ -> String
    end.

%% maybe_set_card_profile(N) ->
%%     Lines = os:cmd("/usr/bin/pactl list short cards"),
%%     maybe_set_card_profile(N, string:tokens(Lines, "\n")).

%% maybe_set_card_profile(_N, []) ->
%%     ok;
%% maybe_set_card_profile(N, [Line|Rest]) ->
%%     case string:tokens(Line, "\t") of
%%         [N, "bluez_card." ++ _ = Card, _] ->
%%             set_card_profile(Card);
%%         _ ->
%%             maybe_set_card_profile(N, Rest)
%%     end.

%% set_card_profile(Card) ->
%%     set_card_profile("/usr/bin/pactl set-card-profile " ++ Card,
%%                      ["headset_head_unit", "handsfree_head_unit"]).

%% set_card_profile(_Command, []) ->
%%     ok;
%% set_card_profile(Command, [Profile|Rest]) ->
%%     FinalCommand = Command ++ " " ++ Profile ++ " 2>&1",
%%     case os:cmd(FinalCommand) of
%%         "" ->
%%             ?LOG_INFO(#{command_success => FinalCommand}),
%%             ok;
%%         Failure ->
%%             ?LOG_INFO(#{command_failure => FinalCommand, reason => Failure}),
%%             set_card_profile(Command, Rest)
%%     end.

%%
%% Device handling
%%

%% refresh_devices(Devices) ->
%%     {_Added,Removed} = inpevt:diff_devices(Devices),
%%     inpevt:delete_devices(Removed),
%%     ?LOG_DEBUG(#{delete_devices => Removed}),
%%     Added1 = inpevt:add_matched_devices([{name, "OpenMove|Jabra"}]),
%%     ?LOG_DEBUG(#{add_devices => Added1}),
%%     inpevt:subscribe(Added1),
%%     inpevt:get_devices().

    %% {ok, UpdatedDevices} = inpevt:get_devices(),
    %% %% Unsubscribe
    %% lists:foreach(
    %%   fun({_Name, #{device := DeviceFilename, port := Port}}) ->
    %%           case device_exists(DeviceFilename, UpdatedDevices) of
    %%               true ->
    %%                   ok;
    %%               false ->
    %%                   ?LOG_DEBUG(#{unsubscribe => DeviceFilename}),
    %%                   ok = inpevt:unsubscribe(Port, self())
    %%           end
    %%   end, Devices),
    %% %% Subscribe
    %% lists:foreach(
    %%   fun({_Name, #{device := DeviceFilename, port := Port}}) ->
    %%           case device_exists(DeviceFilename, Devices) of
    %%               true ->
    %%                   ok;
    %%               false ->
    %%                   ?LOG_DEBUG(#{subscribe => DeviceFilename}),
    %%                   ok = inpevt:subscribe(Port, self())
    %%           end
    %%   end, UpdatedDevices),
    %% UpdatedDevices.

%% delete_devices(_AllDevices, []) ->
%%     ok;
%% delete_devices(AllDevices, [{_Name, #{device := DeviceFilename}}|Rest]) ->
%%     case device_exists(DeviceFilename, AllDevices) of
%%         true ->
%%             delete_devices(AllDevices, Rest);
%%         false ->
%%             ?LOG_DEBUG(#{delete_device => DeviceFilename}),
%%             ok = inpevt:delete_device(DeviceFilename),
%%             delete_devices(AllDevices, Rest)
%%     end.

%% device_exists(_DeviceFilename, []) ->
%%     false;
%% device_exists(DeviceFilename, [{_Name, #{device := DeviceFilename}}|_]) ->
%%     true;
%% device_exists(DeviceFilename, [_|Rest]) ->
%%     device_exists(DeviceFilename, Rest).
