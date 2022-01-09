-module(gaia_serv).
-export([start_link/5, stop/0]).
-export([get_status/0, set_status/1,
         all_peers/0, all_groups/0,
         currently_talking_to/0, start_talking_to/1, stop_talking_to/1,
         get_by_id/1, get_by_name/1,
         generate_artificial_id/1]).
-export([message_handler/1]).
-export_type([id/0, peer_id/0, group_id/0, name/0, peer_name/0, group_name/0,
              mode/0, session_key/0, status/0]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/gaia_serv.hrl").
-include("globals.hrl").

-type id() :: binary(). %% 64 bits key id ala PGP
-type peer_id() :: id().
-type group_id() :: id().
-type name() :: binary().
-type peer_name() :: name().
-type group_name() :: name().
-type mode() :: direct | override_if_busy | ask | ignore | mute | cleartext.
-type session_key() :: binary().
-type status() :: available | busy.

%%
%% Exported: start_link
%%

-spec start_link(binary(), peer_name(), peer_id(), inet:port_number(),
                 string()) ->
          serv:spawn_server_result().

start_link(GaiaDir, PeerName, PeerId, GaiaPort, PlaybackPcmName) ->
    ?spawn_server(fun(Parent) ->
                          init(Parent, GaiaDir, PeerName, PeerId, GaiaPort,
                               PlaybackPcmName)
                  end,
                  fun initial_message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: get_status
%%

-spec get_status() -> status().

get_status() ->
    serv:call(?MODULE, get_status).

%%
%% Exported: set_status
%%

-spec set_status(status()) -> ok.

set_status(Status) ->
    serv:call(?MODULE, {set_status, Status}).

%%
%% Exported: all_peers
%%

-spec all_peers() -> [#gaia_peer{}].

all_peers() ->
    serv:call(?MODULE, all_peers).

%%
%% Exported: all_groups
%%

-spec all_groups() -> [#gaia_group{}].

all_groups() ->
    serv:call(?MODULE, all_groups).

%%
%% Exported: currently_talking_to
%%

-spec currently_talking_to() -> [#gaia_peer{}|#gaia_group{}].

currently_talking_to() ->
    serv:call(?MODULE, currently_talking_to).

%%
%% Exported: start_talking_to
%%

-spec start_talking_to(id() | {name, name()}) ->
          ok | {error, no_such_id | no_such_name | already_talking_to}.

start_talking_to(IdOrName) ->
    serv:call(?MODULE, {start_talking_to, IdOrName}).

%%
%% Exported: stop_talking_to
%%

-spec stop_talking_to(id() | {name, name()} | all) ->
          ok | {error, no_such_id | no_such_name | not_talking_to}.

stop_talking_to(IdOrName) ->
    serv:call(?MODULE, {stop_talking_to, IdOrName}).

%%
%% Exported: get_by_id
%%

-spec get_by_id(id()) -> [#gaia_peer{}|#gaia_group{}].

get_by_id(Id) ->
    serv:call(?MODULE, {get_by_id, Id}).

%%
%% Exported: get_by_name
%%

-spec get_by_name(name()) -> [#gaia_peer{}|#gaia_group{}].

get_by_name(Name) ->
    serv:call(?MODULE, {get_by_name, Name}).

%%
%% generate_artificial_id
%%

-spec generate_artificial_id(peer_name()) -> peer_id().

generate_artificial_id(PeerName) when size(PeerName) < 5 ->
    binary:decode_unsigned(PeerName);
generate_artificial_id(PeerName) ->
    binary:decode_unsigned(binary:part(PeerName, {byte_size(PeerName), -4})).

%%
%% Server
%%

init(Parent, GaiaDir, PeerName, PeerId, GaiaPort, PlaybackPcmName) ->
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start({#{port => GaiaPort},
                         #{pcm_name => PlaybackPcmName,
                           playback_audio => ?PLAYBACK_AUDIO}}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    ok = nodis:set_node_info(#{gaia => #{id => PeerId, port => GaiaPort}}),
    ok = config_serv:subscribe(),
    {ok, NodisSubscription} = nodis_serv:subscribe(),
    ?LOG_INFO("Gaia server has been started"),
    {ok, #{parent => Parent,
           peer_name => PeerName,
           peer_id => PeerId,
           status => available,
           db => new_db(GaiaDir),
           nodis_subscription => NodisSubscription,
           gaia_addresses => #{}}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [NetworkSenderPid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_network_sender_serv], NeighbourWorkers),
            %% DEBUG
            %%ok = gaia_network_sender_serv:set_dest_addresses(
            %%       NetworkSenderPid, [{?DEFAULT_ADDR, ?DEFAULT_PORT}]),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{network_sender_pid => NetworkSenderPid}}
    end.

message_handler(#{parent := Parent,
                  db := Db,
                  status := Status,
                  network_sender_pid := NetworkSenderPid,
                  nodis_subscription := NodisSubscription,
                  gaia_addresses := GaiaAddresses} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {call, From, get_status} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => get_status}),
            {reply, From, Status};
        {call, From, {set_status, NewStatus}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {set_status, Status}}),
            true = update_db(Db, NewStatus),
            ok = set_addresses(Db, NewStatus, NetworkSenderPid ,GaiaAddresses),
            {reply, From, ok, State#{status => NewStatus}};
        {call, From, {get_by_id, Id}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {get_by_id, Id}}),
            {reply, From, db_get_by_id(Db, Id)};
        {call, From, {get_by_name, Name}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => {get_by_name, Name}}),
            {reply, From, db_get_by_name(Db, Name)};
        {call, From, all_peers} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => all_peers}),
            {reply, From, db_all_peers(Db)};
        {call, From, all_groups} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => all_groups}),
            {reply, From, db_all_groups(Db)};
        {call, From, currently_talking_to} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => currently_talking_to}),
            PeersAndGroups =
                db_foldl(
                  fun(#gaia_peer{talks_to = true,
                                 nodis_address = {_IpAddress, _SyncPort}} = Peer, Acc) ->
                          [Peer|Acc];
                     (#gaia_group{talks_to = true} = Group, Acc) ->
                          [Group|Acc];
                     (_, Acc) ->
                          Acc
                  end, [], Db),
            {reply, From, PeersAndGroups};
        {call, From, {start_talking_to, IdOrName}} ->
            case db_get_by(Db, IdOrName) of
                [#gaia_peer{talks_to = true}] ->
                    {reply, From, {error, already_talking_to}};
                [#gaia_group{talks_to = true}] ->
                    {reply, From, {error, already_talking_to}};
                [Peer] when is_record(Peer, gaia_peer) ->

                    %% FIXME: update with session_key from rest server iff my_id > peer_id

                    true = db_insert(Db, Peer#gaia_peer{talks_to = true}),
                    ok = set_addresses(Db, Status, NetworkSenderPid ,GaiaAddresses),
                    {reply, From, ok};
                [Group] when is_record(Group, gaia_group) ->

                    %% FIXME: create group with the help of the rest server if admin /= me

                    true = db_insert(Db, Group#gaia_group{talks_to = true}),
                    ok = set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses),
                    {reply, From, ok};
                [] ->
                    case IdOrName of
                        {name, _} ->
                            {reply, From, {error, no_such_name}};
                        _ ->
                            {reply, From, {error, no_such_id}}
                    end
            end;
        {call, From, {stop_talking_to, all}} ->
            db_foldl(
              fun(#gaia_peer{talks_to = true} = Peer, true) ->
                      db_insert(Db, Peer#gaia_peer{talks_to = false});
                 (#gaia_group{talks_to = true} = Group, true) ->

                      %% FIXME: remove group if admin /= me

                      db_insert(Db, Group#gaia_group{talks_to = false});
                 (_, Acc) ->
                      Acc
              end, true, Db),
            ok = set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses),
            {reply, From, ok};
        {call, From, {stop_talking_to, IdOrName}} ->
            case db_get_by(Db, IdOrName) of
                [#gaia_peer{talks_to = false}] ->
                    {reply, From, {error, not_talking_to}};
                [#gaia_group{talks_to = false}] ->
                    {reply, From, {error, not_talking_to}};
                [Peer] when is_record(Peer, gaia_peer) ->

                    %% FIXME: remove session key??

                    true = db_insert(Db, Peer#gaia_peer{talks_to = false}),
                    ok = set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses),
                    {reply, From, ok};
                [Group] when is_record(Group, gaia_group) ->
                    %% FIXME: remove group if admin /= me
                    true = db_insert(Db, Group#gaia_group{talks_to = false}),
                    ok = set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses),
                    {reply, From, ok};
                [] ->
                    case IdOrName of
                        {name, _} ->
                            {reply, From, {error, no_such_name}};
                        _ ->
                            {reply, From, {error, no_such_id}}
                    end
            end;
        config_update ->
            ?LOG_DEBUG(#{module => ?MODULE, message => config_update}),
            true = sync_db(Db),
            ok = set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses),
            noreply;
        {nodis, NodisSubscription, {pending, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {pending, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {up, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {up, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {change, NodisAddress, Info}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         nodis => {change, NodisAddress, Info}}),
            case nodis_change(Db, Status, NodisAddress, Info) of
                {ok, GaiaAddress} ->
                    UpdatedGaiaAddresses =
                        GaiaAddresses#{NodisAddress => GaiaAddress},
                    ok = set_addresses(Db, Status, NetworkSenderPid,
                                       UpdatedGaiaAddresses),
                    {noreply, State#{gaia_addresses => UpdatedGaiaAddresses}};
                {error, Reason} ->
                    ?LOG_ERROR(#{module => ?MODULE, update_peer => Reason}),
                    noreply
            end;
        {nodis, NodisSubscription, {wait, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {wait, NodisAddress}}),
            noreply;
        {nodis, NodisSubscription, {down, NodisAddress}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {down, NodisAddress}}),
            UpdatedGaiaAddresses = maps:remove(NodisAddress, GaiaAddresses),
            case db_get_peer_by_nodis_address(Db, NodisAddress) of
                [] ->
                    {noreply, State#{gaia_addresses => UpdatedGaiaAddresses}};
                [Peer] ->
                    true = db_insert(Db, Peer#gaia_peer{
                                           nodis_address = undefined,
                                           gaia_port = undefined}),
                    ok = set_addresses(Db, Status, NetworkSenderPid, UpdatedGaiaAddresses),
                    {noreply, State#{gaia_addresses => UpdatedGaiaAddresses}}
            end;
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

set_addresses(Db, Status, NetworkSenderPid, GaiaAddresses) ->
    ok = set_dest_addresses(Db,Status,  NetworkSenderPid, GaiaAddresses),
    set_src_addresses(Db, Status, NetworkSenderPid, GaiaAddresses).

set_dest_addresses(Db, Status, NetworkSenderPid, GaiaAddresses) ->
    AllDestAddresses =
        db_foldl(
          fun(#gaia_peer{name = <<"*">>, talks_to = true}, _Acc) ->
                  ?LOG_DEBUG(#{set_dest_addresses => wildcard}),
                  [wildcard];
             (#gaia_peer{name = PeerName,
                         talks_to = true,
                         modes = Modes,
                         nodis_address = {IpAddress, _SyncPort},
                         gaia_port = GaiaPort}, Acc) ->
                  ?LOG_DEBUG(#{set_dest_addresses => {PeerName, true}}),
                  case lists:member(mute, Modes) of
                      true ->
                          Acc;
                      false when Status == busy ->
                          case lists:member(override_if_busy, Modes) of
                              true ->
                                  [{IpAddress, GaiaPort}|Acc];
                              false ->
                                  Acc
                          end;
                      false when Status == available ->
                          [{IpAddress, GaiaPort}|Acc]
                  end;
             (#gaia_group{name = GroupName,
                          talks_to = true,
                          modes = Modes,
                          members = Members}, Acc) ->
                  ?LOG_DEBUG(#{set_dest_addresses => {GroupName, true}}),
                  case lists:member(mute, Modes) of
                      true ->
                          Acc;
                      false when Status == busy ->
                          case lists:member(override_if_busy, Modes) of
                              true ->
                                  member_peer_addresses(Db, Members) ++ Acc;
                              false ->
                                  Acc
                          end;
                      false when Status == available ->
                          member_peer_addresses(Db, Members) ++ Acc
                  end;
             (A, Acc) ->
                  ?LOG_DEBUG(#{set_dest_addresses => {ignore, A}}),
                  Acc
          end, [], Db),
    UniqueDestAddresses = lists:usort(AllDestAddresses),
    DestAddresses =
        case lists:member(wildcard, UniqueDestAddresses) of
            true ->
                maps:values(GaiaAddresses);
            false ->
                UniqueDestAddresses
        end,
    ?LOG_DEBUG(#{module => ?MODULE,
                 set_dest_addresses => DestAddresses}),
    gaia_network_sender_serv:set_dest_addresses(
      NetworkSenderPid, DestAddresses).

member_peer_addresses(_Db, []) ->
    [];
member_peer_addresses(_Db, [{_Id, <<"*">>}|_]) ->
    [wildcard];
member_peer_addresses(Db, [{Id, _Name}|Rest]) ->
    case db_get_peer_by_id(Db, Id) of
        [#gaia_peer{nodis_address = {IpAddress, _SyncPort},
                    gaia_port = GaiaPort}] ->
            [{IpAddress, GaiaPort}|member_peer_addresses(Db, Rest)];
        _ ->
            member_peer_addresses(Db, Rest)
    end.

set_src_addresses(Db, Status, _NetworkSenderPid, GaiaAddresses) ->
    AllSrcIpAddresses =
        db_foldl(
          fun(#gaia_peer{name = <<"*">>, talks_to = true}, _Acc) ->
                  [wildcard];
             (#gaia_peer{talks_to = true,
                         modes = Modes,
                         nodis_address = {IpAddress, _SyncPort}}, Acc) ->
                  case lists:member(ignore, Modes) of
                      true ->
                          Acc;
                      false when Status == busy ->
                          case lists:member(override_if_busy, Modes) of
                              true ->
                                  [IpAddress|Acc];
                              false ->
                                  Acc
                          end;
                      false when Status == available ->
                          [IpAddress|Acc]
                  end;
             (#gaia_group{talks_to = true,
                          modes = Modes,
                          members = Members}, Acc) ->
                  case lists:member(ignore, Modes) of
                      true ->
                          Acc;
                      false when Status == busy ->
                          case lists:member(override_if_busy, Modes) of
                              true ->
                                  member_peer_ip_addresses(Db, Members) ++ Acc;
                              false ->
                                  Acc
                          end;
                      false when Status == availabe ->
                          member_peer_ip_addresses(Db, Members) ++ Acc
                  end;
             (_, Acc) ->
                  Acc
          end, [], Db),
    UniqueSrcIpAddresses = lists:usort(AllSrcIpAddresses),
    SrcIpAddresses =
        case lists:member(wildcard, UniqueSrcIpAddresses) of
            true ->
                [IpAddress ||
                    {IpAddress, _GaiaPort} <- maps:values(GaiaAddresses)];
            false ->
                UniqueSrcIpAddresses
        end,
    ?LOG_DEBUG(#{module => ?MODULE,
                 set_src_ip_addresses => SrcIpAddresses}),
    gaia_nif:set_src_ip_addresses(SrcIpAddresses).

member_peer_ip_addresses(_Db, []) ->
    [];
member_peer_ip_addresses(_Db, [{_Id, <<"*">>}|_]) ->
    [wildcard];
member_peer_ip_addresses(Db, [{Id, _Name}|Rest]) ->
    case db_get_peer_by_id(Db, Id) of
        [#gaia_peer{nodis_address = {IpAddress, _SyncPort}}] ->
            [IpAddress|member_peer_ip_addresses(Db, Rest)];
        _ ->
            member_peer_ip_addresses(Db, Rest)
    end.

update_db(Db, Status) ->
    db_foldl(fun(Peer, true) ->
                     _ = update_peer(Db, Status, Peer),
                     true
             end, true, Db).

nodis_change(Db, Status, {IpAddress, _SyncPort} = NodisAddress, Info) ->
    MaxPeerId = math:pow(2, 32) - 1,
    case lists:keysearch(gaia, 1, Info) of
        {value, {gaia, #{id := NewPeerId, port := NewGaiaPort}, _PreviousGaiaInfo}}
          when is_integer(NewPeerId) andalso
               NewPeerId > 0 andalso NewPeerId =< MaxPeerId andalso
               is_integer(NewGaiaPort) andalso
               NewGaiaPort >= 1024 andalso NewGaiaPort < 65536 ->
            case db_get_peer_by_id(Db, NewPeerId) of
                [#gaia_peer{gaia_port = GaiaPort} = Peer]
                  when GaiaPort /= NewGaiaPort ->
                    UpdatedPeer = Peer#gaia_peer{nodis_address = NodisAddress,
                                                 gaia_port = NewGaiaPort},
                    true = db_insert(Db, UpdatedPeer),
                    _ = update_peer(Db, Status, UpdatedPeer),
                    {ok, {IpAddress, NewGaiaPort}};
                [Peer] ->
                    _ = update_peer(Db, Status, Peer),
                    {ok, {IpAddress, NewGaiaPort}};
                [] ->
                    {ok, {IpAddress, NewGaiaPort}}
            end;
        {value, {gaia, GaiaInfo, _PreviousGaiaInfo}} ->
            {error, {bad_gaia_info, GaiaInfo}};
        false ->
            {error, no_gaia_info}
    end.

update_peer(Db, Status, #gaia_peer{name = PeerName,
                                   talks_to = false,
                                   modes = Modes,
                                   nodis_address = NodisAddress} = Peer) ->
    case lists:member(direct, Modes) of
        true when Status == busy ->
            case lists:member(override_if_busy, Modes) of
                true ->
                    ?LOG_DEBUG(#{module => ?MODULE,
                                 direct_override_busy => PeerName}),
                    db_insert(Db, Peer#gaia_peer{talks_to = true});
                false ->
                    ?LOG_DEBUG(#{module => ?MODULE, direct_busy => PeerName})
            end;
        true when Status == available ->
            ?LOG_DEBUG(#{module => ?MODULE, direct_available => PeerName}),
            db_insert(Db, Peer#gaia_peer{talks_to = true});
        false when Status == busy ->
            ?LOG_DEBUG(#{module => ?MODULE, not_direct_busy => PeerName});
        false when Status == available andalso NodisAddress == undefined ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         not_direct_available_not_online => PeerName});
        false when Status == available ->
            case lists:member(ask, Modes) of
                true ->
                    ?LOG_DEBUG(#{module => ?MODULE,
                                 not_direct_available_ask => PeerName}),
                    gaia_command_serv:ask(PeerName);
                false ->
                    ?LOG_DEBUG(#{module => ?MODULE,
                                 not_direct_available_do_not_ask => PeerName})
            end
    end;
update_peer(_Db, _Status, _Peer) ->
    true.

%%
%% Peer and group database
%%

new_db(GaiaDir) ->
    FilePath = filename:join([GaiaDir, ?MODULE]),
    #gaia_peer.id = #gaia_group.id, %% This must me true!
    {ok, DetsTab} =
        dets:open_file(?MODULE, [{file, ?b2l(FilePath)},
                                 {keypos, #gaia_peer.id}]),
    Tab = ets:new(?MODULE, [public, {keypos, #gaia_peer.id}]),
    Tab = dets:to_ets(DetsTab, Tab),
    Db = {Tab, DetsTab},
    ok = sync_with_config(Db),
    ?LOG_DEBUG(#{module => ?MODULE, db_created => ets:tab2list(Tab)}),
    Db.

sync_db({Tab, DetsTab} = Db) ->
    ok = sync_with_config(Db),
    DetsTab = ets:to_dets(Tab, DetsTab),
    ?LOG_DEBUG(#{module => ?MODULE, db_synced_with_config => ets:tab2list(Tab)}),
    true.

sync_with_config(Db) ->
    PeerId = config:lookup([gaia, 'peer-id']),
    %% Update existing peers and groups
    {NewConfigPeers, NewConfigGroups} =
        db_foldl(
          fun(#gaia_peer{id = Id} = Peer, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigPeers) of
                      {value, ConfigPeer, RemainingConfigPeers} ->
                          [Modes] = config:lookup_children([modes], ConfigPeer),
                          UpdatedPeer = Peer#gaia_peer{modes = Modes},
                          true = db_insert(Db, UpdatedPeer),
                          {RemainingConfigPeers, ConfigGroups};
                      false ->
                          true = db_delete(Db, Id),
                          {ConfigPeers, ConfigGroups}
                  end;
             (#gaia_group{id = Id} = Group, {ConfigPeers, ConfigGroups}) ->
                  case lists:keytake(id, 1, ConfigGroups) of
                      {value, ConfigGroup, RemainingConfigGroups} ->
                          [Modes, Members] =
                              config:lookup_children([modes, members],
                                                     ConfigGroup),
                          UpdatedGroup =
                              Group#gaia_group{
                                modes = Modes,
                                admin = PeerId,
                                members = add_peer_id(Members)},
                          true = db_insert(Db, UpdatedGroup),
                          {ConfigPeers, RemainingConfigGroups};
                      false ->
                          true = db_delete(Db, Id),
                          {ConfigPeers, ConfigGroups}
                  end
          end, {config:lookup([gaia, peers]),
                config:lookup([gaia, groups])}, Db),
    %% Create new peers
    lists:foreach(
      fun(ConfigPeer) ->
              [Name, Id, Modes] =
                  config:lookup_children([name, id, modes], ConfigPeer),
              Peer = #gaia_peer{
                        name = Name,
                        id = generate_id(Id, Name),
                        modes = Modes},
              true = db_insert(Db, Peer)
      end, NewConfigPeers),
    %% Create new groups
    lists:foreach(
      fun(ConfigGroup) ->
              [Name, Id, Modes, Members] =
                  config:lookup_children([name, id, modes, members],
                                         ConfigGroup),
              Peer = #gaia_group{
                        name = Name,
                        id = generate_id(Id, Name),
                        modes = Modes,
                        admin = PeerId,
                        members = add_peer_id(Members)},
              true = db_insert(Db, Peer)
      end, NewConfigGroups).

add_peer_id([]) ->
    [];
add_peer_id([<<"*">>|Rest]) ->
    [{-1, <<"*">>}|add_peer_id(Rest)];
add_peer_id([PeerName|Rest]) ->
    case config:lookup([gaia, peers, {name, PeerName}]) of
        not_found ->
            PeerName = config:lookup([gaia, 'peer-name']), %% should be true!
            [{config:lookup([gaia, 'peer-id']), PeerName}|add_peer_id(Rest)];
        Peer ->
            [Id] = config:lookup_children([id], Peer),
            [{generate_id(Id, PeerName), PeerName}|add_peer_id(Rest)]
    end.

generate_id(-1, Name) ->
    generate_artificial_id(Name);
generate_id(Id, _Name) ->
    Id.

db_all_peers({Tab, _DetsTab}) ->
    ets:match_object(Tab, #gaia_peer{_ = '_'}).

db_all_groups({Tab, _DetsTab}) ->
    ets:match_object(Tab, #gaia_group{_ = '_'}).

db_insert({Tab, DetsTab}, PeerOrGroup) ->
    ok = dets:insert(DetsTab, PeerOrGroup),
    ets:insert(Tab, PeerOrGroup).

db_delete({Tab, DetsTab}, Id) ->
    ok = dets:delete(DetsTab, Id),
    ets:delete(Tab, Id).

db_get_by(Db, {name, Name}) ->
    db_get_by_name(Db, Name);
db_get_by(Db, Id) ->
    db_get_by_id(Db, Id).

db_get_by_id({Tab, _DetsTab}, Id) ->
    ets:lookup(Tab, Id).

db_get_by_name({Tab, _DetsTab}, Name) ->
    case ets:match_object(Tab, #gaia_peer{name = Name, _ = '_'}) of
	[] ->
            ets:match_object(Tab, #gaia_group{name = Name, _ = '_'});
        Peers ->
            Peers
    end.

db_get_peer_by_id({Tab, _DetsTab}, PeerId) ->
    ets:match_object(Tab, #gaia_peer{id = PeerId, _ = '_'}).

db_get_peer_by_nodis_address({Tab, _DetsTab}, NodisAddress) ->
    ets:match_object(Tab, #gaia_peer{nodis_address = NodisAddress, _ = '_'}).

db_foldl(Fun, Acc, {Tab, _DetsTab}) ->
    ets:foldl(Fun, Acc, Tab).

%db_dump({Tab, _DetsTab}) ->
%    ets:tab2list(Tab).
