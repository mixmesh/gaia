-ifndef(GAIA_SERV_HRL).
-define(GAIA_SERV_HRL, true).

-record(gaia_peer,
        {
         id :: gaia_serv:peer_id(),
         name = undefined :: gaia_serv:peer_name() | undefined,
         mode = undefined :: gaia_serv:mode() | undefined,
         ephemeral = false :: boolean(),
         options = [] :: gaia_serv:options(),
         talks_to = false :: boolean(),
         nodis_address = undefined ::
           {inet:ip_address(), inet:port_number()} | undefined,
         rest_port = undefined :: inet:port_number() | undefined,
         local_port = undefined :: inet:port_number() | undefined,
         remote_port = undefined :: inet:port_number() | undefined,
         session_key = undefined :: gaia_serv:session_key() | undefined
        }).

-record(gaia_group,
        {
         id :: gaia_serv:group_id(),
         name :: gaia_serv:group_name(),
         mode :: gaia_serv:mode(),
         options :: gaia_serv:options(),
         port :: inet:port_number(),
         type :: gaia_serv:group_type(),
         members :: [{gaia_serv:peer_id(), gaia_serv:peer_name()}],
         admin :: gaia_serv:peer_id(),
         talks_to = false :: boolean(),
         session_key = undefined :: gaia_serv:session_key() | undefined
        }).

-endif.
