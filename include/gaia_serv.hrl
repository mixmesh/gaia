-ifndef(GAIA_SERV_HRL).
-define(GAIA_SERV_HRL, true).

-record(gaia_peer,
        {
         name :: gaia_serv:peer_name(),
         id :: gaia_serv:peer_id(),
         talks_to = false :: boolean(),
         modes = [] :: [gaia_serv:mode()],
         session_key = undefined :: gaia_serv:session_key() | undefined,
         nodis_address = undefined ::
           {inet:ip_address(), inet:port_number()} | undefined,
         gaia_port = undefined :: inet:port_number() | undefined
        }).

-record(gaia_group,
        {
         name :: gaia_serv:group_name(),
         id :: gaia_serv:group_id(),
         talks_to = false :: boolean(),
         modes = [] :: [gaia_serv:mode()],
         session_key = undefined :: gaia_serv:session_key() | undefined,
         admin :: gaia_serv:peer_id(),
         members :: [{gaia_serv:peer_id(), gaia_serv:peer_name()}]
        }).

-endif.
