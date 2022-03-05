-ifndef(GAIA_COMMANDS_HRL).
-define(GAIA_COMMANDS_HRL, true).

-record(command,
        {
         name :: atom(),
         patterns :: [[string() | atom()]] | [[[string() | atom()]]],
         onsuccess ::
           fun((map()) -> [{cd, '..' | '.' | [atom()]} |
                           {set_timeout, integer(), fun((map()) -> map())} |
                           remove_timeout |
                           {dict, map()}]),
         children = [] :: [#command{}]
        }).

-endif.
