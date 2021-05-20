%%%-------------------------------------------------------------------
%% @doc yshtola public API
%% @end
%%%-------------------------------------------------------------------

-module(yshtola_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Token = case os:getenv("DISCORD_TOKEN") of
                false -> throw({missing_envvar, "DISCORD_TOKEN"});
                V -> V
            end,
    Msgs = #{
      <<"roles">> => #{
          call => {yshtola, get_roles, []}
         },
      <<"set">> => #{
          call => {yshtola, set_role, []}
         },
      <<"unset">> => #{
          call => {yshtola, unset_role, []}
         }
     },
    discordant:connect(Token),
    discordant:set_routes(Msgs, []),
    yshtola_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
