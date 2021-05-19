-module(yshtola).

-export([get_roles/3]).

ff_roles(#{<<"name">> := <<"Tank">>}) -> true;
ff_roles(#{<<"name">> := <<"Healer">>}) -> true;
ff_roles(#{<<"name">> := <<"Melee DPS">>}) -> true;
ff_roles(#{<<"name">> := <<"Phys Ranged">>}) -> true;
ff_roles(#{<<"name">> := <<"Caster DPS">>}) -> true;
ff_roles(_) -> false.

get_roles(_Args, Api, Msg) ->
    #{<<"guild_id">> := GuildId} = Msg,
    Roles = discord_api:get_roles(Api, GuildId),
    FFRoles = lists:filter(fun ff_roles/1, Roles),
    Members = discord_api:get_guild_members(Api, GuildId),
    Fn = fun(#{<<"name">> := Name, <<"id">> := Id}) ->
        {Name, find_members(Id, Members)}
    end,
    RoleToMember = maps:from_list(lists:map(Fn, FFRoles)),
    {reply, build_roles_reply(RoleToMember), []}.

get_username(#{<<"user">> := #{<<"username">> := Username}}) -> Username.

has_role(RoleId, #{<<"roles">> := Roles}) ->
    lists:member(RoleId, Roles).

find_members(RoleId, Members) ->
    lists:map(fun get_username/1,
              lists:filter(fun(User) -> has_role(RoleId, User) end, Members)).

binary_join(L) ->
    lists:foldl(fun(X, A) -> <<A/binary, X/binary>> end, <<>>, L).

build_roles_reply(RoleToMember) ->
    R0 = build_role_reply(<<"Tank">>, RoleToMember),
    R1 = build_role_reply(<<"Healer">>, RoleToMember),
    R2 = build_role_reply(<<"Melee DPS">>, RoleToMember),
    R3 = build_role_reply(<<"Phys Ranged">>, RoleToMember),
    R4 = build_role_reply(<<"Caster DPS">>, RoleToMember),
    binary_join([R0, R1, R2, R3, R4]).

build_role_reply(Role, RoleToMember) ->
    case maps:get(Role, RoleToMember, undefined) of
        undefined -> <<>>;
        Members ->
            R0 = lists:map(fun(X) -> <<"  ", X/binary>> end, Members),
            R1 = binary_join(lists:join(<<"\n">>, [Role|R0])),
            <<R1/binary, "\n\n">>
    end.
