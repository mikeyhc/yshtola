-module(yshtola).

-export([get_roles/3, set_role/3, unset_role/3]).

%% public API

set_role(RoleNameParts, Api,
         #{<<"author">> := #{<<"id">> := UserId}, <<"guild_id">> := GuildId}) ->
    RoleName = binary_join(lists:join(<<" ">>, RoleNameParts)),
    case valid_role(RoleName) of
        true ->
            set_role(UserId, GuildId, RoleName, Api),
            {reply, <<"role ", RoleName/binary, " added">>, []};
        false ->
            {reply, <<"no such role ", RoleName/binary>>, []}
    end.

unset_role(RoleNameParts, Api,
         #{<<"author">> := #{<<"id">> := UserId}, <<"guild_id">> := GuildId}) ->
    RoleName = binary_join(lists:join(<<" ">>, RoleNameParts)),
    case valid_role(RoleName) of
        true ->
            unset_role(UserId, GuildId, RoleName, Api),
            {reply, <<"role ", RoleName/binary, " removed">>, []};
        false ->
            {reply, <<"no such role ", RoleName/binary>>, []}
    end.

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

%% internal functions

valid_role(<<"Tank">>) -> true;
valid_role(<<"Healer">>) -> true;
valid_role(<<"Melee DPS">>) -> true;
valid_role(<<"Phys Ranged">>) -> true;
valid_role(<<"Caster DPS">>) -> true;
valid_role(<<"Available">>) -> true;
valid_role(_) -> false.

ff_roles(#{<<"name">> := <<"Tank">>}) -> true;
ff_roles(#{<<"name">> := <<"Healer">>}) -> true;
ff_roles(#{<<"name">> := <<"Melee DPS">>}) -> true;
ff_roles(#{<<"name">> := <<"Phys Ranged">>}) -> true;
ff_roles(#{<<"name">> := <<"Caster DPS">>}) -> true;
ff_roles(_) -> false.

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

set_role(UserId, GuildId, RoleName, Api) ->
    RoleId = get_role_id(GuildId, RoleName, Api),
    discord_api:add_member_role(Api, GuildId, UserId, RoleId).

get_role_id(GuildId, RoleName, Api) ->
    Roles = discord_api:get_roles(Api, GuildId),
    Fn = fun(#{<<"name">> := Name}) -> Name =:= RoleName end,
    [#{<<"id">> := RoleId}] = lists:filter(Fn, Roles),
    RoleId.

unset_role(UserId, GuildId, RoleName, Api) ->
    RoleId = get_role_id(GuildId, RoleName, Api),
    discord_api:remove_member_role(Api, GuildId, UserId, RoleId).
