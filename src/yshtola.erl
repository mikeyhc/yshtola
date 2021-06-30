-module(yshtola).

-export([get_roles/3, set_role/3, unset_role/3, static/3]).

-define(MAX_STATIC_SIZE, 8).

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

get_roles(_Args, Api, #{<<"guild_id">> := GuildId}) ->
    RoleToMember = role_members(Api, GuildId),
    {reply, build_roles_reply(RoleToMember), []}.

static(_Args, Api, #{<<"guild_id">> := GuildId}) ->
    Roles = discord_api:get_roles(Api, GuildId),
    Members = discord_api:get_guild_members(Api, GuildId),
    {IsAvailable, ToNameWithRole} = create_role_functions(Roles),
    AvailableMembers = lists:filter(IsAvailable, Members),
    RoledMembers = lists:map(ToNameWithRole, AvailableMembers),
    SortedMembers = lists:sublist(random_sort(RoledMembers), ?MAX_STATIC_SIZE),
    case random_sort(top_scoring(build_statics(SortedMembers))) of
        [] -> {reply, <<"no possible statics available">>, []};
        [Static|_] ->
            {reply, render_static(Static), []}
    end.

%% internal functions

role_members(Api, GuildId) ->
    Roles = discord_api:get_roles(Api, GuildId),
    FFRoles = lists:filter(fun ff_named_role/1, Roles),
    Members = discord_api:get_guild_members(Api, GuildId),
    Fn = fun(#{<<"name">> := Name, <<"id">> := Id}) ->
        {Name, find_members(Id, Members)}
    end,
    maps:from_list(lists:map(Fn, FFRoles)).

ff_roles() -> [<<"Tank">>, <<"HoT Healer">>, <<"Shield Healer">>,
               <<"Melee DPS">>, <<"Phys Ranged">>, <<"Caster DPS">>].

valid_role(Role) ->
    case lists:member(Role, ff_roles()) of
        true -> true;
        false ->
            if Role =:= <<"Available">> -> true;
               true -> false
            end
    end.

ff_named_role(#{<<"name">> := Name}) ->
    lists:member(Name, ff_roles()).

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
    R1 = build_role_reply(<<"HoT Healer">>, RoleToMember),
    R2 = build_role_reply(<<"Shield Healer">>, RoleToMember),
    R3 = build_role_reply(<<"Melee DPS">>, RoleToMember),
    R4 = build_role_reply(<<"Phys Ranged">>, RoleToMember),
    R5 = build_role_reply(<<"Caster DPS">>, RoleToMember),
    binary_join([R0, R1, R2, R3, R4, R5]).

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

create_role_functions(Roles) ->
    FindAvailable = fun(#{<<"name">> := Name}) -> Name =:= <<"Available">> end,
    [#{<<"id">> := AvailableId}] = lists:filter(FindAvailable, Roles),
    NamedRoles = lists:filter(fun ff_named_role/1, Roles),
    NamedLookup = maps:from_list([{Id, Name} ||
                                  #{<<"id">> := Id, <<"name">> := Name}
                                  <- NamedRoles]),
    NameRoles = fun(X) -> maps:get(X, NamedLookup) end,
    IdRoles = lists:map(fun(#{<<"id">> := Id}) -> Id end, NamedRoles),
    FFn = fun(X) -> lists:member(X, IdRoles) end,
    FilterFn = fun(#{<<"roles">> := R0}) -> lists:member(AvailableId, R0) end,
    MapFn = fun(#{<<"nick">> := Nick, <<"roles">> := Roles0})
                  when Nick =/= null ->
                    {Nick, lists:map(NameRoles, lists:filter(FFn, Roles0))};
               (#{<<"user">> := #{<<"username">> := Username},
                  <<"roles">> := Roles0}) ->
                    {Username, lists:map(NameRoles, lists:filter(FFn, Roles0))}
            end,
    {FilterFn, MapFn}.

random_sort(L) ->
    [Y || {_,Y} <- lists:sort([{rand:uniform(), N} || N <- L])].

build_statics([]) -> [[]];
build_statics([{Name, Roles}|Rest]) ->
    Children = build_statics(Rest),
    NewGroups = lists:map(fun(R) -> group_add(Name, R, Children) end, Roles),
    Join = fun(X, Acc) -> X ++ Acc end,
    lists:filter(fun valid_group/1, lists:foldl(Join, [], NewGroups)).

group_add(_Name, _Role, []) -> [];
group_add(Name, Role, [H|T]) ->
    [[{Name, Role}|H]|group_add(Name, Role, T)].

valid_group(G) ->
    Tanks = lists:filter(fun({_, X}) -> <<"Tank">> =:= X end, G),
    HoTHealers = lists:filter(fun({_, X}) -> <<"HoT Healer">> =:= X end, G),
    ShieldHealers = lists:filter(
        fun({_, X}) -> <<"Shield Healer">> =:= X end, G),
    Melee = lists:filter(fun({_, X}) -> <<"Melee DPS">> =:= X end, G),
    Ranged = lists:filter(fun({_, X}) -> <<"Ranged DPS">> =:= X end, G),
    Caster = lists:filter(fun({_, X}) -> <<"Caster DPS">> =:= X end, G),
    length(Tanks) < 3
    andalso length(HoTHealers) < 2
    andalso length(ShieldHealers) < 2
    andalso length(Melee) < 3
    andalso length(Ranged) < 3
    andalso length(Caster) < 3
    andalso length(Melee) + length(Ranged) + length(Caster) < 4.

top_scoring([]) -> [];
top_scoring(Groups) ->
    [{S, G}|Rest] = lists:reverse(
                      lists:keysort(1, lists:map(fun score_group/1, Groups))),
    [G|lists:map(fun({_, X}) -> X end,
                 lists:takewhile(fun({X, _}) -> X =:= S end, Rest))].

score_group(Group) ->
    {score_group_(Group), Group}.

score_group_([]) -> 0;
score_group_([{_, <<"Tank">>}|Rest]) -> 7 + score_group_(Rest);
score_group_([{_, <<"HoT Healer">>}|Rest]) -> 6 + score_group_(Rest);
score_group_([{_, <<"Shield Healer">>}|Rest]) -> 6 + score_group_(Rest);
score_group_([{_, _}|Rest]) -> 5 + score_group_(Rest).

find_missing_roles(Current) ->
    Needed = [<<"Tank">>, <<"Tank">>,
              <<"Shield Healer">>, <<"HoT Healer">>,
              <<"DPS">>, <<"DPS">>, <<"DPS">>, <<"DPS">>],
    find_missing_roles(Current, Needed).

find_missing_roles([], Needed) -> Needed;
find_missing_roles([H|T], Needed) ->
    Role = case H of
               <<"Melee DPS">> -> <<"DPS">>;
               <<"Phys Ranged">> -> <<"DPS">>;
               <<"Caster DPS">> -> <<"DPS">>;
               R -> R
           end,
    find_missing_roles(T, lists:delete(Role, Needed)).

render_static(Static) ->
    Header = case length(Static) of
                 8 -> <<"Full Party">>;
                 _ -> <<"Light Party">>
             end,
    MapFn = fun({Name, Role}) -> <<"  ", Name/binary, " - ", Role/binary>> end,
    Members = binary_join(lists:join(<<"\n">>, lists:map(MapFn, Static))),
    case find_missing_roles(lists:map(fun({_, Role}) -> Role end, Static)) of
        [] -> <<Header/binary, "\n", Members/binary>>;
        MissingRoles ->
            MapFn2 = fun(Role) -> <<"  ", Role/binary>> end,
            Missing = binary_join(lists:join(<<"\n">>,
                                             lists:map(MapFn2, MissingRoles))),
            <<Header/binary, "\n", Members/binary, "\n\nMissing\n",
              Missing/binary>>
    end.
