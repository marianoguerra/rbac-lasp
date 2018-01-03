-module(rbacl).

-export([add_group/1,
         add_group/2,
         groups/0,
         group/1,
         add_user/2,
         add_user/3,
         users/0,
         user/1,
         add_permission/1,
         permissions/0,
         user_join/2,
         group_join/2,
         user_groups/1,
         group_groups/1,
         user_grant/3,
         group_grant/3,
         grants/1]).

-export([watch_roles/1, watch_permissions/1, watch_memberhips/1, watch_grants/1]).

% roles: Map<String, Map<String, Any>>
% {RoleType, Name} => {Attr => Value}
-define(VAR_ROLES, {<<"roles">>, {state_awmap, [{state_awmap, [state_mvregister]}]}}).
% permissions: valid permission ids that can be assigned
-define(VAR_PERMS, {<<"perms">>, state_awset}).
% memberships: key is the RoleId, value is a set of roles it belongs to
-define(VAR_MEMBS, {<<"membs">>, {state_awmap, [state_awset]}}).
% grants: key is {ResourceType, ResourceId}, value is a map of permission id -> set of roles for that permission and that resource
-define(VAR_GRANTS, {<<"grants">>, {state_awmap, [{state_awmap, [state_awset]}]}}).

actor() -> node().

timestamp() ->
    erlang:unique_integer([monotonic, positive]).

watch_roles(Fun) ->
    watch(?VAR_ROLES, Fun).

watch_permissions(Fun) ->
    watch(?VAR_PERMS, Fun).

watch_memberhips(Fun) ->
    watch(?VAR_MEMBS, Fun).

watch_grants(Fun) ->
    watch(?VAR_GRANTS, Fun).

watch(LaspVar, Fun) ->
    lasp:stream(LaspVar, Fun).

add_user(Rolename, Password) ->
    add_user(Rolename, Password, #{}).

add_user(Rolename, Password, Details)
  when is_binary(Rolename), is_binary(Password), is_map(Details) ->
    add_role(user, Rolename, Details#{password => Password}).

users() ->
    list_role(user).

add_group(Rolename) ->
    add_group(Rolename, #{}).

add_group(Rolename, Details) when is_binary(Rolename), is_map(Details) ->
    add_role(group, Rolename, Details).

groups() ->
    list_role(group).

add_permission(Name) when is_binary(Name) ->
    lasp:update(?VAR_PERMS, {add, Name}, actor()).

permissions() ->
    {ok, Set} = lasp:query(?VAR_PERMS),
    sets:to_list(Set).

add_role(Role, Rolename, Details) ->
    Key = {Role, Rolename},
    Ts = timestamp(),
    Actor = actor(),
    Update = {apply, Key, {apply, details, {set, Ts, Details}}},
    lasp:update(?VAR_ROLES, Update, Actor).

role_details_data(PList) ->
    maps:from_list([{Key, sets:to_list(Val)} || {Key, Val} <- PList]).

list_role(Role) ->
    {ok, PropList} = lasp:query(?VAR_ROLES),
    [{Key, role_details_data(Value)} || {{R, Key}, Value} <- PropList, R =:= Role].

user_join(Username, Groupname)
  when is_binary(Username), is_binary(Groupname) ->
    role_join(user, Username, Groupname).

group_join(GroupnameSrc, Groupname)
  when is_binary(GroupnameSrc), is_binary(Groupname) ->
    role_join(group, GroupnameSrc, Groupname).

user_groups(Username) when is_binary(Username) ->
    role_groups(user, Username).

group_groups(Groupname) when is_binary(Groupname) ->
    role_groups(group, Groupname).

user(Username) when is_binary(Username) ->
    role_details(user, Username, ?VAR_ROLES).

group(Groupname) when is_binary(Groupname) ->
    role_details(group, Groupname, ?VAR_ROLES).

user_grant(Username, Permission, Resource)
  when is_binary(Username), is_binary(Permission) ->
    role_grant(user, Username, Permission, Resource).

group_grant(Groupname, Permission, Resource)
  when is_binary(Groupname), is_binary(Permission) ->
    role_grant(group, Groupname, Permission, Resource).

role_grant(Role, Rolename, Permission, Resource) ->
    case role_exists(Role, Rolename) of
        false -> {error, {notfound, {Role, Rolename}}};
        true ->
            case permission_exists(Permission) of
                false -> {error, {notfound, {permission, Permission}}};
                true ->
                    role_grant_raw(Role, Rolename, Permission, Resource)
            end
    end.

role_grant_raw(Role, Rolename, Permission, Resource) ->
    Actor = actor(),
    Update = {apply, Resource, {apply, Permission, {add, {Role, Rolename}}}},
    lasp:update(?VAR_GRANTS, Update, Actor),
    ok.

grants(Resource) ->
    {ok, PList} = lasp:query(?VAR_GRANTS),
    Grants = [V || {R, V} <- PList, R =:= Resource],
    case Grants of
         [] -> {ok, []};
         [PermRolesPList] -> {ok, PermRolesPList}
    end.

permission_exists(Permission) ->
    {ok, Set} = lasp:query(?VAR_PERMS),
    sets:is_element(Permission, Set).

role_groups(Role, Rolename) ->
    case role_details_raw(Role, Rolename, ?VAR_MEMBS) of
        notfound -> [];
        {ok, Membs} -> [Name || {group, Name} <- sets:to_list(Membs)]
    end.

role_join(Role, Rolename, Groupname) ->
    case {role_exists(Role, Rolename), role_exists(group, Groupname)} of
        {true, true} ->
            Key = {Role, Rolename},
            Actor = actor(),
            Update = {apply, Key, {add, {group, Groupname}}},
            lasp:update(?VAR_MEMBS, Update, Actor);
        {false, _} ->
            {error, {notfound, {Role, Rolename}}};
        {true, false} ->
            {error, {notfound, {group, Groupname}}}
    end.

role_exists(Role, Rolename) ->
    case role_details_raw(Role, Rolename, ?VAR_ROLES) of
        notfound -> false;
        _ -> true
    end.

role_details_raw(Role, Rolename, LaspVar) ->
    {ok, PropList} = lasp:query(LaspVar),
    R = [{Key, Value} || {{R, Key}, Value} <- PropList,
                         R =:= Role andalso Key =:= Rolename],
    case R of
        [] -> notfound;
        [{_, Value}] -> {ok, Value}
    end.

role_details(Role, Rolename, LaspVar) ->
    case role_details_raw(Role, Rolename, LaspVar) of
        notfound -> notfound;
        {ok, Value} -> {ok, role_details_data(Value)}
    end.

