rbacl
=====

An experimental RBAC library using lasp

.. code-block:: erlang

    MakeLog = fun(Type) -> fun(V) -> io:format("~p: ~p~n", [Type, V]) end end.
    rbacl:watch_roles(MakeLog(roles)).
    % ok
    rbacl:watch_perms(MakeLog(perms)).
    % ok
    rbacl:watch_memberhips(MakeLog(memberships)).
    % ok
    rbacl:watch_grants(MakeLog(grants)).
    % ok

    rbacl:users().
    % []
    rbacl:groups().
    % []
    rbacl:permissions().
    % []

    rbacl:add_user(<<"bob">>, <<"secret">>).
    % {ok, {{<<"roles">>, LaspVarType}, LaspVarType, _, _}}
    rbacl:user(<<"bob">>).
    % {ok,#{details => [#{password => <<"secret">>}]}}
    rbacl:user(<<"sandy">>).
    % notfound
    rbacl:add_group(<<"users">>).
    % {ok, {{<<"roles">>, LaspVarType}, LaspVarType, _, _}}
    rbacl:add_user(<<"sandy">>, <<"texas">>).
    % {ok, {{<<"roles">>, LaspVarType}, LaspVarType, _, _}}
    rbacl:user(<<"sandy">>).
    % {ok,#{details => [#{password => <<"texas">>}]}}
    Perm = <<"data.read">>.
    rbacl:add_permission(Perm).
    % {ok, {{<<"perms">>, LaspVarType}, LaspVarType, _, _}}
    rbacl:permissions().
    % [<<"data.read">>]
    rbacl:user_groups(<<"sandy">>).
    % []
    rbacl:user_join(<<"wat">>, <<"users">>).
    % {error,{notfound,{user,<<"wat">>}}}
    rbacl:user_join(<<"sandy">>, <<"users">>).
    % {ok, {{<<"membs">>, LaspVarType}, LaspVarType, _, _}}
    rbacl:user_groups(<<"sandy">>).
    % [<<"users">>]

    Resource = {key, <<"ns">>, <<"k1">>}.
    rbacl:user_grant(<<"bob">>, <<"watperm">>, Resource).
    % {error,{notfound,{permission,<<"watperm">>}}}
    rbacl:user_grant(<<"watuser">>, Perm, Resource).
    % {error,{notfound,{user,<<"watuser">>}}}
    rbacl:user_grant(<<"bob">>, Perm, Resource).
    ok
    rbacl:grants(Resource).
    % {ok,[{<<"data.read">>, Set}]}

    rbacl:group_grant(<<"users">>, <<"watperm">>, Resource).
    % {error,{notfound,{permission,<<"watperm">>}}}
    rbacl:group_grant(<<"watgroup">>, Perm, Resource).
    % {error,{notfound,{group,<<"watgroup">>}}}
    rbacl:group_grant(<<"users">>, Perm, Resource).
    % ok
    {ok, [{Role, GrantsSet}]} = rbacl:grants(Resource).
    sets:to_list(GrantsSet).
    % [{user,<<"bob">>},{group,<<"users">>}]

Build
-----

::

    rebar3 release

Test
----

::

    rebar3 ct

Run
---

::

    rebar3 run

Clustering
----------

::

    make devrel

    # on 3 different shells
    make dev1-console
    make dev2-console
    make dev3-console

    # join all nodes:
    make devrel-join

    # check node members
    make devrel-status

    # join node1 to node2 manually:
    ./_build/dev1/rel/rbacl/bin/rbacl-admin cluster join rbacl2@127.0.0.1

    # check node1 members
    ./_build/dev1/rel/rbacl/bin/rbacl-admin cluster members

    # check node1 connections
    ./_build/dev1/rel/rbacl/bin/rbacl-admin cluster connections

Ping node2 from node1 using partisan::

    1> rbacl:ping('rbacl2@127.0.0.1').
    ok

    % check logs/console on node2, you should see:
    got msg ping

Run some Lasp code:

On one of the nodes' shell run:

.. code-block:: erlang

    Key1 = <<"key1">>.
    Key2 = <<"key2">>.
    Timestamp = fun () -> erlang:unique_integer([monotonic, positive]) end.

    AwMapType = {state_awmap, [state_mvregister]}.
    AwMapVarName = <<"awmap">>.
    AwMapVal = #{what => i_am_an_awmap_value}.

    % declare the variable
    {ok, {AwMap, _, _, _}} = lasp:declare({AwMapVarName, AwMapType}, AwMapType).

    % update its content setting Key1 = AwMapVal
    {ok, {AwMap1, _, _, _}} = lasp:update(AwMap, {apply, Key1,
                                                  {set, Timestamp(), AwMapVal}},
                                          self()).
    % timestamp argument is not needed in mvregister, it's only for compatibility
    % with lwwregister
    {ok, _} = lasp:update(AwMap, {apply, Key1, {set, nil, AwMapVal}}, self()).

    % get the value
    {ok, AwMapRes} = lasp:query(AwMap1).

    AwMapRes.
    % {ok,[{<<"key1">>, {set, ...#{what => i_am_an_awmap_value} ... }}]}

    [{_, AwMapSet}] = AwMapRes.
    sets:to_list(AwMapSet).
    % [#{what => i_am_an_awmap_value}]

in another one run:

.. code-block:: erlang

    {ok, AwMapRes} = lasp:query({<<"awmap">>,{state_awmap,[state_mvregister]}}).

    AwMapRes.

    [{_, AwMapSet}] = AwMapRes.
    sets:to_list(AwMapSet).

You should get:

.. code-block:: erlang

    [#{what => i_am_an_awmap_value}]


Quit
----

::

    1> q().

TODO
----

* define license and create LICENSE file

License
-------

TODO
