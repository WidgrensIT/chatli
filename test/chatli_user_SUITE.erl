-module(chatli_user_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(BASEPATH, <<"http://localhost:8080">>).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(_Config) ->
    Username = <<"username1">>,
    Password = <<"1234">>,
    Username2 = <<"username2">>,
    Password2 = <<"12345">>,
    User1 = #{<<"email">> => <<"my@email.com">>,
              <<"username">> => Username,
              <<"password">> => Password},
    User2 = #{<<"phoneNumber">> => <<"123456">>,
              <<"username">> => Username2,
              <<"password">> => Password2},
    Path = [?BASEPATH, <<"/v1/signup">>],
    #{status := {201, _}} = shttpc:post(Path, encode(User1), opts()),
    Path = [?BASEPATH, <<"/v1/signup">>],
    #{status := {201, _}} = shttpc:post(Path, encode(User2), opts()),
    LoginPath = [?BASEPATH, <<"/v1/login">>],
    #{status := {200, _}, body := LoginRespBody} = shttpc:post(LoginPath, encode(#{username => Username,
                                                                                   password => Password}), opts()),
    #{access_token := Token} = decode(LoginRespBody),
    [_ , Payload, _] = bstring:tokens(Token, <<".">>),
    UserObj1 = decode(base64:decode(Payload)),
    #{status := {200, _}, body := LoginRespBody2} = shttpc:post(LoginPath, encode(#{username => Username2,
                                                                                    password => Password2}), opts()),
    #{access_token := Token2} = decode(LoginRespBody2),
    [_ , Payload2, _] = bstring:tokens(Token2, <<".">>),
    UserObj2 = decode(base64:decode(Payload2)),
    [{user1, #{object => UserObj1,
               token => Token}},
     {user2, #{object => UserObj2,
               token => Token2}}].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    #{object := #{id := Id}} = proplists:get_value(user1, Config),
    #{object := #{id := Id2}} = proplists:get_value(user2, Config),
    Path = [?BASEPATH, <<"/client/user/">>, Id],
    shttpc:delete(Path, opts()),
    Path2 = [?BASEPATH, <<"/client/user/">>, Id2],
    shttpc:delete(Path2, opts()),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [get_all_users].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
get_all_users(Config) ->
    #{token := Token} =  proplists:get_value(user1, Config),
    Path = [?BASEPATH, <<"/client/user">>],
    #{status := {200, _}, body := RespBody} = shttpc:get(Path, opts(Token)),
    2 = length(decode(RespBody)).

opts() ->
    opts(undefined).
opts(undefined) ->
    #{headers => #{'Content-Type' => <<"application/json">>}, close => true};
opts(Token) ->
    ct:log("Token is: ~p", [Token]),
    Res = #{headers => #{'Content-Type' => <<"application/json">>,
                         'Authorization' => <<"Bearer ", Token/binary>>},
            close => true},
    ct:log("Returning opts: ~p", [Res]),
    Res.

decode(Json) ->
    json:decode(Json, [maps, atom_keys]).

encode(Json) ->
    json:encode(Json, [maps, binary]).