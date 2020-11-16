-module(chatli_auth).

-export([auth_jwt/1]).

-include("chatli.hrl").

-spec auth_jwt(Req :: cowboy_req:req()) -> {true, Claims :: map()} | false.
auth_jwt(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, Token} ->
            try jwerl:verify(Token, hs512, ?SECRET) of
                {ok, Claims} ->
                    {true, Claims};
                {error, invalid_signature} ->
                    false
            catch
                _:_ ->
                    false
            end;
        _ -> false
    end.