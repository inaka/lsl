%%% @doc cryptographic utilities
-module(lsl_crypto).

-export([secret/0, hash/1, match/2]).

%% @doc generates a secure randmo string
-spec secret() -> binary().
secret() ->
  binary:replace(
    base64:encode(crypto:strong_rand_bytes(32)),
    [<<$/>>, <<$=>>, <<$+>>], <<>>, [global]).

%% @equiv erlpass:hash(Value).
-spec hash(binary()) -> binary().
hash(Value) -> erlpass:hash([Value]).

%% @equiv erlpass:match(Password, PasswordHash).
-spec match(binary(), binary()) -> boolean().
match(Password, PasswordHash) ->
  erlpass:match([Password], PasswordHash).
