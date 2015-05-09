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
hash(Value) -> erlpass:hash(Value).

%% @doc Sometimes, bcrypt fails to hash, causing lsl_crypto:match to fail,
%%      so we try 5 times in a row.
-spec match(binary(), binary()) -> boolean().
match(Password, PasswordHash) ->
  match(5, Password, PasswordHash).

match(1, Password, PasswordHash) ->
  erlpass:match(Password, PasswordHash);
match(N, Password, PasswordHash) ->
  try
    erlpass:match(Password, PasswordHash)
  catch
    _:_ ->
      match(N - 1, Password, PasswordHash)
  end.
