-module(rover_utils).

-export([ get_register_token/0
	, get_country_code/1
	, get_verification_token/2
	, get_ast/0
	, split_by_comma/1
	, join_by_comma/1
	]).

-include("rover.hrl").

get_register_token() ->
    DefaultAllowedChars =
        "123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz",
    list_to_binary(get_rand_str(23, DefaultAllowedChars)).

get_country_code(_IP) ->
    <<"98">>.

get_verification_token(CC, PNO) ->
    {ok, {TestPnoFrom, TestPnoTo}} = rover_conf:get('test.ct.pno'),
    {ok, VerificationCode} = rover_conf:get('test.ct.verification.code'),
    {ok, {VerificationCodeRangeFrom, VerificationCodeRangeTo}} = rover_conf:get('verification.token.range'),
    
    PhoneNumber = list_to_integer(binary_to_list(CC) ++ binary_to_list(PNO)),
    if (PhoneNumber >= TestPnoFrom) and (PhoneNumber =< TestPnoTo) ->
	    { list_to_binary(integer_to_list(VerificationCode))
	    , length(integer_to_list(VerificationCode))
	    };
       true ->
	    VC = integer_to_list(crypto:rand_uniform(VerificationCodeRangeFrom, VerificationCodeRangeTo)),
	    VerificationToken = list_to_binary(VC),
	    %% === Send Verification Token via SMS.
	    ok = rover_sms:send(CC, PNO, VerificationToken),
	    { VerificationToken
	    , length(VC)
	    }
    end.

-spec get_ast() -> string().
get_ast() ->
    DefaultAllowedChars =
        "123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz",
    list_to_binary(get_rand_str(32, DefaultAllowedChars)).
    
-spec split_by_comma(string() | bitstring()) -> list().
split_by_comma(IN) when is_bitstring(IN) ->     
    split_by_comma(binary_to_list(IN));
split_by_comma([]) -> [];
split_by_comma(IN) when is_list(IN) ->
    {E, L} = lists:foldl(fun($,, {Temp, Acc}) ->
				 {"", Acc ++ [list_to_binary(Temp)]};
			    (I, {Temp, Acc}) ->
				 {Temp ++ [I], Acc}
			 end, {[], []}, IN),
    [list_to_binary(E)] ++ L.

get_rand_str(CharSize, AllowedChars) ->
    [begin
         Picked = crypto:rand_uniform(1, length(AllowedChars)),
         lists:nth(Picked, AllowedChars)
     end || _ <- lists:seq(1, CharSize)].

-spec join_by_comma(list()) -> bitstring().
join_by_comma([]) -> <<"">>;
join_by_comma([IN]) -> IN;    
join_by_comma(IN) ->
    R = lists:foldl(fun(E, Acc) when is_list(E) ->
			    E ++ "," ++ Acc;
		       (E, Acc) when is_bitstring(E) ->
			    binary_to_list(E) ++ "," ++ Acc
		    end, "", IN),
    list_to_binary(lists:droplast(R)).
    
	  
