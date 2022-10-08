%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2022 9:59 PM
%%%-------------------------------------------------------------------
-module(ct_help).
-author("bnjm").

%% API
-export([all/1, config/2, data_dir/1, compile_all_data_dir/1]).

all(Suite) ->
    AllExported = Suite:module_info(exports),
    lists:filtermap(
        fun({Fun, _Arity}) ->
            case re:run(erlang:atom_to_list(Fun), "^(init_|end_|module_info|all|do_)") of
                nomatch -> {true, Fun};
                _ -> false
            end
        end,
        AllExported
    ).

config(Key, Config) when is_atom(Key) -> proplists:get_value(Key, Config).
data_dir(Config) -> config(data_dir, Config).

%% @doc
%% Tries to compile all files residing in the data_dir of the given SUITE config
%% @end
compile_all_data_dir(Config) ->
    DataDir = ct_help:data_dir(Config),
    {ok, Files} = file:list_dir(DataDir),
    lists:foreach(
        fun(FilePath) ->
            F = filename:join(DataDir, FilePath),
            compile:file(F)
        end,
        Files
    ),
    ok.
