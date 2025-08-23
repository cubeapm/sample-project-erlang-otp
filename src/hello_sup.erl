-module(hello_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  io:format("Starting supervisor...~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", hello_handler, []},
      {"/param/:id", hello_handler, [{action, param}]},
      {"/exception", hello_handler, [{action, exception}]}
    ]}
  ]),

  CowboyOpts = #{
    env => #{dispatch => Dispatch}
  },

  CowboySpec = #{
    id => cowboy_http_listener,
    start => {cowboy, start_clear, [http_listener, [{port, 8080}], CowboyOpts]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [cowboy]
  },

  {ok, {{one_for_one, 5, 10}, [CowboySpec]}}.
