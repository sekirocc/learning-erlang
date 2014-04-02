-module (curling_scoreboard_hw).
-export ([set_teams/2, next_round/0, add_points/1, reset_board/0]).

set_teams(TeamA, TeamB) ->
  io:format("ScoreBoard: Team ~s vs. Team ~s ~n", [TeamA, TeamB]).

next_round() ->
  io:format("ScoreBoard: round over~n").

add_points(Team) ->
  io:format("ScoreBoard: increased score of team ~s by 1 point~n", [Team]).

reset_board() ->
  io:format("ScoreBoard: All Teams are undefined and all scores are 0~n").