%%%-------------------------------------------------------------------
%%% @author roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Nov 2016 10:54
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-module(warden).
-author("roger").
-export([benchmark/1,playGame/1,start/1,add/2,stats/1,run/2,supervisor/1]).



benchmark(N)-> %code for measuring execution time
  statistics(runtime),
  statistics(wall_clock),

  playGame(N), %play the game inside the benchmark

  {_,Time1}=statistics(runtime),
  {_,Time2}=statistics(wall_clock),
  U1=Time1*1000,
  U2=Time2*1000,
  io:format("Code time=~p (~p) microseconds~n",
    [U1,U2]).

playGame(NoOfTurns)-> %play the game
  %AVAILABLE STRATEGIES: random, titForTatRandom, suspiciousTitForTat, grudger%
  Joseph=prisoner:create(joseph,titForTatRandom,[]), %format follows: (prisonername,strategy,[])
  Roger=prisoner:create(roger,grudger,[]),
  Supervisor=warden:start({[],[]}),
  warden:add(Supervisor,Joseph),
  warden:add(Supervisor,Roger),
  warden:run(Supervisor,NoOfTurns),
  warden:stats(Supervisor).

start(State)-> %begin warden instance
  ets:new(log,[set,named_table,public]),
  PID=spawn(?MODULE,supervisor,[State]),
  PID.

add(SupPID,PID)-> %give prisoner to warden
  SupPID!{self(),add,PID},
  receive
    {SupPID,done,Total} ->
      Total
  end.

stats(SupPID)-> %get list of prisoners decisions
  SupPID!{self(),stats},
  receive
    {SupPID, Log}->
      lists:keysort(2, Log)
  end.

run(SupPID,Ntimes)-> %play ? times
  SupPID!{self(),run,Ntimes},
  receive
    {SupPID,done}->
      ok
  end.

supervisor({Prisoners,ActionLog})-> %warden player takes care of everything
  receive
    {Sender,add,PID}->
      Sender!{self(),done,length(Prisoners)+1},
      supervisor({[PID|Prisoners],ActionLog});
    {Sender,stats}->
      Sender!{self(), etsToList(ets:first(log),[])},
      supervisor({Prisoners,ActionLog});
    {Sender,run,Count}->
      {NewActionLog}=iterateAnswers(Prisoners,ActionLog,Count),
      Sender!{self(),done},
      supervisor({Prisoners,NewActionLog})
  end.

etsToList(Name,List) %ets to list
  when Name=='$end_of_table'->
  lists:append(List);
etsToList(Name,List)->
  etsToList(ets:next(log,Name),[ets:lookup(log,Name)|List]).

iterateAnswers(_,ActionLog,0)-> %run prisoner answers N times
  {ActionLog};

iterateAnswers(Prisoners,ActionLog,N)->
  {NewActionLog}=doOneRun(Prisoners,ActionLog),
  iterateAnswers(Prisoners,NewActionLog,N-1).

doOneRun([],ActionLog)-> %does one round of answers
  {ActionLog};
doOneRun([First|Rest],ActionLog) ->
  {NewActionLog}=doOnce(First,Rest,ActionLog),
  doOneRun(Rest,NewActionLog).

doOnce(_,[],ActionLog)-> %executes the round
  {ActionLog};
doOnce(Prisoner1,[Prisoner2|Rest],ActionLog)->
  Prisoner1 !{self(),name},
  receive
    {Prisoner1,name,MyName}->
      ok
  end,
  Prisoner2 !{self(),name},
  receive
    {Prisoner2,name,OtherName}->
      ok
  end,
  Prisoner2 !{self(),choice,MyName},
  receive
    {Prisoner2,choice,OtherChoice}->
      ok
  end,
  Prisoner1 !{self(),choice,OtherName},
  receive
    {Prisoner1,choice, P1choice}->
      ok
  end,
  Prisoner2 !{self(),result, P1choice},
  Prisoner1 !{self(),result,OtherChoice},
  addUpSentence({P1choice, OtherChoice},MyName),
  addUpSentence({OtherChoice, P1choice},OtherName),
  doOnce(Prisoner1,Rest,[{MyName,P1choice,OtherName,OtherChoice}|ActionLog]).

addUpSentence(ThisRound,CurName)-> %filters the log & adds up years to serve
  case ets:lookup(log,CurName) of
    []->
      ets:insert(log,{CurName, addUpYrsToServe(ThisRound,0)});
    [{Name,YrsToServe}] ->
      ets:delete(log,Name),
      ets:insert(log,{Name, addUpYrsToServe(ThisRound,YrsToServe)})
  end.

addUpYrsToServe(ThisRound,YrsToServe)-> %adds up yrs to serve
  case ThisRound of
    {do_cooperation,do_defect}->
      YrsToServe;
    {do_defect,do_defect}->
      YrsToServe+1;
    {do_cooperation,do_cooperation}->
      YrsToServe+2;
    {do_defect,do_cooperation}->
      YrsToServe+3
  end.