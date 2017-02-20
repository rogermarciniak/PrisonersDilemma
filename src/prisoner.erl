%%%-------------------------------------------------------------------
%%% @author roger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Nov 2016 10:52
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-module(prisoner).
-author("roger").
-export([create/3,getName/1,getStrategy/1,getState/1,prisoner/1]).

%----TESTS----%
name_test()->
  PID=create(nme,do_cooperate,[]),
  PID!{self(),name},
  receive
    {PID,name,Name}->?assert(nme=:=Name)
  end.
%------*------%

create(Name, Strat,State)-> %create prisoner
  PID=spawn(?MODULE,prisoner,[{Name,Strat,State}]),
  PID.

getName(PID)-> %get prisoner name
  PID!{self(),name},
  receive
    {PID,name,Name}->
      Name
  end.

getStrategy(PID)-> %get prisoner strat
  PID!{self(),strat},
  receive
    {PID,strat,Strat}->
      Strat
  end.

getState(PID)-> %get past decisions
  PID!{self(),state},
  receive
    {PID,state,State}->
      State
  end.

prisoner({Name,Strat,State})-> %save prisoner information
  receive
    {Sender,name} ->
      Sender!{self(),name,Name},
      prisoner({Name,Strat,State});
    {Sender,state} ->
      Sender!{self(),state,State},
      prisoner({Name,Strat,State});
    {Sender,strat} ->
      Sender!{self(),strat,Strat},
      prisoner({Name,Strat,State});
    {Sender,choice, OppName}->
      P1choice=case Strat of
                 random-> random();
                 titForTatRandom-> titForTatRandom(oppDecisionFilter(OppName,State));
                 suspiciousTitForTat-> suspiciousTitForTat(oppDecisionFilter(OppName,State));
                 grudger-> grudger(oppDecisionFilter(OppName,State));
                 do_cooperation-> do_cooperation;
                 do_defect-> do_defect
               end,
      Sender!{self(),choice,P1choice},
      receive
        {_,result,P2choice}->
          prisoner({Name,Strat,[{OppName,P2choice,P1choice}|State]})
      end
  end.

oppDecisionFilter(Opp,State)-> %list filter for opp decisions
  lists:filter(fun(Turn)->
    case Turn of
      {Opp,_,_}->
        true;
      _->
        false
    end end, State).

random()-> %9- 50 50 random
  Options=[do_defect, 'do_cooperation'],
  PickedOption=rand:uniform(length(Options)),
  lists:nth(PickedOption,Options).

titForTatRandom(State)-> %2- titfortat but goes random when planned to cooperate
  case State of
    [{_,do_defect,_}|_]->
      do_defect;
    _->
      random()
  end.

suspiciousTitForTat(State) -> %17- titfortat but initially defect
  case State of
    [{_,do_cooperation,_}|_]->
      do_cooperation;
    _ ->
      do_defect
  end.

grudger(State)-> %12- grudger
  case State of
    [{_,do_cooperation,_}|Rest]->
      grudger(Rest);
    [{_,do_defect,_}|_]->
      do_defect;
    _->
      do_cooperation
  end.
