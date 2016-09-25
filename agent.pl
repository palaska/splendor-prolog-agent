:- module(me2010401045, []).

initialize(PlayerName, PlayerCount) :-
  show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).

bonusGem(1, [1,0,0,0,0,0]).
bonusGem(2, [0,1,0,0,0,0]).
bonusGem(3, [0,0,1,0,0,0]).
bonusGem(4, [0,0,0,1,0,0]).
bonusGem(5, [0,0,0,0,1,0]).


decideAction(Player, _, StateProxy, Action) :-
  call(StateProxy, Player, gems, Gems),
  call(StateProxy, Player, bonuses, Bonuses),
  call(StateProxy, Player, reserves, Reserves),
  call(StateProxy, game, cards, Cards),
  call(StateProxy, game, nobles, Nobles),
  currentBonusPriority(Nobles, Bonuses, BonusPrio),
  (
    canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
    cardPointList(StateProxy, Player, CanBuyCards, PointList, Nobles, BonusPrio),
    maximum_list(PointList, _, Index),
    nth1(Index, CanBuyCards, CardId),
    Action = buyCard(CardId)
  ;
    %length(Reserves, ReservesLength),
    %ReservesLength<3,
    %nth1(2, Cards, ReservedCard),
    %Action = reserveCard(ReservedCard)
    %;
    cardPointList(StateProxy, Player, Cards, PointList2, Nobles, BonusPrio),
    maximum_list(PointList2, _, Index2),
    nth1(Index2, Cards, CardId2),
    card(CardId2, NeededGems-_-_),
    call(StateProxy, game, tokens, Tokens),
    feasibleGetGems(Gems, Bonuses, NeededGems, Tokens, TakenGems, BackGems),
    %randomGetGems(Gems, Tokens, RandGems, BackGems),
    %Action = getGems(RandGems, BackGems)
    Action = getGems(TakenGems, BackGems);

    call(StateProxy, game, tokens, Tokens),
    randomGetGems(Gems, Tokens, RandGems, BackGems),
    Action = getGems(RandGems, BackGems)
  )
  .

feasibleGetGems(Gems, Bonuses, NeededGems, Tokens, TakenGems, BackGems):-
  list_butlast(Tokens,Tokens2),
  addGems(Gems, Bonuses, TotalGems),
  subtractGems(NeededGems, TotalGems, GemsToTake),
  nonzeroIndex(GemsToTake, 1, NonZeroReq),
  nonzeroIndex(Tokens2, 1, NonZeroPres),
  inter(NonZeroReq,NonZeroPres,Feas),
  union(Feas,NonZeroPres,Uni),
  take(3,Uni,TakenIndices),
  nth1(1,TakenIndices, TakenIndex1),
  nth1(2,TakenIndices, TakenIndex2),
  nth1(3,TakenIndices, TakenIndex3),
  bonusGem(TakenIndex1, TakenG1),
  bonusGem(TakenIndex2, TakenG2),
  bonusGem(TakenIndex3, TakenG3),
  addGems(TakenG1,TakenG2,TakenTemp),
  addGems(TakenG3, TakenTemp, TakenGems),
  addGems(Gems, TakenGems, NewGems),
  gemCount(NewGems,GemCount),
  ExcessGemCount is GemCount -10,
  (ExcessGemCount=<0,BackGems=[0,0,0,0,0,0];ExcessGemCount>0,randomGems(NewGems, ExcessGemCount, false, BackGems)).


  gemCount([N1,N2,N3,N4,N5,N6], N) :-
    N is N1+N2+N3+N4+N5+N6.

  union([A|B], C, D) :- member(A,C), !, union(B,C,D).
union([A|B], C, [A|D]) :- union(B,C,D).
union([],Z,Z).

subtract(A,B,C):-
  findall(X, (member(X,A), not(member(X,B))), C).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

inter([], _, []).

inter([H1|T1], L2, [H1|Res]) :-
  member(H1, L2),
  inter(T1, L2, Res).

inter([_|T1], L2, Res) :-
  inter(T1, L2, Res).

nonzeroIndex([], _, []).
  nonzeroIndex([H|T], I, A) :-
    J is I+1,
    nonzeroIndex(T, J, B),
    (H>0,A = [I|B];H=<0,A = B).

    cardPointList(_,_,[],[],_,_):-!.
  cardPointList(StateProxy,Player,[CardId|CT], [PH|PT], Nobles, BonusPrio):-
    %initialCards(L1,L2,L3),
    %append([L1,L2,L3], AllCards),
    %member(CardId, AllCards),

    card(CardId, _-BonusColor-Points),
    nth1(Ind,BonusPrio,BonusColor),
    Ind2 is 5-Ind,
    Ind3 is Ind2/10,
    %(((CardId<100, TierPt is 0.3);(CardId<200, TierPt is 0.2)); TierPt is 0.1),

    call(StateProxy, Player, bonuses, Bonuses),
    bonusGem(BonusColor, BonusGem),
    addGems(Bonuses, BonusGem, NewBonuses),
    canGetNobles(NewBonuses, NobleGets, Nobles),

    length(NobleGets, NobleGetCount),
    (
      NobleGetCount = 0,
      PH is Points + Ind3
    ;
      NobMult is NobleGetCount*3,
      PH is Points + Ind3 + NobMult
    ),

    cardPointList(StateProxy,Player,CT,PT,Nobles,BonusPrio),!.


  selectNoble([H|_],H).


canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
  (
    canBuyCard(Gems, Bonuses, H),
    A = [H|X2]
  ;
    A=X2
  ),
  canBuyCards(Gems, Bonuses, T, X2)
  .

sumAux([],Bonuses,TotalNobleCosts, TotalNobleCosts, TotalNobleCostsCnt, TotalNobleCostsCnt).
sumAux([NId-X-C|T],Bonuses, AccumNobleCosts, SumNobleCosts, AccumNobleCnt, SumNobleCnt) :-
  subtractGems(X,Bonuses,BonusRem),
  (
    (
      (nth1(1,BonusRem,WhiteDev),WhiteDev>0,Devels1=[WhiteDev],Devels1Cnt=[1]);(Devels1=[0],Devels1Cnt=[0])
      ),
    (
      (nth1(2,BonusRem,BlueDev),BlueDev>0, append(Devels1,[BlueDev],Devels2),append(Devels1Cnt,[1],Devels2Cnt)); (append(Devels1,[0],Devels2),append(Devels1Cnt,[0],Devels2Cnt))
    ),
    (
      (nth1(3,BonusRem,GreenDev),GreenDev>0, append(Devels2,[GreenDev],Devels3),append(Devels2Cnt,[1],Devels3Cnt)); (append(Devels2,[0],Devels3),append(Devels2Cnt,[0],Devels3Cnt))
    ),
    (
      (nth1(4,BonusRem,RedDev),RedDev>0, append(Devels3,[RedDev],Devels4),append(Devels3Cnt,[1],Devels4Cnt)); (append(Devels3,[0],Devels4),append(Devels3Cnt,[0],Devels4Cnt))
    ),
    (
      (nth1(5,BonusRem,BlackDev),BlackDev>0, append(Devels4,[BlackDev],Devels5),append(Devels4Cnt,[1],Devels5Cnt)); (append(Devels4,[0],Devels5),append(Devels4Cnt,[0],Devels5Cnt))
    )
  ),
  myAddL(AccumNobleCosts, Devels5, NewAccumNobleCosts),
  myAddL(AccumNobleCnt, Devels5Cnt, NewAccumNobleCnt),
  sumAux(T, Bonuses, NewAccumNobleCosts, SumNobleCosts, NewAccumNobleCnt, SumNobleCnt).

sum(Nobles, Bonuses, SumNobleCosts, SumNobleCnt) :- sumAux(Nobles, Bonuses, [0,0,0,0,0], SumNobleCosts, [0,0,0,0,0], SumNobleCnt).


currentBonusPriority(Nobles, Bonuses, Y) :-
  sum(Nobles, Bonuses, RLNc, LNcc),
  list_butlast(Bonuses, Bonuses2),
  multwith01(Bonuses2, Bonuses3),
  myAddL(Bonuses3,RLNc, RLNc2),
  fix0s(LNcc,RLNcc),
  mySqL(RLNcc,SqLNcc),
  myDivL(RLNc2,SqLNcc, RealPrio),
  %myDivL(RLNc,SqLNcc, RealPrio),
  appendIndex(RealPrio, IReadPrio),
  %appendIndexRev(RealPrio, IReadPrio),
  keysort(IReadPrio, Sorted),
  getSortedIndex(Sorted, Y)
  .

list_butlast([X|Xs],Ys) :-
  list_butlast_prev(Xs,Ys,X).

list_butlast_prev([],[],_).
list_butlast_prev([X1|Xs],[X0|Ys],X0) :-
  list_butlast_prev(Xs,Ys,X1).

fix0s([A1,A2,A3,A4,A5], [B1,B2,B3,B4,B5]):-
  ((A1 = 0, B1 is 0.001); (B1 is A1)),
  ((A2 = 0, B2 is 0.001); (B2 is A2)),
  ((A3 = 0, B3 is 0.001); (B3 is A3)),
  ((A4 = 0, B4 is 0.001); (B4 is A4)),
  ((A5 = 0, B5 is 0.001); (B5 is A5)).

multwith01([A1,A2,A3,A4,A5], [B1,B2,B3,B4,B5]):-
  B1 is A1 * 0.1,
  B2 is A2 * 0.1,
  B3 is A3 * 0.1,
  B4 is A4 * 0.1,
  B5 is A5 * 0.1.

appendIndex([A1,A2,A3,A4,A5],[B1,B2,B3,B4,B5]):-
  B1 = A1-1,
  B2 = A2-2,
  B3 = A3-3,
  B4 = A4-4,
  B5 = A5-5.

appendIndexRev([A1,A2,A3,A4,A5],[B1,B2,B3,B4,B5]):-
  B1 = A1-5,
  B2 = A2-4,
  B3 = A3-3,
  B4 = A4-2,
  B5 = A5-1.

getSortedIndex([A-IA,B-IB,C-IC,D-ID,E-IE], [IA,IB,IC,ID,IE]):-
  true.


myAddL([A1,A2,A3,A4,A5],[B1,B2,B3,B4,B5],[C1,C2,C3,C4,C5]) :-
  C1 is A1+B1,
  C2 is A2+B2,
  C3 is A3+B3,
  C4 is A4+B4,
  C5 is A5+B5.

mySqL([A1,A2,A3,A4,A5],[B1,B2,B3,B4,B5]) :-
  B1 is A1*A1,
  B2 is A2*A2,
  B3 is A3*A3,
  B4 is A4*A4,
  B5 is A5*A5.

myDivL([A1,A2,A3,A4,A5],[B1,B2,B3,B4,B5],[C1,C2,C3,C4,C5]) :-
  C1 is A1/B1,
  C2 is A2/B2,
  C3 is A3/B3,
  C4 is A4/B4,
  C5 is A5/B5.


canGetNobles(Bonusses, NobleGets, Nobles) :-
  findall(
  NId-X-C, 
  (
    member(NId-X-C,Nobles),
    subtractGems(Bonusses, X, BonusGemRemain),
    splendor:minGem(BonusGemRemain, MinC),
    MinC >= 0
  ),
  NobleGets
).


  % LIST OPS
insert_at(E,L,1,[E|L]):-!.
insert_at(E,[H|T],Num,[H|L2]):-
  Num > 1,
  NewNum is Num-1,
  insert_at(E,T,NewNum,L2).

remove_at(H,[H|T],1,T):-!.
remove_at(E,[H|T],Num,[H|L2]):-
  Num > 1,
  NewNum is Num - 1,
  remove_at(E,T,NewNum,L2).

maximum_list(L, M, I) :- nth1(I, L, M), \+ (member(E, L), E > M),!.
