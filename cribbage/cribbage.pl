/* 
Author: David Naughton
Login ID: dna@student.unimelb.edu.au
Subject: Declarative Programming (COMP30020_2020_SM2) - Project 1 (Cribbage)

Purpose: Perform two keys tasks for a player in a game of Cribbage. To score
a hand according to the rules and to select the best 4 card hand from the 
cards a player is dealt.

The hand value score is determined by breaking down the scoring into
constituent sub scores including score for pairs, score for a flush, score
for runs, score for fifteens and score for one for a his nob. We then sum the
sub scores to get the total score.

The select hand predicate is selected by considering all possible hands from
the dealt cards and all possible start cards. We score each possible 
combination of hand and startcard by brute force and then choose the hand 
which has the best total score over all start cards.

The two keys tasks of scoring a hand and selecting the best hand from dealt
cards make some assumptions about the input. We assume the rules of cribbage
as provided in the assignment specification such as a cribbage hand
consists of 4 cards etc. We assume each input card is valid 
(e.g. no 'queen of sapphires'). We also assume that the cribcards returned by
'select_hand' are all cards that are dealt but not selected in the best hand.
*/

/******************************* HAND VALUE ******************************/

%% hand_value(+Hand, +Startcard, ?Value)
%
% this predicate takes a hand and startcard and returns the score 
% according to the rules of cribbage. 
hand_value(Hand, Startcard, Value):-
    oneForHisNob(Hand, Startcard, OneForHisNobValue),
    flushScore(Hand, Startcard, FlushValue),

    % remaining predicates do not distinguish startcard from rest of hand
    append([Startcard], Hand, HandWithStartcard),
    fifteenScore(HandWithStartcard, 15, FifteenScore),
    
    % convert card ranks in hand to unique integer list
    maplist(rankToUnique, HandWithStartcard, HWSIntegers),
    runScore(HWSIntegers, RunScore),
    pairScore(HWSIntegers, PairValue),

    Value is OneForHisNobValue + FlushValue + 
        FifteenScore + RunScore + PairValue.

%% oneForHisNob(+Hand, +Startcard, ?Score)
% 
% checks the hand for a jack of same suit as the start card
oneForHisNob([], _, 0).
oneForHisNob([card(Rank, Suit)|Tail],card(_, StartCardSuit), Score):-
    (Rank=jack, Suit=StartCardSuit ->
        Score is 1
    ; 
        oneForHisNob(Tail, card(_, StartCardSuit), Score)
    ).

%% flushScore(+Hand, +Startcard, ?Score)
%
% scores the hands for flush points
flushScore([card(_, Suit)|Tail], card(_, StartcardSuit), Score):-
    % convert tail cards to list of suits
    maplist(getSuitOfCard, Tail, Suits),
    % check frequency of first card's suit in rest of hand
    occurrences_of_term(Suit, Suits, Frequency),
    
    hand(CardsInHand),
    (Frequency is CardsInHand - 1 ->
        % if Startcard also has the same suit, award extra point
        (Suit = StartcardSuit -> 
            Score is CardsInHand + 1
        ; 
            Score = CardsInHand
        )
    ; 
        Score = 0
    ).

%% fifteenScore(+Hand, +Sum, ?Score)
%
% calculate points for sum combinations equal to the sum target (i.e. 15)
fifteenScore(Hand, Sum, Score):-
    % Converts card ranks to integers which cutoff at 10
    maplist(rankToCutoff, Hand, Integers),
    % generates all subsets of the hand card integers
    bagof(Subsets, subset(Integers, Subsets), SubsetList),

    % sum each subset and store in list
    maplist(sum_list, SubsetList, SumList),
    % find how many subset sums equal the 'Sum' (ie. 15)
    occurrences_of_term(Sum, SumList, Combinations),
    % each combination is worth two points
    Score is Combinations*2.                                   

%% runScore(+HandIntegers, ?RunScore)
%
% get score of runs in a hand
runScore(HandIntegers, RunScore):-
    % sort and remove dupes
    sort(HandIntegers, SortedList),
    % get longest consecutive sublist
    consecutiveCount(SortedList, Run),
    length(Run, RunLength),
    hand(CardsInHand),

    % run must be at least length 3
    (RunLength >= CardsInHand - 1 ->
        duplicate(HandIntegers, RunLength, IntegersArray),
        % get frequencies of each card in the run
        maplist(occurrences_of_term, Run, IntegersArray, Frequency),
        % multiply them all together
        foldl(multiply, Frequency, 1, Multiplier),
        % product of multiplier and the length of run is the score.
        RunScore is Multiplier*RunLength
    ; 
        RunScore = 0
    ).
    
%% consecutiveCount(+HandIntegers, ?Result)
%
% takes sorted, unique list and returns longest consecutive run > 2
consecutiveCount([Head|Tail], Result):-
    consecutiveCount(Tail, Head, 1, [Head], [], Result).

%% consecutiveCount(+Tail, +Head, +RunLength, +Run, +UpdatedRun, ?Result)
consecutiveCount([], _, _, _, Result, Result).
consecutiveCount([Next|Tail], Head, RunLength, Run, LongestRun, Result):- 
    (Next is Head + 1 ->
        UpdatedRunLength is RunLength + 1,
        hand(CardsInHand),

        % if we reach a run of 3 or more in a five card hand it 
        % must be the longest run possible, so unify with LongestRun
        % in either case add 'Next' to Run list, order not important
        (UpdatedRunLength >= CardsInHand - 1 -> 
            consecutiveCount(Tail, Next, UpdatedRunLength, [Next|Run], [Next|Run], Result)
        ; 
            consecutiveCount(Tail, Next, UpdatedRunLength, [Next|Run], LongestRun, Result)
        ) 
    ; 
        % next item is not consecutive, reset run
        consecutiveCount(Tail, Next, 1, [Next], LongestRun, Result)
    ).    

%% pairScore(+HandIntegers, ?Score)
%
% find all possible pair combinations by recursively searching for head in
% tail and then removing head from HandIntegers at each step. 
% E.g. [2,1,1,3,1] will add 0 + 2*2 + 2*1 + 0 + 0 = 6  
pairScore(HandIntegers, Score):-
    pairScore(HandIntegers, 0, Score).

%% pairScore(+HandIntegers, +Acc, ?Score)
pairScore([], Score, Score).
pairScore([Head|Tail], Acc, Score):-
    % find # of occurences of head card rank in the tail
    occurrences_of_term(Head, Tail, Frequency),
    % 2 points for each pair are added to subtotal
    UpdatedAcc is Acc+2*Frequency, 
    pairScore(Tail, UpdatedAcc, Score).
    

/******************************* SELECT HAND ******************************/

% select_hand(+Cards, ?Hand, ?Cribcards)
%
% takes a list of cards, selects the best 4 card hand, sends the rest
% to crib.
select_hand(Cards, Hand, Cribcards):- 
    % get scores for each possible hand accumulated over all possible 
    % startcards.
    getScores(Cards, Hands, Scores),
    max_list(Scores, Maximum),
    % find the position of the maximum score in the list, then use this to
    % index the Hands list to return the best hand
    indexOf(Scores, Maximum, Position),
    nth0(Position, Hands, Hand),
    subtract(Cards, Hand, Cribcards).

%% getScores(+Cards, ?Hands, ?Scores).
%
% generates all possible startcards and hands and then feeds these into the
% scoreHands predicate.
getScores(Cards, Hands, Scores):-
    % generates a list of all possible startcards
    startCardGenerator(Cards, StartCards),
    % generates a list of all possible hands
    handGenerator(Cards, Hands),
    length(Hands, HandCombinations),
    % initialise AccList as list of zeros with length of possible 
    % hand combinations
    duplicate(0, HandCombinations, AccList),
    % score all possible hand + startcard combinations and return as list
    scoreHands(StartCards, Hands, HandCombinations, AccList, Scores).

%% scoreHands(+Hand, +Hands, +HandCombinations, ?Scores)
%
% will loop through all possible Startcards & accumulate hand value for each
% possible hand. This will allow us to compare possible hands to see which
% has the greatest accumulated value. We could average over the number of
% startcards to give expected value but not necessary to find optimal hand.
scoreHands([], _, _, Scores, Scores).
scoreHands([Head|Tail], Hands, HandCombinations, Acc, Scores):- 
    % get list with length of HandCombinations with each element as Startcard
    duplicate(Head, HandCombinations, HeadList),
    % run each possible hand through hand_value with the Startcard and score
    maplist(hand_value, Hands, HeadList, HandScores),
    % accumulate the scores for each possible hand in Acc list
    maplist(plus, Acc, HandScores, UpdatedAcc),    
    scoreHands(Tail, Hands, HandCombinations, UpdatedAcc, Scores).

%% handGenerator(+Cards, ?Subsets)
%
% takes dealt cards and generates all subsets of length 4
handGenerator(Cards, Subsets):-
    hand(CardsInHand),
    bagof(Sets, (subset(Cards, Sets),
        length(Sets, CardsInHand)), Subsets).

%% startCardGenerator(+Cards, ?List)
%
% gets list of all possible startcards by taking all cards in a deck
% and then removes the cards dealt to player
startCardGenerator(Cards, Startcards):- 
    bagof(card(Integer, Suit), card(Integer, Suit), CardList),
    subtract(CardList, Cards, Startcards).


/*************************** HELPER PREDICATES ****************************/

%% indexOf(+Scores, +Maximum, ?Position)
%
% used to get the first index position of the maximum score.
indexOf(Scores, Maximum, Position):- 
    indexOf(Scores, Maximum, 0, Position).

%% indexOf(+Scores, +Maximum, +Acc, ?Position)
indexOf([], _, _, _).
indexOf([Head|Tail], Maximum, Acc, Position):- 
    (Head \= Maximum ->
        UpdatedAcc is Acc+1,
        indexOf(Tail, Maximum, UpdatedAcc, Position)
    ; 
        Position = Acc
    ). 

%% card(?Rank, ?Suit).
% 
% used to generate cards in a 'deck'
card(Rank, Suit):-
    suit(Suit),
    (between(2, 10, Rank) 
    ; royal(Rank, _)
    ).

%% multiply(+X, +Y, ?Z)
%
% simple multiplication function used for foldl
multiply(X, Y, Z):-
    Z is X*Y.

%% duplicate(?Input, ?Length, ?List).
%
% convert input to list of N elements
duplicate(Input, Length, List):-
    length(List, Length),
    maplist(=(Input), List).

%% subset(+Set, ?Subsets) 
% 
% generate all sub sets of given set. Leaves choice point at 
% every level of recursion.
subset([], []).
subset([Head|Tail], [Head|Tail_Test]) :- 
    subset(Tail, Tail_Test).
subset([_|Tail], Tail_Test) :- 
    subset(Tail, Tail_Test).  

%% rankToUnique(+Card, ?Unique)
%
% converts a rank to a unique integer
rankToUnique(card(Rank,_), Unique):-
    rankToIntegers(Rank, Unique, _).

%% rankToCutoff(+Card, ?Cutoff)
%
% converts a rank to an integer (cutoff at 10)
rankToCutoff(card(Rank,_), Cutoff):-
    rankToIntegers(Rank, _, Cutoff).    

%% getSuitOfCard(+Card, ?Suit)
%
% returns the suit of given card
getSuitOfCard(card(_, Suit), Suit).

/****************************** FACTS *************************************/

%% hand(?CardsInHand)
%
% 4 cards in a hand, gives context to numbers used in code
hand(4).

%% suit(?Suit)
%
% facts about card suits
suit(hearts).
suit(diamonds).
suit(clubs).
suit(spades).

%% royal(?Suit, ?Value)
%
% facts about royals
royal(jack, 11).
royal(queen, 12).
royal(king, 13).
royal(ace, 1).

%% rankToIntegers(?Rank, ?UniqueInteger, ?CutoffInteger)
%
% facts about mapping ranks to integers, unique values are 
% required to identify runs and count frequencies, however for sum 
% to fifteen we need to assign value of 10 for jack, queen and king.
rankToIntegers(ace, 1, 1).
rankToIntegers(2, 2, 2).
rankToIntegers(3, 3, 3).
rankToIntegers(4, 4, 4).
rankToIntegers(5, 5, 5).
rankToIntegers(6, 6, 6).
rankToIntegers(7, 7, 7).
rankToIntegers(8, 8, 8).
rankToIntegers(9, 9, 9).
rankToIntegers(10, 10, 10).
rankToIntegers(jack, 11, 10).
rankToIntegers(queen, 12, 10).
rankToIntegers(king, 13, 10).
                    