:- use_module(library(random)).
:- dynamic(askedsymp/1).
:- dynamic(has/1).

start :-
    retractall(has(_)),
	assert(has([])),
	retractall(askedsymp(_)),
	assert(askedsymp([])),
    format("Start~n"), 
    format("Welcome to ... press y n q to ...~n"),
    asksymptom('have fatigue').

asksymptom(X) :- 
    format("Bot: Do you ~w? ~n", [X]),
    read(Symptom),
    (Symptom == q -> abort;
     Symptom == y -> yes(X);
     Symptom == n -> negate(X)).
 
yes(X) :- 
    has(A), append(A, [X], AX), retract(has(A)), asserta(has(AX)),
    askedsymp(B), append(B, [X], BX), retract(askedsymp(B)), asserta(askedsymp(BX)), 
     /*query lahat ng related kay X and get the facts na natanong na and check if ung related kay X ay nandoon na ba sa facts 
      na natanong na,, kasi if nandoon na, hindi na iistore sa list R*/
    findall(C, (relatedCluster(X, C), askedsymp(D), \+member(C, D)), R), 
    (R == [] -> diagnose;
     random_member(M, R)),
    asksymptom(M).

negate(X) :-
    askedsymp(B), append(B, [X], BX), retract(askedsymp(B)), asserta(askedsymp(BX)), 
    bronchitis(H),
    askedsymp(D),
    /* query all members ng aggregattedlist and check if each member is part na ng natanong na facts,,
      if hindi pa, store sa list R
    */
    findall(C, (member(C, H), \+member(C, D)), R),
    (R == [] -> diagnose;
     random_member(M, R)),
    asksymptom(M).

diagnose :-
    write("We have completed your diagnosis chuchu. Based on our analysis, "),
    (diagnosebronchitis; 
    format("we still lack the information to form a conclusion.~n")),
    abort().

diagnosebronchitis :-
    has(A), bronchitis(H), intersection(A, H, R), length(R, L), length(H, L2),
    (L >= 80 * L2 // 100 -> format("you might have bronchitis.~n")).


relatedCluster(A, B) :-
    respiratory(R), member(A, R), member(B, R).


bronchitis(['have cold in the past', 'have flu in the past']).
pneumonia(['have blood in sputum', 'have high grade fever', 'have yellow/green sputum']).

/*kailangan ubusin muna to sa related */
respiratory(['have fatigue', 'have low grade fever', 'have shortness of breath', 'persitent cough', 'wheeze', 'have chest pain']).


askedsymp([]).
has([]).


