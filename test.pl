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
    askedsymp(A), append(A, [X], AX), retract(askedsymp(A)), asserta(askedsymp(AX)), 
    (Symptom == q -> abort;
     Symptom == y -> yes(X);
     Symptom == n -> negate).
 
yes(X) :- 
    has(A), append(A, [X], AX), retract(has(A)), asserta(has(AX)),
     /*query lahat ng related kay X and get the facts na natanong na and check if ung related kay X ay nandoon na ba sa facts 
      na natanong na,, kasi if nandoon na, hindi na iistore sa list R*/
    findall(C, (related(X, C), askedsymp(D), \+member(C, D)), R), 
    (R == [] -> diagnose;
     random_member(M, R)),
    asksymptom(M).

negate :-
    bronchitis(H), pneumonia(P), tubercolosis(T),
    union(H, P, HP), union(HP, T, HPT),
    askedsymp(D),
    /* query all members ng aggregattedlist and check if each member is part na ng natanong na facts,,
      if hindi pa, store sa list R
    */
    findall(C, (member(C, HPT), \+member(C, D)), R),
    (R == [] -> diagnose;
     random_member(M, R)),
    asksymptom(M).

diagnose :-
    write("We have completed your diagnosis chuchu. Based on our analysis, "),
    (diagnosebronchitis; 
     diagnosepneumonia;
     diagnosetubercolis;
     format("we still lack the information to form a conclusion.~n")),
    abort().

diagnosebronchitis :-
    has(A), bronchitis(H), intersection(A, H, R), length(R, L), length(H, L2),
    (L >= 80 * L2 // 100 -> format("you might have bronchitis.~n")).

diagnosepneumonia :- 
    has(A), pneumonia(P), intersection(A, P, R), length(R, L), length(P, L2),
    (L >= 80 * L2 // 100 -> format("you might have pneumonia.~n")).

diagnosetubercolis :-
    has(A), tubercolosis(T), intersection(A, T, R), length(R, L), length(T, L2),
    (L >= 80 * L2 // 100 -> format("you might have tubercolosis.~n")).

related(A, B) :-
    (bronchitis(H), member(A, H), member(B, H));
    (pneumonia(P), member(A, P), member(B, P));
    (tubercolosis(T), member(A, T), member(B, T)).

/*Initalization*/
bronchitis(['have shortness of breath', 'persitent cough', 'wheeze', 'have chest pain', 'have fatigue', 
            'have cold/flu in the past', 'have low grade fever']).
pneumonia(['have blood in sputum', 'have high grade fever', 'have yellow/green sputum', 'have fatigue']).
tubercolosis(['have low grade fever', 'have blood in sputum', 'have fatigue', 'have yellow/green sputum', 
              'have sudden weight loss', 'have tubercolosis in the past', 'have exposure to a tubercolosis positive individual']).
askedsymp([]).
has([]).
