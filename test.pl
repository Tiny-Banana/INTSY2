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

/*Initialization*/

/* RESPIRATORY DISEASES */
tuberculosis(['have night sweats', 'have shortness of breath', 'have low grade fever (<38.8C) for more than a week', 'have fatigue', 
              'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum', 'have chest pain',
              'have sudden weight loss', 'have a history of tuberculosis', 
              'have a history of exposure to a tuberculosis positive individual recently']).

pneumonia(['have shortness of breath', 'have fatigue', 'have high grade fever (>=38.8C)', 'have chest pain', 
            'experience confusion', 'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum']).

bronchitis(['have shortness of breath', 'have low grade fever (<38.8C)', 'have fatigue', 'have sore throat', 
            'have cold symptoms such as mild headache or body ache', 'have a cold or flu', 'have persistent cough']).

cvd(['have shortess of breath', 'have fast heartbeat', 'have slow heartbeat', 
     'have pain / weakness / numbness in your upper / lower extremities', 'have feelings of loghtheadedness', 'have fatigue', 
     'have swollen limbs', 'have a consistent blood pressure of systolic 130 mm Hg or more, and diastolic 80 mm Hg or more', 
     'have a family history of heart disease', 'have obesity', 'frequently intake alcohol', 'frequently use tobacco', 'exercise / walk regularly'])

/* OTHERS */
dengue(['have gastrointestinal symptoms such as abdominal / belly pain , jaundice and others',
        'have fever with either one of the following: eye pain, muscle pain, bone pain, joint pain, fever, headache, nausea / vomiting, and rash',
        'experience vomiting at least 3 times in 24 hours', 'experience vomiting with blood', 'have bloody stool', 
        'feel tired / restless / irritable'])

hypertension(['have a consistent blood pressure of systolic 130 mm Hg or more, and diastolic 80 mm Hg or more'])

/* DISEASES RELATED TO THE STOMACH / LIVER */
typhoidfever(['have bloody stool', 'experience confusion', 'have an attention deficit', 'experience nosebleeds', 'have severe fatigue', 
              'have high grade fever (>=38.8C)', 'have stomach ache', 'have diarrhea', 'have loss of appetite'])

hepatitisA(['have jaundice', 'have loss of appetite', 'have stomach ache', 'have diarrhea', 'have dark (brown) urine', 'have light colored stool'
            'have fatigue', 'have high grade fever (>=38.8C)'])

leptospirosis(['have high grade fever (>=38.8C)', 'have headaches', 'have muscle aches', 'experience vomiting', 'have fatigue', 'have diarrhea',
               'have rashes', 'have stomach ache', 'have jaundice', 'have red eyes'])

helminthiasis(['have diarrhea', 'have abdominal pain', 'have chronic symptoms of malnutrition', 'have fatigue', 'have rectal prolapse',])

cholera(['have diarrhea', 'experience vomiting', 'have leg cramps', 'experience dehydration', 'experience loss of skin elasticity',
         'have rapid heart rate', 'have a dry mouth', 'experience thirst', 'have fatigue', 'experience restlessness', 
         'have low blood pressure'])


askedsymp([]).
has([]).
