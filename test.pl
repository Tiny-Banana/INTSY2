:- use_module(library(random)).
:- dynamic(askedsymp/1).
:- dynamic(has/1).

start :-
    retractall(has(_)),
	assert(has([])),
	retractall(askedsymp(_)),
	assert(askedsymp([])),
    format("Bot: Greetings! I am a ChatBot that diagnoses diseases!~n"),
    format("Bot: Press y for yes, n for no, and q to quit...~n"),
    asksymptom('have fatigue').

asksymptom(X) :- 
    format("Bot: Do you ~w? ~n", [X]),
    read(Symptom),
    askedsymp(Asked), append(Asked, [X], AskedX), retract(askedsymp(Asked)), asserta(askedsymp(AskedX)), 
    (Symptom == q -> abort;
     Symptom == y -> yes(X);
     Symptom == n -> negate).

yes(X) :- 
    has(Symptoms1), append(Symptoms1, [X], Symptoms1X), retract(has(Symptoms1)), asserta(has(Symptoms1X)),
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V),
    has(Symptoms), askedsymp(Asked), most_similar_list(Symptoms, [H, P, T, V], MostSimilar),
    findall(Symptom, (member(Symptom, MostSimilar), \+member(Symptom, Asked)), RelatedSymptoms),
    (RelatedSymptoms == [] -> diagnose;
     random_member(Random, RelatedSymptoms)),
    asksymptom(Random).

negate :-
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V),
    has(Symptoms),
    most_similar_list(Symptoms, [H, P, T, V], MostSimilar), 
    similarity_rate(Symptoms, MostSimilar, MaxRate),
    similar(MaxRate, 0.3, MostSimilar, RelatedSymptoms),
    (RelatedSymptoms == [] -> diagnose;
     random_member(Random, RelatedSymptoms)),
    asksymptom(Random).

similarity_rate(List1, List2, Rate) :-
    intersection(List1, List2, Intersection),
    union(List1, List2, Union),
    length(Intersection, IntersectionLength),
    length(Union, UnionLength),
    Rate is IntersectionLength / UnionLength.

similarity_rates(List, Lists, Rates) :-
    findall(Rate, (member(List2, Lists), similarity_rate(List, List2, Rate)), Rates).

most_similar_list(List, Lists, MostSimilar) :-
    similarity_rates(List, Lists, Rates),
    max_list(Rates, MaxRate),
    nth0(Index, Rates, MaxRate),
    nth0(Index, Lists, MostSimilar).

similar(MaxRate, Threshold, MostSimilar, RelatedSymptoms) :-
    MaxRate >= Threshold, 
    askedsymp(Asked), 
    findall(Symptom, (member(Symptom, MostSimilar), \+member(Symptom, Asked)), RelatedSymptoms).

similar(MaxRate, Threshold, _, RelatedSymptoms) :-
    MaxRate < Threshold,
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V),dengue(E), typhoidfever(Y),
    hepatitisA(I), leptospirosis(U), helminthiasis(S), cholera(O),
    union(H, P, HP), union(HP, T, HPT), union(HPT, V, HPTV), union(HPTV, E, HPTVE),
    union(HPTVE, Y, HPTVEY), union(HPTVEY, I, HPTVEYI), union(HPTVEYI, U, HPTVEYIU),
    union(HPTVEYIU, S, HPTVEYIUS), union(HPTVEYIUS, O, HPTVEYIUSO), askedsymp(Asked), 
    findall(Symptom, (member(Symptom, HPTVEYIUSO), \+member(Symptom, Asked)), RelatedSymptoms).

diagnose :-
    write("We have formed a diagnosis from your answers. Based on our analysis, "),
    (diagnosetuberculosis;
     diagnosepneumonia;
     diagnosebronchitis; 
     diagnosecvd;
     diagnosedengue;            
     diagnosetyphoidfever;
     diagnosehepatitisA;
     diagnoseleptospirosis;
     diagnosehelminthiasis;
     diagnosecholera; 
     format("we still lack the information to form a conclusion.~n")),
    abort().

diagnosetuberculosis :-
    has(Symptom), tuberculosis(T), intersection(Symptom, T, Intersection), length(Intersection, IntersectionLength), length(T, TLength),
    (IntersectionLength >= 90 * TLength // 100 -> format("you might have tuberculosis.~n")).

diagnosepneumonia :- 
    has(Symptom), pneumonia(P), intersection(Symptom, P, Intersection), length(Intersection, IntersectionLength), length(P, PLength),
    (IntersectionLength >= 90 * PLength // 100 -> format("you might have pneumonia.~n")).

diagnosebronchitis :-
    has(Symptom), bronchitis(H), intersection(Symptom, H, Intersection), length(Intersection, IntersectionLength), length(H, HLength),
    (IntersectionLength >= 90 * HLength // 100 -> format("you might have bronchitis.~n")).

diagnosecvd:-
    has(Symptom), cvd(V), intersection(Symptom, V, Intersection), length(Intersection, IntersectionLength), length(V, VLength),
    (IntersectionLength >= 90 * VLength // 100 -> format("you might have cardiovascular disease.~n")).

diagnosedengue:-
    has(Symptom), dengue(E), intersection(Symptom, E, Intersection), length(Intersection, IntersectionLength), length(E, ELength),
    (IntersectionLength >= 90 * ELength // 100 -> format("you might have dengue.~n")).

diagnosetyphoidfever:-
    has(Symptom), typhoidfever(Y), intersection(Symptom, Y, Intersection), length(Intersection, IntersectionLength), length(Y, YLength),
    (IntersectionLength >= 90 * YLength // 100 -> format("you might have typhoid fever.~n")).

diagnosehepatitisA:-
    has(Symptom), hepatitisA(I), intersection(Symptom, I, Intersection), length(Intersection, IntersectionLength), length(I, ILength),
    (IntersectionLength >= 90 * ILength // 100 -> format("you might have hepatitis A.~n")).

diagnoseleptospirosis:-
    has(Symptom), leptospirosis(U), intersection(Symptom, U, Intersection), length(Intersection, IntersectionLength), length(U, ULength),
    (IntersectionLength >= 90 * ULength // 100 -> format("you might have leptospirosis.~n")).

diagnosehelminthiasis:-
    has(Symptom), helminthiasis(S), intersection(Symptom, S, Intersection), length(Intersection, IntersectionLength), length(S, SLength),
    (IntersectionLength >= 90 * SLength // 100 -> format("you might have helminthiasis.~n")).

diagnosecholera:-
    has(Symptom), cholera(O), intersection(Symptom, O, Intersection), length(Intersection, IntersectionLength), length(O, OLength),
    (IntersectionLength >= 90 * OLength // 100 -> format("you might have cholera.~n")).

tuberculosis(['have night sweats', 'have shortness of breath', 'have low grade fever (<38.8C) for more than a week', 'have fatigue', 
              'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum', 'have chest pain',
              'have sudden weight loss', 'have a history of tuberculosis', 
              'have a history of exposure to a tuberculosis positive individual recently']).

pneumonia(['have shortness of breath', 'have fatigue', 'have high grade fever (>=38.8C)', 'have chest pain', 
            'experience confusion', 'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum']).

bronchitis(['have shortness of breath', 'have low grade fever (<38.8C)', 'have fatigue', 'have sore throat', 
            'have cold symptoms such as mild headache or body ache', 'have a cold or flu', 'have persistent cough']).

cvd(['have shortness of breath', 'have fast heartbeat', 'have slow heartbeat', 
     'have pain / weakness / numbness in your upper / lower extremities', 'have feelings of lightheadedness', 'have fatigue', 
     'have swollen limbs', 'have a consistent blood pressure of systolic 130 mm Hg or more, and diastolic 80 mm Hg or more', 
     'have a family history of heart disease', 'have obesity', 'frequently intake alcohol', 'frequently use tobacco', 'exercise / walk regularly']).

dengue(['have gastrointestinal symptoms such as abdominal / belly pain, jaundice and others',
        'have fever with either one of the following: eye pain, muscle pain, bone pain, joint pain, fever, headache, nausea / vomiting, and rash',
        'experience vomiting at least 3 times in 24 hours', 'experience vomiting with blood', 'have bloody stool', 
        'feel tired / restless / irritable']).

typhoidfever(['have bloody stool', 'experience confusion', 'have an attention deficit', 'experience nosebleeds', 'have severe fatigue', 
              'have high grade fever (>=38.8C)', 'have stomach ache', 'have diarrhea', 'have loss of appetite']).

hepatitisA(['have jaundice', 'have loss of appetite', 'have stomach ache', 'have diarrhea', 'have dark (brown) urine', 'have light colored stool',
            'have fatigue', 'have high grade fever (>=38.8C)']).

leptospirosis(['have high grade fever (>=38.8C)', 'have headaches', 'have muscle aches', 'experience vomiting', 'have fatigue', 'have diarrhea',
               'have rashes', 'have stomach ache', 'have jaundice', 'have red eyes']).

helminthiasis(['have diarrhea', 'have abdominal pain', 'have chronic symptoms of malnutrition', 'have fatigue', 'have rectal prolapse']).

cholera(['have diarrhea', 'experience vomiting', 'have leg cramps', 'experience dehydration', 'experience loss of skin elasticity',
         'have rapid heart rate', 'have a dry mouth', 'experience thirst', 'have fatigue', 'experience restlessness', 
         'have low blood pressure']).

askedsymp([]).
has([]).
