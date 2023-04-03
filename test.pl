:- use_module(library(random)).
/*Marking the predicates as dynamic*/
:- dynamic(askedsymp/1).
:- dynamic(has/1).

/*The start predicate initializes the program and removes past diagnosis history*/
start :-
    retractall(has(_)),
	assert(has([])),
	retractall(askedsymp(_)),
	assert(askedsymp([])),
    format("Bot: Greetings! I am a ChatBot that diagnoses diseases!~n"),
    format("Bot: Press y. for yes, n. for no, and q. to quit...~n"),
    ask_symptom('have fatigue').

/*The ask_symptom predicate asks symptoms from the user. According to patient's answer, the program will either abort or enter affirm/negate predicates*/
ask_symptom(X) :- 
    format("Bot: Do you ~w? ~n", [X]),
    read(Symptom),
    askedsymp(Asked), append(Asked, [X], AskedX), retract(askedsymp(Asked)), asserta(askedsymp(AskedX)), 
    (Symptom == q -> abort;
     Symptom == y -> affirm(X);
     Symptom == n -> negate).

/*The affirm predicate adds the confirmed symptom, X, to has(). Based on the confirmed symptoms, a related symptom will be asked. If all of the symptoms 
  on a given disease has ran out, enter diagnose predicate*/
affirm(X) :- 
    has(Symptoms1), append(Symptoms1, [X], Symptoms1X), retract(has(Symptoms1)), asserta(has(Symptoms1X)),
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V), dengue(E), typhoidfever(Y),
    hepatitisA(I), leptospirosis(U), helminthiasis(S), cholera(O), has(Symptoms), 
    askedsymp(Asked), most_similar_list(Symptoms, [H, P, T, V, E, Y, I, U, S, O], MostSimilar, _),
    findall(Symptom, (member(Symptom, MostSimilar), \+member(Symptom, Asked)), RelatedSymptoms),
    (RelatedSymptoms == [] -> diagnose;
    (random_member(Random, RelatedSymptoms), ask_symptom(Random))).

/*Depending on the threshold set on disease similarity rate, the negate predicate asks either a related symptom or a random symptom from all of the diseases. 
  If all of the symptoms on a given disease has ran out, enter diagnose predicate*/
negate :-
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V), dengue(E), typhoidfever(Y),
    hepatitisA(I), leptospirosis(U), helminthiasis(S), cholera(O), has(Symptoms),
    most_similar_list(Symptoms, [H, P, T, V, E, Y, I, U, S, O], MostSimilar, MaxRate), 
    similar(MaxRate, 0.3, MostSimilar, RelatedSymptoms),
    (RelatedSymptoms == [] -> diagnose;
    (random_member(Random, RelatedSymptoms), ask_symptom(Random))).

/*The similarity_rate predicate computes the similarity rate between two lists based on the number of common elements*/
similarity_rate(List1, List2, Rate) :-
    intersection(List1, List2, Intersection),
    union(List1, List2, Union),
    length(Intersection, IntersectionLength),
    length(Union, UnionLength),
    Rate is IntersectionLength / UnionLength.

/*The similarity_rates predicate takes a list and a list of lists as input and computes the similarity rate between the 
  input list and each list in the list of lists*/
similarity_rates(List, Lists, Rates) :-
    findall(Rate, (member(List2, Lists), similarity_rate(List, List2, Rate)), Rates).

/*The most_similar_list predicate takes a list and a list of lists as input and returns the list in the list of lists that has the highest similarity rate*/
most_similar_list(List, Lists, MostSimilar, MaxRate) :-
    similarity_rates(List, Lists, Rates),
    max_list(Rates, MaxRate),
    nth0(Index, Rates, MaxRate),
    nth0(Index, Lists, MostSimilar).

/*If the max rate is greater than or equal to the given threshold, the similar predicate returns related symptoms based on the confirmed symptoms, excluding 
  the symptoms that were already asked*/
similar(MaxRate, Threshold, MostSimilar, RelatedSymptoms) :-
    MaxRate >= Threshold, 
    askedsymp(Asked), 
    findall(Symptom, (member(Symptom, MostSimilar), \+member(Symptom, Asked)), RelatedSymptoms).

/*If the max rate is less than to the given threshold, the similar predicate returns the combination of the symptoms of all the diseases, excluding 
  the symptoms that were already asked*/
similar(MaxRate, Threshold, _, RelatedSymptoms) :-
    MaxRate < Threshold,
    bronchitis(H), pneumonia(P), tuberculosis(T), cvd(V), dengue(E), typhoidfever(Y),
    hepatitisA(I), leptospirosis(U), helminthiasis(S), cholera(O),
    union(H, P, HP), union(HP, T, HPT), union(HPT, V, HPTV), union(HPTV, E, HPTVE),
    union(HPTVE, Y, HPTVEY), union(HPTVEY, I, HPTVEYI), union(HPTVEYI, U, HPTVEYIU),
    union(HPTVEYIU, S, HPTVEYIUS), union(HPTVEYIUS, O, HPTVEYIUSO), askedsymp(Asked), 
    findall(Symptom, (member(Symptom, HPTVEYIUSO), \+member(Symptom, Asked)), RelatedSymptoms).

/*The diagnose predicate diagnoses the user based on the given diseases*/
diagnose :-
    write("We have formed a diagnosis from your answers. Based on our analysis, "),
    (diagnose_tuberculosis;
     diagnose_pneumonia;
     diagnose_bronchitis; 
     diagnose_cvd;
     diagnose_dengue;            
     diagnose_typhoidfever;
     diagnose_hepatitisA;
     diagnose_leptospirosis;
     diagnose_helminthiasis;
     diagnose_cholera; 
     format("we still lack the information to form a conclusion.~n")),
    abort().

/*The following predicates compare the symptoms in has() and the symptoms on a certain disease. If the confirmed symptoms matched a ceratain disease
  at a 90% similarity rate, a diagnosis message will be printed*/
diagnose_tuberculosis :-
    has(Symptom), tuberculosis(T), intersection(Symptom, T, Intersection), length(Intersection, IntersectionLength), length(T, TLength),
    (IntersectionLength >= 90 * TLength // 100 -> format("you might have tuberculosis.~n")).

diagnose_pneumonia :- 
    has(Symptom), pneumonia(P), intersection(Symptom, P, Intersection), length(Intersection, IntersectionLength), length(P, PLength),
    (IntersectionLength >= 90 * PLength // 100 -> format("you might have pneumonia.~n")).

diagnose_bronchitis :-
    has(Symptom), bronchitis(H), intersection(Symptom, H, Intersection), length(Intersection, IntersectionLength), length(H, HLength),
    (IntersectionLength >= 90 * HLength // 100 -> format("you might have bronchitis.~n")).

diagnose_cvd:-
    has(Symptom), cvd(V), intersection(Symptom, V, Intersection), length(Intersection, IntersectionLength), length(V, VLength),
    (IntersectionLength >= 90 * VLength // 100 -> format("you might have cardiovascular disease.~n")).

diagnose_dengue:-
    has(Symptom), dengue(E), intersection(Symptom, E, Intersection), length(Intersection, IntersectionLength), length(E, ELength),
    (IntersectionLength >= 90 * ELength // 100 -> format("you might have dengue.~n")).

diagnose_typhoidfever:-
    has(Symptom), typhoidfever(Y), intersection(Symptom, Y, Intersection), length(Intersection, IntersectionLength), length(Y, YLength),
    (IntersectionLength >= 90 * YLength // 100 -> format("you might have typhoid fever.~n")).

diagnose_hepatitisA:-
    has(Symptom), hepatitisA(I), intersection(Symptom, I, Intersection), length(Intersection, IntersectionLength), length(I, ILength),
    (IntersectionLength >= 90 * ILength // 100 -> format("you might have hepatitis A.~n")).

diagnose_leptospirosis:-
    has(Symptom), leptospirosis(U), intersection(Symptom, U, Intersection), length(Intersection, IntersectionLength), length(U, ULength),
    (IntersectionLength >= 90 * ULength // 100 -> format("you might have leptospirosis.~n")).

diagnose_helminthiasis:-
    has(Symptom), helminthiasis(S), intersection(Symptom, S, Intersection), length(Intersection, IntersectionLength), length(S, SLength),
    (IntersectionLength >= 90 * SLength // 100 -> format("you might have helminthiasis.~n")).

diagnose_cholera:-
    has(Symptom), cholera(O), intersection(Symptom, O, Intersection), length(Intersection, IntersectionLength), length(O, OLength),
    (IntersectionLength >= 90 * OLength // 100 -> format("you might have cholera.~n")).

/*Facts about the diseases*/
tuberculosis(['have night sweats', 'have shortness of breath', 'have low grade fever (<38.8C) for more than a week', 'have fatigue', 
              'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum', 'have chest pain',
              'have sudden weight loss', 'have a history of tuberculosis', 
              'have a history of exposure to a tuberculosis positive individual recently']).

pneumonia(['have shortness of breath', 'have fatigue', 'have high grade fever (>=38.8C)', 'have chest pain', 
            'experience confusion', 'have persistent cough with yellow or green sputum for more than 2 weeks', 'have blood in sputum']).

bronchitis(['have shortness of breath', 'have low grade fever (<38.8C)', 'have fatigue', 'have sore throat', 
            'have cold symptoms such as mild headache or body ache', 'have a cold or flu', 'have persistent cough']).

cvd(['have shortness of breath', 'have irregularly slow or fast heartbeat', 'have pain / weakness / numbness in your upper / lower extremities', 
     'have feelings of lightheadedness', 'have fatigue', 'have swollen limbs', 
     'have a consistent blood pressure of systolic 130 mm Hg or more, and diastolic 80 mm Hg or more', 
     'have a family history of heart disease', 'have obesity', 'frequently intake alcohol', 'frequently use tobacco', 'not exercise / walk regularly']).

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

/*Initialization*/
askedsymp([]).
has([]).
