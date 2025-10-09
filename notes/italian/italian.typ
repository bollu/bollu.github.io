#let defn(term) = box(fill: rgb("#EAF2F5"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]
#let todo(term) = box(fill: rgb("#FFCDD2"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]
// material orange: #FFE0B2
#let emph(term) = box(fill: rgb("#FFe0B2"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]

= Basic Italian Grammar (Complete for Beginner-Speaking)

// https://docs.google.com/spreadsheets/d/1fPsEuRvMUclkjSagu3c0dgl7ogw2avZHlOvwYnkQgAI/edit?gid=0#gid=0

== Lesson 1

We learn the basic constructs of #emph[Italian].
Italian is a *positional* language, so positions of words in a sentence matter.
Most basic sentence construction is #defn[ *SVO* : Subject + Verb + Object],
where #defn[*Subject* : (Definite/Indefinite)Article + Noun + Adjective],
and adverbs fit on the verb.


#emph[Nouns have suffixes] that indicate their gender and their number:
#table(columns: (10em, 10em, 10em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Gender*][*Number*][*Noun*],
[Masculine], [Singular], [Ragazzo],
[Feminine],  [Singular], [Ragazza],
[Masculine],  [Plural], [Ragazzi],
[Feminine],  [Plural], [Ragazze],
)


Basic verbs are essere (to be), avere (to have), mangiare (to eat).
Conjugations of essere and avere are:
// TODO: move these tables to be adjacent to each other.
#table(columns: (5em, 7em, 10em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Verb*][*Person*][*Example*],
[Essere], [Io (I)], [sono (am)],
[Essere], [Tu (You)], [sei (are)],
[Essere], [Lui/Lei (He/She)], [è (is)],
[Essere], [Noi (We)], [siamo (are)],
[Essere], [Voi (Y'all)], [siete (are)],
[Essere], [Loro (They)], [sono (are)]
)
#table(columns: (5em, 7em, 10em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Verb*][*Person*][*Example*],
[Avere], [Io (I)], [ho (have)],
[Avere], [Tu (You)], [hai (have)],
[Avere], [Lui/Lei (He/She)], [ha (has)],
[Avere], [Noi (We)], [abbiamo (have)],
[Avere], [Voi (Y'all)], [avete (have)],
[Avere], [Loro (They)], [hanno (have)]
)

== Lesson 2 (Definite Articles)


The general pattern for modifying a noun for marking gender/number.
This is called #emph[conjugation]
#todo[Ask Martina that this is indeed called conjugation.]

#table(columns: (10em, 5em, 5em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Gender*][*Singular*][*Plural*],
[Masculine (il gatto)], [-o], [-i],
[Feminine (la Mente)], [-e], [-i],
[Feminine (la Mano)], [-o], [-i],
[Feminine (la citta)], [-a], [-a],
)

=== Definite articles / Articolo Determinativi (the)

We learn the #emph[definite articles], which are used to refer to specific nouns.
The consstruction depends on gender as well as the first letter of the noun.

#table(columns: (5em, 5em, 15em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Gender*][*Number*][*1st Letter*][*Example Definite Article*],
[Maschile] , [Singolare] , [vocale], [*l'* albero],
[Maschile] , [Plurale] , [vocale] , [*gli* alberi],
[Maschile] , [Singolare] , [consonant (normal)] , [*il* cane],
[Maschile] , [Plurale] , [consonant (normal)] , [*i* cani],
[Maschile] , [Singolare] , [s+cons / z / p + cons / y / x], [*lo* zucchero, lo spazio],
[Maschile] , [Plurale] , [s+consonant/ z/ p+ cons/ x/ y] , [*gli* studenti],
[Femminile] , [Singolare] , [consonant (normal)], [*le* chiesa],
[Femminile] , [Plurale] , [consonant (normal)], [*le* chiese],
[Femminile] , [Singolare] , [vocale] , [*l'* amica],
[Femminile] , [Singolare] , [vocale], [*le* amiche],
)


== Lesson 3 (Indefinite Articles)
// 5 June

We learn #emph[Indefinite articles] (a, an, some) are used to refer to non-specific nouns.
Note that in English, we can only say "a dog", we cannot say "a dogs".
It's the same as in Italian, where we only have indefinite articles for the singular nouns. Thus, the indefinite article depends on the gender of the noun and the first letter of the noun.

#table(columns: (5em, 15em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Gender*][*1st Letter*][*Example Indefinite Article*],
[Maschile] , [vocale], [*un* albero],
[Maschile] , [consonant (normal)] , [*un* cane],
[Maschile] , [s+cons / z / p + cons / y / x], [*uno* zucchero, *uno* spazio],
[Femminile] , [vocale] , [*un'* amica],
[Femminile] ,[consonant (normal)], [*una* chiesa],
)

== Lesson 4 (Conjugation for Regular Verb Forms)
// 14 June

We learn the #emph[conjugation for regular verb forms] for `-are`, `-ere` and `-ire`.

#table(columns: (5em, 10em, 10em, 10em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Pronoun*][*Parl`-are`*][*Cred`-ere`*][*Dorm`-ire`*],
[(Meaning)] , [(to speak)], [(to believe)], [(to sleep)],
[Io] , [parl-o], [cred-o], [dorm-o],
[Tu] , [parl-i] , [cred-i], [dorm-i],
[Lui/Lei] , [parl-#emph[a]], [cred-#emph[e]], [dorm-#emph[e]],
[Noi] , [parl-iamo] , [cred-iamo], [dorm-iamo],
[Voi] ,[parl-#emph[ate]], [cred-#emph[ete]], [dorm-#emph[ite]],
[Loro] ,[parl-ono], [cred-ono], [dorm-ono],
)


== Lesson 5 (Modal Verbs)
// 19 June

A #emph[modal verb] is a verb that is used with another verb to express ability, necessity, permission, or possibility. The most common modal verbs in Italian are
#defn[volere (to want)], #defn[potere (can/to be able to)], and 
#defn[dovere (must/to have to)]. 
#todo[what makes it a modality?]

#table(columns: (5em, 10em, 10em, 10em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Pronoun*][*Vol`-ere`*][*Pot`-ere`*][*Dov`-ere`*],
[(Meaning)] , [(to want)], [(to be able to)], [(to have to)],
[Io] , [voglio], [posso], [devo],
[Tu] , [vuoi] , [puoi], [devi],
[Lui/Lei] , [vuole], [puo], [deve],
[Noi] , [vogliamo] , [possiamo], [dobbiamo],
[Voi] ,[volete], [potete], [dovete],
[Loro] ,[vogiono], [possono], [devono],
)

=== New Words

#defn[andare (to go)], #defn[venire (to come)], #defn[dare (to give)], #defn[fare (to do/make)]


== Lesson 6 (Pronouns) #todo[Ask Martina]
// 24 June

In this lesson, we will learn about #emph[pronouns].
When we think about pronouns, we need to know:
(a) #emph[direct] / #emph[indirect] objects,
(b) #emph[reflexive] pronouns, 
(c) #emph[transitive] verbs and (d) #emph[intransitive] verbs.

=== Reflexive Pronouns

A #emph[reflexive pronoun]
is a pronoun that refers back to the subject of the sentence.
See that the reflexive pronoun must occur with a subject pronoun
(e.g. I eat #emph[myself]).


#table(columns: (15em, 15em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Subject Pronoun*][*Object Pronoun*][*Reflexive Variant*],
[I],  [Me],  [Myself],
[#emph[I] eat an apple], [bear eats #emph[me]], [I eat #emph[myself]],
[you],  [you],  [yourself],
[#emph[you] eat an apple], [bear eats #emph[you]], [you eat #emph[yourself]],
[he], [him], [himself],
[#emph[he] eats an apple], [bear eats #emph[him]], [he eats #emph[himself]],
[it], [it], [itself],
[#emph[it] eats an apple], [bear eats #emph[it]], [it eats #emph[itself]],
[they], [them], [themselves],
[#emph[they] eat an apple], [bear eats #emph[them]], [they eat #emph[themselves]],
)

==== Example Use of Reflexive Verb With Reflexive Pronoun #defn[Svegliarsi (to wake oneself up)]:

#table(columns: (7em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Pronoun*][*Conjugation*],
[Io], [mi sveglio],
[Tu], [ti svegli], 
[Lui/Lei], [si sveglia],
[Noi], [ci svegliamo],
[Voi], [vi svegliate],
[Loro], [si svegliano],
)
=== Direct and Indirect Objects #todo[get a precise defn.]

Note that an indirect object is always preceded by a preposition (e.g. to, for).

+ I #defn[(subject)] read a book #defn[(direct object)].
+ I #defn[(subject)] read a book #defn[(direct object)] to luisa #defn[(indirect object)].

==== Direct Object Pronouns Table

table.header[][*Subject Pronoun*][*Object Direct Pronoun*],
[I], [I], [Me],
[I], [Io], [Mi],
[You], [Tu], [Ti],
[He], [Lui], [Lo],
[She], [Lei], [La],
[We], [Noi], [Ci],
[Y'all], [Voi], [Vi]
[They], [Loro], [Li/Le]
)


=== Transitive and Intransitive Verbs

+ #defn[Intransitive verb] is a verb that #emph[does not] take a direct object.
+ A #defn[Transitive verb] is a verb that #emph[does take] a direct object.

Both transitive and intransitive verbs can sometimes #todo[(is it sometimes? is it always? if it is not always, what is an example of such a verb?)] take an indirect object.

=== Big Table


#table(columns: (7em, 7em, 7em, 7em, 7em, 7em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[][*Subject Pronoun*][*Object Direct Pronoun*][*Reflexive Pronoun*][*Indirect Strong*][*Indirect Weak*],
[I],  [I],  [I], [Myself], [#todo[Me]], [To Me],
[I],  [Io],  [Mi], [Mi], [Mi], [(a) me],
[You],  [You],  [You], [Yourself], [#todo[You]], [To You],
[You],  [Tu],  [Ti], [Ti], [Ti], [(a) te],
[He],  [He],  [Him], [Himself], [#todo[Him]], [To Him],
[He], [Lui], [Lo], [Si], [Si], [(a) lui],
[She], [Lei], [La], [Si], [Si], [(a) lei],
[We], [Noi], [Ci], [Ci], [Ci], [(a) noi],
[Y'all], [Voi], [Vi], [Vi], [Vi], [(a) voi],
[They], [Loro], [Li], [Si], [Si], [(a) loro],
)

In normal convo, always use the weak form of indirect object pronouns.


== Lesson 7 (Question Words)
// September 23


#table(columns: (7em, 15em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*English*][*Italian*][*Example Sentence*],
[When], [Quando], [#todo[sentence]],
[Where], [Dove], [#todo[sentence]],
[Who], [Chi], [#todo[sentence]],
[How], [Come], [#todo[sentence]],
[Why], [Perche], [#todo[sentence]],
[What], [Che cosa], [#todo[sentence]],
[Which], [Quale], [#todo[sentence]],
[How Many], [Quanto], [#todo[sentence]],
)

== Lesson 7.5 (Prepositions)
// TODO

We will drop a big table of prepositions here.
Prepositions in italian can combine with the definite articles to form contractions.

== Interlude: Celebrate!

This suffices our basic grammar for beginner-speaking.
At this point, it is worth it to learn vocabulary and practice speaking.

== Lesson 8 (Simple Future)
// September 23


== Vocabulary

#table(columns: (15em, 15em),
stroke: none,
fill: (_, y) => if calc.odd(y) { rgb("EAF2F5") },
table.header[*Italian*][*English*],
[Qualcuno], [Someone],
[Qualcosa], [Something],
[Questo], [This],
[Quello], [That],
[Qui], [Here],
[Ci], [There],
[Adesso], [Now],
[Quindi], [Then/So],
[Sempre], [Always],
[Mai], [Never],
[Anche], [Also/Too],
[Ma], [But],
[Perche], [Because/Why],
[O], [Or],
[Oggi], [Today],
[Domani], [Tomorrow],
[Ieri], [Yesterday],
[Settimana], [Week],
[Mese], [Month],
[Anno], [Year],
[Tempo], [Time],
[Primavera], [Spring], 
[Estate], [Summer],
[Inverno], [Winter],
[Autunno], [Fall],
[Mattina], [Morning],
[Pomeriggio], [Afternoon],
[Sera], [Evening],
[Notte], [Night],
[Colazione], [Breakfast],
[Pranzo], [Lunch],
[Cena], [Dinner],
[Insieme], [Together],
[Sapere], [To know (a fact)],
[Ricordare], [To remember],
[Capire], [To understand],
[Vedere], [To see],
[Guardare], [To watch/look at],
[Ascoltare], [To listen to],
[Sentire], [To hear/feel],
[Parlare], [To speak/talk],
[Chiamare], [To call],
[Domandare], [To ask],
[Rispondere], [To answer],
[Leggere], [To read],
[Scrivere], [To write],
[Studiare], [To study],
[Imparare], [To learn],
[Insegnare], [To teach],
[Giocare], [To play (a game)],
[Suonare], [To play (an instrument)],
[Viaggiare], [To travel],
[Comprare], [To buy],
[Vendere], [To sell],
[Pagare], [To pay],
[Costare], [To cost],
[Dare], [To give],
[Ricevere], [To receive],
[Prendere], [To take],
[Fare], [To do/make],
[Andare], [To go],
[Venire], [To come],
[Stare], [To stay/be],
[Mettere], [To put/place],
[Trovare], [To find],
[Lasciare], [To leave (something/someone)],
[Portare], [To bring/carry],
[Svegliarsi], [To wake oneself up],
[Alzarsi], [To get oneself up (out of bed)],
[Diversi], [To have fun/enjoy oneself],
[Chiedere], [To ask (a question)],
[Rispondere], [To answer (a question)],
[Come], [How],
[Quando], [When],
[Dove], [Where],
[Chi], [Who],
[Che cosa], [What],
[Quale], [Which],
[Perche], [Why],
[Triste], [Sad],
[Felice], [Happy],
[Stanco], [Tired],
[Malato], [Sick],
[Dormo], [Sleepy],
)
