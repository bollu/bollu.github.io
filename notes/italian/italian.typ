#let defn(term) = box(fill: rgb("#EAF2F5"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]
#let todo(term) = box(fill: rgb("#FFCDD2"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]
// material orange: #FFE0B2
#let emph(term) = box(fill: rgb("#FFe0B2"), inset: 0.5pt, outset: 2pt, radius: 4pt)[~#term]

= Basic Italian Grammar (Complete for Beginner-Speaking)

// https://docs.google.com/spreadsheets/d/1fPsEuRvMUclkjSagu3c0dgl7ogw2avZHlOvwYnkQgAI/edit?gid=0#gid=0

== Lesson 1

Italian is a *positional* language, so positions of words in a sentence matter.
Most basic sentence construction is #defn[ *SVO* : Subject + Verb + Object],
where #defn[*Subject* : Article + Noun + Adjective].

Nouns have suffixes that indicate their gender and their number:
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

== Lesson 2 


The general pattern for modifying a noun for marking gender/number is:
#todo[what is this called?]

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

The definite article is used to refer to specific nouns.
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


== Lesson 3
// 5 June

Indefinite articles (a, an, some) are used to refer to non-specific nouns.
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

== Lesson 4

We learn the verb forms for `-are`, `-ere` and `-ire`.

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
