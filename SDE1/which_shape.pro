/* Jonathan Ayala */
/* CPSC 3520 */
/* SDE 1 */
/* 2-27-19 */



                   /* 3.1 */
                /* uA/3 (+) */
/* Parses string for sequence of "u", and, if succeeds,*/
        /* indicates length (number of "u").*/

uA(0) --> [].
uA(VAR) --> ["u"], uA(NUM), {VAR is NUM + 1}.

                   /* 3.2 */
           /* rA/3, dA/3, lA/3 (+) */
/* Same structure as uA, but unifies with strings */
      /* of "r","d" and "a" respectively.*/

rA(0) --> [].
rA(VAR) --> ["r"], rA(NUM), {VAR is NUM + 1}.

dA(0) --> [].
dA(VAR) --> ["d"], dA(NUM), {VAR is NUM + 1}.

lA(0) --> [].
lA(VAR) --> ["l"], lA(NUM), {VAR is NUM + 1}.

                   /* 3.3 */
                /* sq/2 (+) */
/* This predicate is included just to point out that without */
/* contextual constraints (e.g., all sides of the same length),*/
    /* the desired class is different. See the examples.*/

sq --> [].
sq --> up,rt,dw,lf.
up --> uA(L),{L > 0}.
rt --> rA(L),{L > 0}.
dw --> dA(L),{L > 0}.
lf --> lA(L),{L > 0}.

                  /* 3.4 */
                /* sqA/2 (+) */
  /* like sq, but the length of each side must be equal.*/

sqA --> [].
sqA --> uA(SQ), rA(SQ), dA(SQ), lA(SQ).

                  /* 3.5 */
              /* rectA/2 (+) */
  /* Like sqA, but only parallel sides must have equal length. */

rctA --> [].
rctA --> uA(UD), rA(LR), dA(UD), lA(LR).

                  /* 3.6 */
                /* grect/3 */
/* This predicate generates a rectangle in the PDL.*/

grect(0,0,0).
grect(UD,LR,OUT) :-
    UD > 0,
    LR > 0,
    uA(UD,W1,[]),
    rA(LR,W2,[]),
    append(W1,W2,N1),
    dA(UD,W3,[]),
    lA(LR,W4,[]),
    append(W3,W4,N2),
    append(N1,N2,OUT).

                  /* 3.7 */
         /* m30A/3 and p240A/3 (+) */
/* Just like uA,rA, etc. but unify with a contiguous */
/* substring of "m30A" or "p240A" and return length. */

m30A(0) --> [].
m30A(VAR1) --> ["m30"], m30A(VAR2), {VAR1 is VAR2 + 1}.

p240A(0) --> [].
p240A(VAR1) --> ["p240"], p240A(VAR2), {VAR1 is VAR2 + 1}.

                  /* 3.8 */
              /* eqtriA/2 (+) */
/* Equilateral triangle recognizer, starting at the first "u". */

eqtriA --> [].
eqtriA --> uA(EQ), m30A(EQ), p240A(EQ).

                      /* 4.1 */
                  /* one_shift/2 */
                /* one_shift(+A,-R) */
/* R is list A cyclically shifted to the left by one position.*/

one_shift(0,0).
one_shift([HEAD|TAIL],W) :-
    append(TAIL,[HEAD],W).

                    /* 4.2 */
                /* all shifts/4 */
          /* all_shifts(+A,-R,+L,+S) */
/* R is all cyclic shifts of A > 0. L is the length of A. */
              /* S starts at 1. */

all_shifts(_,[],S1, S1).
all_shifts(S1,OUTPUT,STRLEN,INC) :-
    INC < STRLEN, INC2 is INC + 1,
    one_shift(S1, VAR3), all_shifts(VAR3,OUT2,STRLEN,INC2),
    append([VAR3],OUT2,OUTPUT).

                      /* 4.3 */
                  /* start shifts/2 */
                /* start_shifts(+L,-AS) */
  /* L is the input list, AS is all cyclic shifts of L. */
/* Note: This predicate uses all_shifts. AS still does not contain A.*/
    /*Look at the example immediately preceding this one.*/

start_shifts(0,0).
start_shifts(STR,VAR) :-
    length(STR, STRLEN),
    all_shifts(STR,VAR,STRLEN,1).

                          /* 4.4 */
                      /* all cases/2 */
                    /* all_cases(+A,-R).*/
/* R is all shifts of A (computed via previous predicates) appended to A. */
              /* This is all possible shifts */

all_cases(0,0).
all_cases(STR,OUTPUT) :-
    length(STR, STRLEN),
    all_shifts(STR,VAR1,STRLEN,1),
    append([STR],VAR1,OUTPUT).

                      /* 4.5 */
                  /* try_all_sqA/1 */
            /* Given all_shifts (including none), */
  /* succeeds if some cyclic shift of Cases satisfies sqA.*/

try_all_sqA([]).
try_all_sqA(IN) :-
    member(X, IN),
    sqA(X,[]),
    format('Cyclic Shift: ~p is a square.', [X]).


                      /* 4.6 */
              /* try_all_rctA/1, try_all_eqtriA/1 */
  /* Same as try_all_sqA(+Cases), but looking for rctA and eqtriA in Cases.*/

try_all_rctA([]).
try_all_rctA(IN) :-
    member(X, IN),
    rctA(X,[]),
    format('Cyclic Shift: ~p is a rectangle.', [X]).

try_all_eqtriA([]).
try_all_eqtriA(IN) :-
      member(X, IN),
      eqtriA(X,[]),
      format('Cyclic Shift: ~p is an equilateral triangle.', [X]).
