% CPEN457 - Programming Languages
% Author: Kenneth R. Young Castro
% Spring 2018
% Prof: Phd I. Vergara Laurens
% Description: This program can derive equations and will print out the
% result as simplified as possible.

%Handle Derivations
d(X, U**Y,Y*DU*U**(Y-1)):-d(X,U,DU).
d(X, U+V, DU+DV):-d(X, U,DU),d(X,V,DV).
d(X,U-V,DU-DV):-d(X, U,DU),d(X,V,DV).
d(X,U*V,U*DV+ V*DU):-d(X, U,DU),d(X,V,DV).
d(X,U/V,(DU*V - DV*U)/(V*V)):-d(X, U,DU),d(X,V,DV).
d(X,C,0):-integer(C), not(C=X).
d(X,X,1).

%Simplify Derivation
%X = Math Variable
%U = Equation to derive
%SDU = Simplified Result of derivation
sd(X,U,SDU):-s(U, SU),d(X,SU,DU),s(DU,SDU).

%%%Handle atomic
s(A, A):- atomic(A),!.

%%%Handle sum logic

%If they are integers, sum them.
s(A + B, C):- number(A), number(B), C is A + B.

% If its not an integer, then it means its either an expression or a
% variable. This will try to continue simplifying it. IF a change does
% occur, then continue simplifying with the new change.
s(A + B, C):- s(A, RA), s(B, RB), (A \== RA ; B \== RB), s(RA + RB, C).

%Any number plus 0 is the same number
s(A + 0, R):- s(A, R).
s(0 + A, R):- s(A, R).

%%%Handle Substraction logic

%Handle same number substractios(X - X, 0).

%If they are integers, substract them.
s(A - B, C):- number(A), number(B), C is A - B.

% If its not an integer, then it means its either an expression or a
% variable. This will try to continue simplifying it. IF a change does
% occur, then continue simplifying with the new change.
s(A - B, C):- s(A, RA), s(B, RB), (A \== RA ; B \== RB), s(RA - RB, C).

%Any number minus 0 is the same number
s(A - 0, R):- s(A, R).
s(0 - A, -(R)):- s(A, R).


%%%Handle Multiplication logic

%If they are integers, multiply them.
s(A * B, C):- number(A), number(B), C is A * B.

% If its not an integer, then it means its either an expression or a
% variable. This will try to continue simplifying it. IF a change does
% occur, then continue simplifying with the new change.
s(A * B, C):- s(A, RA), s(B, RB), (A \== RA ; B \== RB), s(RA * RB, C).

%Anything multipied by 1 is the same number
s(A * 1, A).
s(1 * A, A).

%Any number multiplied by 0 is 0
s(_A * 0, 0).
s(0 * _A, 0).

%%%Handle division logic

%If they are integers, divide them.
s(A / B, C):- number(A), number(B), C is A / B.

% If its not an integer, then it means its either an expression or a
% variable. This will try to continue simplifying it. IF a change does
% occur, then continue simplifying with the new change.
s(A / B, C):- s(A, RA), s(B, RB), (A \== RA ; B \== RB), s(RA / RB, C).

%Any number divided by one is the same number
s(A / 1, A).

%Any number divided by itself is 1
s(A / A, 1).

%0 divided by any number is 0
s(0 / _A, 0).

%%%Handle exponential logic

%Anything to the power of 0 is 1
s(A ** 0, 1).

%Anything to the power of 1 is itself
s(A ** 1, A).

%Anything multiplied by itself is the same as the number but squared
s(A * A, A**2).

% A number multiplied by itself but elevated to the power of anything is
% the same as adding 1 to the exponent
s(A ** X * A, R):-s(A ** (X + 1), R).
s(A * A ** X, R):-s(A ** (X + 1), R).

% A number to the power of anything times the same number to the power
% of anything is the number to the power of the sum of the "anythings".
% Continue simplifying with the new expression
s(A ** X * A ** Y, R):- s(A ** (X + Y), R).

% A number with exponents divided by the same number with exponents is
% the number to the power of the substraction of the original exponents
s(A ** X / A ** Y, R):- s(A** (X-Y), R).

% Try to simplify exponents if possible. If it did change, then continue
% simplifying with the new changes
s(A ** X, R):- s(X, RX), X \= RX, s(A ** RX, R).

%%% Handle Variable Algebra logic

% The sum of a variable with the same variable is two times the
% variable(but it concatenates them, not actually multiply them)
s(A + A, R):- not(number(A)), R = 2 * A.

% The sum of a variable with the same variable but with a coefficient is
% the coefficient + 1 times the variable. This will try to simplify A +
% 1 if possible.
s(A * X + X, R):- number(A), s(A + 1, RA), R = RA * X.
s(X + A * X, R):- number(A), s(A + 1, RA), R = RA * X.

% This will handle some specific cases
% where A * X + B * X is the same as (A + B) * X, even if they are all
% numbers
s(A * X + B * X, R):- C is A + B, s(C * X, R).

% This will handle some specific cases
% where A * X - B * X is the same as (A - B) * X, even if they are all
% numbers
s(A * X - B * X, R):- C is A - B, s(C * X, R).

% This will handle some specific cases
% where A * X * B * X is the same as (A * B) * X, even if they are all
% numbers
s(A * X * B * X, R):- C is A * B, s(C * X, R).

% this will handle some specific cases where you have A * (B * X) where
% the result will be A * B times the variable X.
s(A * (B * X), R):- number(A), number(B), not(number(X)), C is A * B, R = C * X.

% This will handle multiplication of variables with different power
s(A * X**N * B * X **M, R):- number(A), number(B), number(N), number(M), not(number(X)), C is A * B, O is M + N, s(C * X **O, R).

%Attempt at handling divisions
%This is suppose to handle simplification of division with variables
%
% if the power of numerator is higher than the power of the divisor,
% then substract them and put the variable to the power of the result of
% the substraction on the numerator
s(A * X ** N / B*X**M, R):- number(A), number(B), number(N), number(M), not(number(X)), O is N - M, O > 0, s(A * X **O /B, R).

% if the power of numerator is lower than the power of the divisor,
% then substract them and put the variable to the power of the result of
% the substraction but negative on the divisor
s(A * X ** N / B*X**M, R):- number(A), number(B), number(N), number(M), not(number(X)), O is N - M, O < 0, O is -(O), s(A/B*X ** O, R).

% if the power of numerator is the same as the power of the divisor,
% then eliminate the variables
s(A * X ** N / B*X**M, R):- number(A), number(B), number(N), number(M), not(number(X)), O is N - M, O == 0, s(A / B, R).

%Failed attempt at simplifying division
% s(A * X ** M / B, R):- number(A), number(B), number(M), not(number(X)),
% C is A / B, s(C * X ** M, R).


% If you couldn't simplify it, just return the number that wasn't
% possible to simplify(possibly because it was already simplified to the
% fullest)
s(X, X).















