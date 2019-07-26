# What is this repo?

I've doodled up a few examples to the "fib n-k" problem (explained below) in
different languages.  Write a solution in your own language, or update mine
(the Haskell one is especially shoddy), and make a pull request! I'd be happy
to take any submissions.

Writing a program which can find all "fib n-k" sequences for a given n & k is a
fairly comfortable exercise for the fundamentals of any programming language.

It covers the basics of taking & parsing user input, applying functions
repeatedly, and (very!) simple data structures.

# What is Fib N-K?

Fib N-K is a math game.

The standard fibonacci sequence starts with two numbers (the "seed"), a and b,
which are a = 0 and b = 1.  
Every successive number in the sequence is formed by adding the previous two
numbers.

```
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144...
```

If you apply a modulo of 10 to every single term after summation, you end up
with a similar but different sequence.

```
0, 1, 1, 2, 3, 5, 8, 3, 1, 4, 5, 9, 4...
```

You can think of this sequence as the result of a function which takes a pair
of values (a,b) and produces a sucessive pair (b,a+b), where a and b can take
on the values 0 to 9.

```
(0, 1) -> f -> (1, 1) -> f -> (1, 2) -> f -> (2, 3) -> f -> (3, 5) -> ...
 0              1              1              2              3
```

Obviously, this sequence eventually must produce the terms 0 and 1 again (it's
an injective function on a finite domain), at which point it will loop back on
itself. 

The full sequence, assuming the first two terms are 0 and 1, is as follows:

```
0, 1, 1, 2, 3, 5, 8, 3, 1, 4, 5, 9, 4, 3, 7, 0, 7, 7, 4, 1, 5, 6, 1, 7, 8, 5, 3, 8, 1, 9, 0, 9, 9, 8, 7, 5, 2, 7, 9, 6, 5, 1, 6, 7, 3, 0, 3, 3, 6, 9, 5, 4, 9, 3, 2, 5, 7, 2, 9, 1 (, 0, 1...)
```

Interestingly, this sequence consists of 60 pairs of numbers, so there are 40
other pairs in the domain that are never explored by it.

If we choose other pairs as seed values, we can discover 5 other sequences
that, combined with the sequence above, partition the domain entirely.

The first case, where the initial values are 0 and 0, is trivial and obviously
loops back on itself instantly.

```
0, 0 (, 0, 0...)
```
```
0, 2, 2, 4, 6, 0, 6, 6, 2, 8, 0, 8, 8, 6, 4, 0, 4, 4, 8, 2 (, 0, 2...)
```
```
5, 0, 5, 5 (, 0, 5...)
```
```
2, 1, 3, 4, 7, 1, 8, 9, 7, 6, 3, 9, 2 (, 1, 3...)
```
```
4, 2, 6, 8, 4 (, 2, 6...)
```

Since these partitions are completely disjoint, the function can be said to
form equivalence classes on pairs in the same sequence.

## Enter N & K
So, the first example added together the previous 2 values, and modulo'd them
by 10.

We call the first term (2) the "k", the total number of previous values to add
together.

The second term (10) is the "n", the modulo to take over each sum.

Varying the n & k make for differently long cycles w/ different patterns.

