# Protypo

## What is this?

This Ada library implements a template expansion engine.

I wrote it because I needed it and nothing that I found satisfied me fully. Moreover, I had lots of fun (and work...) writing it ;-)

I like the final result (but I guess you can say I am partial to it). It is quite flexible and with a simple conceptual model and a syntax is very Ada-ish. At the moment it is not strongly typed, but rather duck-typed "a la Ruby."

## A fast tutorial (or maybe two?)

I guess the best way to introduce you to this library is by means of a simple tutorial.  Being this a library there are two different type of users
1. Users that write templates to be expanded by the engine.  We will call these users *final users* or simply *users*
2. Programmers that include the library into a software of theirs. We will call these users *programmers*

It follows that we actually need *two* tutorials: one to explain to the *users* how to write templates and another to explain to the *programmers* how to include the library into a software. See
* [The user tutorial](doc/User/user_tutorial.md) in the `doc/` directory for the user-level tutorial
* A more precise language description can be found [here](doc/Blueprints/syntax.md)
* [The API description](doc/Programmer/API.md), still in the `doc/` directory for the programming documentation
* Example codes in `src/examples` (to be written)


