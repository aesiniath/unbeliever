A library helping you write Haskell programs
============================================

This library is for writing command-line applications, be they tools or
long-running daemons. It provides the following:

  - a Program type encapsulating internals of logging, signal
    handling, and exception handling;
  - a mechansim for plumbing application state throughout a program;
  - setting up command-line parameters parsing,
  - a simplified UTF-8 text handling type optimized for appending chunks
    and then its subsequent output to a file handle.

What's so opinionated about this?

This project started as an exercise to record library choices, useful
idioms, and recommended practices for teams writing Haskell programs in a
large corporate environment. We quickly realized that a cookbook would be
far less helpful than a library which simply presented these opinions as a
framework application developers could just use out of the box.

Our approach has been to create wrapper layers around existing
functionality to bring them together in a useful gestalt. There are some
extremely powerful libraries in the Haskell ecosystem, but using them
together can sometimes be difficult. Packages date from different
eras of Haskell's growth as an industrial language and so they often follow
different usage idioms making interoperability a challenge.

The working title for the as-yet unpublished text on this topic is _Haskell
for Unbelievers_ and so **unbeliever** became the name for this package.
If you wish to dismiss this library as a giant bikeshedding exercise you
would not be wrong. Hopefully you'll like the _colour_ of our bikeshed.

Some goals:

  - Be compatable with the builtin prelude from **base**.  One of the
    exciting things about Haskell is the degree of experimentation that
    researchers and engineers are able to do with the language itself, its
    library ecosystem, and the development tooling used to build and test
    software.  Even the basic functions and syntax of the language are
    implemented as as a "prelude" library whose code is imported by default
    into every module. There are some fascinating explorations of
    _different_ preludes—all with an aim to improve on the well-documented
    shortcomings of the built-in one. The original `Prelude` module remains
    in scope by default, however, and is in use by the majority of other
    libraries. So we set out to be compatible with (or at least able to
    ignore) that default and thus **unbeliever** is _not_ implemented as a
    custom prelude.
    
    Nothing prevents you using an alternative prelude in a program built on
    top of this package, should you wish.

    \[We do _not_ argue that the Haskell language should never change. We'd
    be the first ones to cheer if String was ripped out and tossed on the
    dungheap of history next to VHS tapes and compters without Esc keys.
    We can fix the textbooks! But in the present tense, if the first thing
    a newcomer has to deal with is basic idioms they've been taught not
    working as they've been shown to expect, it makes for an awkward
    learning curve.\]

  - Facilitate interoperability. One of the biggest annoyances working in
    Haskell is trying to figure out how to get a value returned from one
    library converted to the type you need to feed into the next one you
    are calling. This is common with textual types and when dealing with
    sets and maps from the various containers libraries. This library
    choses one implementation for each of these areas and then supplies a
    typeclass to permit conversion from our type to the types in common use
    in other libraries.

  - Make it possible to use package's modules without using qualified
    imports. Haskell's qualified import mechanism is excellent, but often
    leads to cumbersome looking code in type signatures and at function
    call sites. The alternative is picking function names (which do tend to
    be longer, alas) which do not collide with **base** and hence do not
    require being qualified to avoid name collisions.

  - Provide a place to implement common application functionality. Over
    time, ideas about best practices evolve. Current approaches to
    structuring programs include an outer layer over IO which carries the
    application's state and makes it available to inner layers which can be
    more restricted or better yet pure. This library includes an
    implementation of that pattern.

