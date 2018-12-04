A library helping you write 
=======================

What's so opinionated about this.


Some goals:

  - Be compatable with the builtin prelude from **base**. This library is
    _not_ implemented as a custom prelude. One of the exciting things about
    Haskell is the degree of experimentation that researchers and engineers
    are able to do with the language itself, its library ecosystem, and the
    development tooling used to build and test software.  Even the basic
    functions and syntax of the language are implemented as as a "prelude"
    library whose code is imported by default into every module. There are
    some fascinating explorations of _different_ preludes—all with an aim
    to improve on the well-documented shortcomings of the built-in one. The
    original `Prelude` module remains in scope by default, however, and so
    we set out to be compatible with (or at least ignore) that default as
    it is in use by the majority of other libraries. Nothing prevents you
    using an alternative prelude in a program built on top of this package,
    should you wish.

    \[We do _not_ argue that the Haskell language should never change. We'd
    be the first ones to cheer if String was ripped out and tossed on the
    dungheap of history next to VHS tapes and compters without Esc keys.
    We can fix the textbooks! But in the present tense, if the first thing
    a newcomer has to deal with is basic idioms they've been taught not
    working as they've been shown to expect, then it makes for an awkward
    learning curve.\]

  - Facilitate interoperability. One of the biggest annoyances working in
    Haskell is trying to figure out how to get a value returned from one
    library converted ot the type you need to feed into the next one you
    are calling. This is common with textual types and when dealing with
    sets and maps from the various containers libraries.

  - Make it possible to use package's modules without using qualified
    imports. Haskell's qualified import mechanism is excellent, but often
    leads to cumbersome looking code. The alternative is picking function
    names (which do tend to be longer, alas) which do not collide with
    **base** and hence do not require being imported qualified to avoid
    name collisions.


  - Provide a place to implement common application functionality. Over
    time, ideas about best practices evolve. Current approaches to
    structuring programs include an outer layer over IO which carries the
    application's state and makes it available to inner layers. This
    library includes an implementation of that pattern.

This library provides the following:

  - a Program type encapsulating both internals 


Our approach has been to create wrapper layers around existing
functionality to bring them together in a useful gestalt. If you wish to
dismiss this library as a giant bikeshedding exercise you would not be
since

