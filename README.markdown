A library helping you write 
=======================

What's so opinionated about this.


Some goals:

  - Be compatable with the builtin prelude from **base**. Our library is
    _not_ implemented as a custom prelude. One of the exciting things about
    working in Haskell is the degree of experimentation that researchers
    and engineers are able to do with the language itself, its library
    ecosystem, and the development tooling used to build and test software.
    There are some fascinating explorations of different preludes, but the
    original `Prelude` module remains in scope by default and in use by the
    majority of other libraries. Nothing prevents you using an alternate
    prelude on top of this package, of course.

    \[We do _not_ argue that the Haskell language should never change (we'd
    be the first ones to cheer if String was ripped out and tossed on the
    dungheap of history next to VHS tapes and compters without Esc keys).
    We can fix the textbooks! But in the present tense, if the first thing
    a newcomer has to deal with is basic idioms not being what they have
    been shown to expect and typeclasses from other libraries not working
    then it makes for an awkward learning curve.\]

  - Facilitate interoperability. One of the biggest annoyances working in
    Haskell is trying to figure out how to get a value returned from one
    library converted ot the type you need to feed into the next one you
    are calling. This is common with textual types and when dealing with
    sets and maps from the various containers libraries.

  - Provide a place to implement common application functionality. Over
    time, ideas about best practices evolve. Current approaches to
    structuring programs include an outer layer over IO and then 

  - Make it possible to use package's modules without using qualified
    imports.

This library provides the following:

  - a Program type encapsulating both internals 


Our approach has been to create wrapper layers around existing
functionality to bring them together in a useful gestalt. If you wish to
dismiss this library as a giant bikeshedding exercise you would not be
since

