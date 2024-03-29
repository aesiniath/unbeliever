A library helping you write Haskell programs
============================================

This library is for writing command-line applications, be they tools or
long-running daemons. It provides the following:

  - a `Program` type encapsulating internals of logging, signal
    handling, and exception handling;
  - logging handlers for event tracing and application debugging carefully
    designed so that normal output to console is not corrupted by logging
    output or error messages;
  - setting up what command-line parameters your program expects in a
    declarative manner and easily looking up options and arguments at
    runtime;
  - a mechanism for plumbing top-level application state throughout a
    program including support for updating that state;
  - support for extracting metadata from the project's _.cabal_ file for
    use in your program should you need it, for example for use in
    `--version` output;
  - simplified UTF-8 text handling via a `Rope` type backed by a
    finger-tree and optimized for both conveying large passages of text and
    appending chunks when building up texts for subsequent output to a file
    handle, along with code facilitating conversion to other textual types;
  - first class ANSI coloured pretty-printing support;
  - auto-generated `--help` output that is sensitive to available terminal
    width;
  - wrappers around key/value maps and sets of elements optimized for
    common cases and facilitating conversion to other dictionary and
    collection types;
  - and more!

What's so opinionated about this?

This project started as an effort to record library choices, useful
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

The working title for the as-yet unpublished text on this topic is _Haskell for
Unbelievers_ and so **unbeliever** became the name for this library.  We've
since split the library into a number of packages with smaller dependency
footprints. This repo contains the sources for the **core-text**,
**core-data**, and **core-program** packages on Hackage.

Some goals:

  - Be compatible with the builtin prelude from **base**.  One of the
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
    be the first ones to cheer if `String` was ripped out and tossed on the
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
    chooses one implementation for each of these areas and then supplies a
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
    structuring programs include an outer layer over `IO` which carries the
    application's state and makes it available to inner layers which can be
    more restricted or better yet pure. This library includes an
    implementation of that pattern.

If you wish to dismiss this library as a giant bikeshedding exercise you
would not be wrong. Hopefully you'll like the _colour_ of our bikeshed.

Getting Started
---------------

To learn about the text type, see the documentation for
[Core.Text.Rope](https://hackage.haskell.org/package/core-text/docs/Core-Text-Rope.html).
To use the Program monad, you can get started by looking at
[Core.Program.Execute](https://hackage.haskell.org/package/core-program/docs/Core-Program-Execute.html).
