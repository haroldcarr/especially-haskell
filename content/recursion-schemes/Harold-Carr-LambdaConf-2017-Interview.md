# Harold Carr LambdaConf 2017 Interview

Harold Carr will be giving a talk at
[LambdaConf 2017][CONF]. He will be speaking about Refactoring Recursion.

[CONF]: http://lambdaconf.us/events/2017/lcusc.html

Follow him on his [Homepage], [GitHub] and [Twitter].

[Homepage]: http://haroldcarr.com/
[GitHub]: https://github.com/haroldcarr
[Twitter]: https://twitter.com/haroldcarr

***PurelyFunctional.tv***: How did you get into Functional Programming?
 
***Harold Carr***: I worked as a research programmer in Robert Kessler's University of Utah
programming languages research group---building Utah Common Lisp and Scheme.
We compiled to something conceptually similar to LLVM (before it existed) then
targeted various processors --- including generating C code (before compiling
to JavaScript, etc., became fashionable). That work made me comfortable with
anonymous high-order functions and recursion. In the late 90s I was the Lisp guy
at Autodesk, completing a major system upgrade.

Starting in 1999 I built and used "llava" ([http://llava.org/](http://llava.org/)), a "Java in Lisp
Syntax" system. In the process of writing a [paper on llava](http://llava.org/ilc2005.pdf)
for the 2005 International Lisp Conference I
discovered I liked using what I had built, but wanted to completely redo the
implementation. Then Clojure came along, making the redo unnecessary. Four years
ago I started typed-functional programming in Haskell.


 
***PF.tv***: What is your talk about?
 
***HC***: 
Recursion schemes. Specifically, how to refactor recursion out of function
definitions by using library routines.

One thing this talk is NOT about (making it different from other recursion
scheme talks and papers): it will not show refactoring recursion out of
data definitions. Most talks quickly go into using `Fix` with data definitions
and then go on to discuss recursion schemes. That makes a talk difficult for
beginners.

My talk only uses list versions of recursion schemes so it can concentrate on
the schemes themselves.  At then end, I will mention `Fix` and give references
for further study.


 
***PF.tv***: Who is your talk for?
 
***HC***: Beginning to intermediate functional programmers.


 
***PF.tv***: What do you hope people will take away from the talk?
 
***HC***: Understanding some of the types of recursion:

- iteration  : doing something with every element of a structure
- iteration  : iteration with access to the current cursor into the structure being traversed
- iteration  : with access to previously computed values in the traversal
- generation : creating a structure (from a seed)
- iteration or generation : that can exit traversal early on specified conditions
- combinations of the above

A desire to understand how to generalize the "list-only" recursion schemes to
other data types (i.e., via `Fix`).


 
***PF.tv***: What concepts do you recommend people be familiar with to maximize their experience with the talk?
 
***HC***: 
My talk will use Haskell.  Knowing Haskell

- type signatures
- function definitions
- anonymous function notation
- function composition (using `.`)
- lists
- explicit recursion (i.e., how to write `length` or `sum` of a list)

***PF.tv***: What resources are available for people who want to study up before the talk?
 
***H***: The first two sections of [A tutorial on the universality and expressiveness of fold](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf).

The blog post by [Antoni Diller](http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html).

My stackoverflow question on list versions of [recursion schemes](https://stackoverflow.com/questions/36851766/histomorphisms-zygomorphisms-and-futumorphisms-specialised-to-lists).

Edward Kmett's [Recursion Schemes : A Field Guide](http://comonad.com/reader/2009/recursion-schemes/)
is useful to become familiar with the terminology,
as is [Wikipedia
](https://en.wikipedia.org/wiki/Category:Recursion_schemes).

The repo where I am preparing the material for the talk is [here](https://github.com/haroldcarr/especially-haskell/tree/master/content/recursion-schemes).

[Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) is a classic paper (but not beginner friendly).

Not necessarily for studying beforehand, but checkout [Tim Williams's](http://www.timphilipwilliams.com/slides.html) great "March 2013 : Recursion Schemes by Example" talk (scroll down) on [recursion schemes](https://www.youtube.com/watch?v=Zw9KeP3OzpU).
 
***PF.tv***: Where can people follow you online?
 
***HC***: 
* Tech:
    - Twitter : [@haroldcarr](https://twitter.com/haroldcarr)
    - GitHub  : [haroldcarr](https://github.com/haroldcarr)
    - Blog    : [haroldcarr.com](http://haroldcarr.com/)
    - YouTube : [tech playlist](https://www.youtube.com/playlist?list=PLH8NVjolugrMUnvnojSVwvyg4HsDLdnNE)

* Music:
    - [BandCamp](https://haroldcarr.bandcamp.com/)
    - [YouTube music playlist](https://www.youtube.com/playlist?list=PLH8NVjolugrMfspsIUQKHQ6VVcL_ElrP3)

* Poetry:
    - [YouTube poetry playlist](https://www.youtube.com/playlist?list=PLH8NVjolugrNwMLAB50ThWD6piJcxbBC_)
 
***PF.tv***: Are there any projects you’d like people to be aware of? How can people help out?
 
***HC***: I have a very large repo [here](https://github.com/haroldcarr/learn-haskell-coq-ml-etc)
where I learn by doing:
It includes code I wrote from coursework, books, blogs etc.
Others might find it useful if they get stuck - I might have done it.

My current interest is [BlockChain](https://en.wikipedia.org/wiki/Blockchain).
I just started a repo [here](https://github.com/haroldcarr/blockchain-framework)
that contains the skeleton of a framework to experiment with different consensus algorithms,
smart contract engines communication subsystems, etc.

Speaking of communication systems, it would be fun to create a version of the [PEPT remoting framework
](http://haroldcarr.net/computerScience/pept/indexPept.html)
in Haskell.

 One thing I have toyed with on-and-off for years is an [RDF](https://en.wikipedia.org/wiki/Resource_Description_Framework) triple browser ([repo here](https://github.com/haroldcarr/rdf-triple-browser)). I have done versions of this in Java in GWT and Swing,
and in Haskell in [threepenny-gui](https://hackage.haskell.org/package/threepenny-gui)
and [reflex](https://github.com/reflex-frp/reflex).
It would be fun to build a version of this with [Purescript](http://www.purescript.org/).

It would be useful to combine educational material on functional programming
into a browsable RDF database using the above triple browser. I have started to
collect and organize such material
[here](https://github.com/haroldcarr/especially-haskell) and
[here](https://github.com/haroldcarr/learn-haskell-coq-ml-etc/tree/master/haskell/topic).

Anyone interested in any of the above, feel free to contact me.

 
 
***PF.tv***: here do you see the state of functional programming in 10 years?
 
***HC***: It will be more mainstream, but still a minority compared to imperative approaches.

 
 
***PF.tv***: If functional programming were a superhero, what superpower would it have?
 
***HC***: The ability to turn lead into gold.

  
