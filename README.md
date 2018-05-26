These are just some random scripts, experiments, and resources I'm writing/using
as I learn more Common Lisp.

I've had the most luck using [Clozure Common Lisp](https://ccl.clozure.com/download.html).
the other impelementations I've tried have trouble with C extensions like SDL2
and OpenGL, especially on macOS.

I also like [SBCL](http://www.sbcl.org/) for another workhorse CL implementation
and [CLISP](https://clisp.sourceforge.io/) is good when I need some auto-complete
and friendlier error messages.

I would recommend, if you're just starting to learn lisp, that you start out
using CLISP since it'll tab complete lots of stuff for you and if you keep
tabbing or ask it it'll open up the web docs for you. For the other
implementations you'll want to grab [rlwrap](https://github.com/hanslub42/rlwrap)
and use it so you can get a few modern REPL features and behaviour.

If you want to try out some off this just grab the right Clozure CL for your system
then get [Quicklisp](https://www.quicklisp.org/beta/) and set it up, I've also
got a copy in the `tools` directory. Then just follow any instructions in the
specific folders.

* `books`
    * it's on the tin, just some PDF's of super valuable resources.
* `byte-parser`
    * reading in a file byte-by-byte, how you'd start parsing lots of file types.
    * this also has a bit of infrastructure for building a standalong executable on CCL and SBCL.
* `cd-database`
    * implementation of the simple database from chapter 3 of *Practical Common Lisp*
* `cl-ls`
    * a halfway working `ls` implementation. file/path handling is a big edge case in CL.
    * also contains the some stuff to build a standalone exectuable.
* `email`
    * implementation of spam filter from chapter 23 and pathing lib from
      chapter 15 of *Practical Common Lisp*
    * also contains my own minimal RFC 5322 implementation (`email.lisp`) for
      parsing and prepping the emails in `ham.tar.gz` and `spam.tar.gz`.
* `guis`
    * a couple small experiments/examples of using SDL2 and OpenGL
* `log-parser`
    * some code to parse webserver access logs to see who's been visiting
* `midi`
    * an example of parsing some information out of a midi file
* `notes`
    * my notes and code snippets, mostly from when I read *Practical Common Lisp*
* `sketch`
    * my lisp implementations of the [Coding Math](https://www.youtube.com/playlist?list=PL7wAPgl1JVvUEb0dIygHzO4698tmcwLk9)
      videos just using CL and the [Sketch](https://github.com/vydd/sketch) framework.
* `tools`
    * a copy of `quicklisp.lisp`, a script, and a specific copy of `asdf.lisp`
* `webserver`
    * a couple of small experiements creating webservers in Lisp using
      [Hunchentoot](https://edicl.github.io/hunchentoot/) and
      [Wookie](http://wookie.lyonbros.com/)
