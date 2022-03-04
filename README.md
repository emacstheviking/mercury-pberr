# PBERR - Puts Back Errors

This is a utility for Mercury development with nano (my main editor these days)
that, should you wish, will reformat the current code buffer with the contents
of any corresponding error file, that is, if you are editing a file called
`foo.m` and there exists a `foo.err`, that contents of the error file are
loaded, parsed and then placed into the source buffer at the lines specified by
the starting block of errors. It's easier to see than explain!

The error content is inserted as comments so your code would still be
compilable. I use nano with a custom syntax highlighting file I created (a copy
is committed here for clarity) based on the Prolog one, it works for me.

# Requirements

That you have a recent working Mercury compiler on your path and `make` installed. Actually this program is so simple you could just build it like this:

    $ mmc pberr.m

using make:

    $ make
    $ make install

but I use Makefile for everything most of the time with Mercury. Actually, my first ever Mercury program I wrote will generate a blank module and a Makefile for you.

It can be found here:

  [GitHub: mcnew](https://github.com/emacstheviking/mcnew)


**NOTE** For me, make install places it into $HOME/bin, if that's not right for
you then just copy the final executable to somewhere on your path so that Nano
can find it when you invoke the formatter with Meta-F.


# Using it

Copy the nano configuration file in this project to the relevant place for your platform, I am using OS X so for me it is:

    $ ~/.nano/mercury.nanorc

Invoke the formatter with M-F (Esc F) and it should, if configured correctly, execute the formatter and then you will see that the errors from that file have been inserted as comments:

**BEFORE**

        ( if
            Args = [ sexp(_, Inits), HaltCond, _Step | Body]
        then
            arg_to_callterm(L, HaltCond, CoRes, !Errors),
            args_to_inst(L, Body, Inst, !Errors),

            ( if
                CoRes = yes(Conditional)
            then

**AFTER**

        ( if
    %E
    %E 000237    warning: variable `Inits' occurs only once in this scope.
    %E 000237  In clause for predicate `ck_felt'/7:
    %E 000237    warning: variable `Inits' occurs only once in this scope.
    %E 000237  In clause for predicate `ck_felt'/7:
    %E
            Args = [ sexp(_, Inits), HaltCond, _Step | Body]
        then

Then you can read the error, shown in red if you used my nanorc as directed,
understand your mistake, delete the comments and fix your code. Rinse repeat.
Happy days.

# Ending

Bugs etc, raise them here. Suggestions too. TBH the audience is limited!

