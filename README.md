# Documentation: General Overview:

The idea in this file is to use some variant of test
driven development (for now, within the framework
describe in ~/emacs-24.3/info/ert.info) as a way to
keep track of an evolving model of a codebase.  It is
build around a set of tests that instantiate the FACE
model of computational creativity.  This model is
specified, at the outline level, as follows:

```
F = framing
A = aesthetic
C = concept
E = Example / Expression (?)
```

# Potential uses of this system:

- In a given instantiation of this programme, we might
model something like the growth of the APM XI corpus
through the addition of new examples and new
relationships between elements.

- In general, like with other kinds of test-driven
development, we might write the tests "first" and use
these to specify objectives as a "development roadmap"
i.e. when those tests are passing, a given phase of the
project is complete.

Note that it seems vastly easier to build a particular
instantiation of the FACE model than it is to build a
general-purpose test that would always say "you have
added a new concept" or "you have added a new
aesthetic-method".

Instead of having a fully general model that works
every time, we could instead come up with a
type-theoretic treatment of the FACE schema, so that we
could check whether a given system implements a test
for a given facet of FACE.

If we go that route, it might make more sense to
implement the test system in Haskell rather than
LISP... but of course we actually want to be flexible,
so we need to maintain a general sense of a test
"implementing" a given type of test, across different
programming languages - eventually, what we could
provide in the COINVENT project is a set of sample
implementations in various languages, together with an
API for reporting/logging progress.  In order to make
this attractive to potential users (and generate some
empirical data) we might want to have a computational
creativity contest.

From a philosophical point of view, note that simply
creating more and more concepts probably isn't the most
"creative" -- actually, defining (and passing) new
tests which get at different facets of creativity would
tend to improve the overall "score".  Maybe each new
test should be treated as a "multiplier" or something.

From a procedural point of view, the system should,
accordingly, work on several levels:

- A test that can detect when e.g. a new framing has
been produced by looking for new documentation strings
or whatever else qualifies as a "framing".

- A test that can detect when a new *test* has been
written, E.g. in addition to documentation strings,
suppose we define a new kind of framing object called a
"walkthrough", and we will consider a new framing to
have been produced whenever either a new doc string or
a new walkthrough is produced.  In the first place, we
need to note the existence of the walkthrough test.

"Tests" themselves could be considered to be concepts
or perhaps aesthetics (typically they map into {0,1},
but in general they could map to a set of error codes).

Note also that the tests generally need to be applied
to the "increment", for instance, all *new* code that
has been written since the last commit.  So, we need a
method for extracting this increment.  I did something
similar in my thesis that analysed git logs and looked
for named entities in the diffs.  Extracting a
meaningful piece of code *directly* from a line-diff is
not always going to be possible, since adding a single
line typically isn't a "semantically" meaningful
change.

> Making our diffs more semantic is an obvious thing to fix.

Let's assume that we can get ahold of the semantically
meaningful changes with a little bit of code that scans
a git commit and then reads the corresponding file.
Then we need to analyse this new code with the
available tests -- and maybe, for user-friendly
record-keeping, update the commit message automatically
with a tally.  All of this git stuff is a fairly
specific implementation detail.

We could start with something simpler, like identifying
the number of `defun' and `defvar' usages in a given
file.  It's not entirely clear that the `ert-deftest'
mechanism will be the best way to go, rather than using
some other ad hoc code scanning and reporting
mechanism, but we can try!

> After a first pass, I can see that ERT does work, but the reporting mechanism isn't quite aligned with what we want to do, so after a prototyping phase, it would be nice to augment ERT with some more "qualitative" reporting mechanisms.

The ERT documentation says "ERT only provides
explanations for predicates that have an explanation
function registered."  But for now, explanations are
only triggered when a test fails, so the intuitive
logic in the prototype is "reversed", i.e. "failing" is
good, because it produces explanations.

They also say:

>   ERT does not have built-in support for mocks or stubs.  The package `el-mock' (see `http://www.emacswiki.org/emacs/el-mock.el') offers mocks for Emacs Lisp and can be used in conjunction with ERT.

Potentially worth checking this out when we get to the
point of implementing a proper type theoretic schema.

# Documentation: Implementation plan:

The code in the ert-x package includes a macro for
running tests on the current buffer, called
`ert-with-buffer-renamed'.  In theory, we could use
this to test how many matches for a given regexp are
present in the buffer, say.  The one issue here is
figuring out how to get ERT to report something
qualitative, other than just pass/fail.

We have a sort of cheap method of doing that now, in
the section "Implement a cheap counting test".

We can have the code take in an "increment" to scan,
see "Pass in an increment"

We can run all of the tests on a selected increment,
see "Run a battery of tests on a given file".

Next up, we would want to do a bit of "model
selection", i.e. check which of the 8 possible tests
are actually implemented, and run only those.  Although
actually there are 8 *types* of test, and not just 8
tests.  Maybe if I take out the "shoulds" the tests
will automatically pass?  Yes, that's true.  So, this
way I will only see reporting on the tests that are
actually implemented.

It seems like I might as well just go ahead and
implement this in ONE case (e.g. the APMXI case) and
then I can take another pass to implement it in a more
general type theoretic way.

[...]

Develop a nicer reporting framework so that we're not
just seeing a bunch of failed tests, but instead, a
nice-looking summary.  Note that this is not separate
from the type theoretic issues - we want to have a
"score card" for each increment.

# Documentation: Comments on first working prototype:

I now have a prototype that can run tests
non-interactively on the incremental changes in a git
repository, using the git post-commit hook.  As I
mentioned just above, it would be nice to have a
somewhat better reporting mechanism, and to then alter
the commit messages by sticking relevant "score card"
information into them.  This is eminently doable.

For now the "interesting things we can notice" are
defined in a rather simplistic way, since we only
notice a few regexps that describe the creation of
defuns and similar.  However, this is already a usable
start, i.e. I can easily make some tests that notice
things of interest in hcode (e.g. defthm instead of
deftest; I'll have to take a look at my old notes to
see what's there).

The thing I'm thinking is: actually, just creating more
and more definitions -- at least in this manner -- is
not particularly creative.  On one level it might be
"more creative" to do a lot of them (as with a big
translation effort) and ultimately when very many
definitions are known, it would be creative to add
something truly new.  However, one might expect a very
long non-creative interlude in the middle, where we
just transcribe things from lots and lots of known
sources.  Clearly there would be *some* elements of
creativity in this process, however: having to do in
this case with inventive forms of transcription or
translation, with interesting speed-ups and so forth.

But the basic point is that creativity is only really
defined relative to an "edge".  Once we've done that
thing before, or a lot of things like it, then another
iteration of the same is not particularly creative.

Continuing this idea, do we want to count "new"
appearances of `defn` or `defthm` as truly new concepts,
if they have simply been transcribed from somewhere else?
(If so, we'll have to do a little more integration work to
describe how one test triggers recognition for another
test.)
