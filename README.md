Translations to Clojure of On Lisp by Paul Graham
=================================================

This is not meant to be a complete reference by any means, it was simply
a by-product of reading the book while teaching myself Clojure.

It's also a work in progress and I expect to gradually work through at
least some of the larger macros presented in the second half of the
book.

Get the Book (it's great)
-------------------------

It's available for free [here](http://www.paulgraham.com/onlisp.html)

Anyway, what follows probably won't be very comprehensible without it.

Other Notes
-----------

To state the (hopefully) obvious, this code is not meant to be just
executed. The code has all been developed via Slimv/Swank which is to
incrementally push selected forms to the REPL, hence the mixture of
definition and execution at the top level of each file. Also, no attempt
has been made to make any code here production quality.

In some places I faced various difficulties or misunderstandings, but
rather than rewrite or edit my comments, I've just added my correction
or new understanding below, so read on if something looks obviously
wrong.


Mistakes or Poor Style?
-----------------------

If you find a mistake or any other poor forms, please feel free to file
a bug or comment on the offending line, or of course just make the
project your own.


License
-------

<a rel="license"
href="http://creativecommons.org/licenses/by/3.0/deed.en_US"><img
alt="Creative Commons License" style="border-width:0"
src="http://i.creativecommons.org/l/by/3.0/88x31.png" /></a><br /><span
xmlns:dct="http://purl.org/dc/terms/" property="dct:title">On Lisp in
Clojure</span> by <a xmlns:cc="http://creativecommons.org/ns#"
href="https://github.com/pangloss/onlisp-in-clojure"
property="cc:attributionName" rel="cc:attributionURL">pangloss</a> is
licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by/3.0/deed.en_US">Creative
Commons Attribution 3.0 Unported License</a>.<br />Based on a work at <a
xmlns:dct="http://purl.org/dc/terms/"
href="http://www.paulgraham.com/onlisp.html"
rel="dct:source">http://www.paulgraham.com/onlisp.html</a>.
