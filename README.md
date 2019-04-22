# Common-deque


This simple project is a clone of [C++ std::deque](https://en.cppreference.com/w/cpp/container/deque).
My intent was to mimic 
it as close as possible, so the names of methods are the same and some technical
details are identical when possible.
It depends on `iterate` because I am planning to add an iterate clause for it.

It's very raw at the moment, hence the version. I'm planning to upgrade it
to the point where it can actually be used by other people. A lot of repeating code,
probably need macros, the style is also poor, some design stuff is questionable.
No exceptions at all are implemented, which is No1 problem, I'm just not sure
about how and where do I do this.


#INTHEFARFUTURE

Make this a sequence sublass, look http://www.sbcl.org/manual/#Extensible-Sequences.
Add an iterator protocol

