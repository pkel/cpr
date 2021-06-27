# TODO

We lost factor ~5 in speed and increased allocations by factor ~4.26
between 808cb11 ... 37b595b. This seems a bit extreme.

I did a quick test without the "bad network conditions". I still
observed factor 5 increase in allocations.

Another option is: Exponential delays in fully connected graph with 32
nodes causes that in many cases an indirect delivery is faster
than direct delivery. ... This seems to be the case. Less extreme
delays reduce allocations and runtime.
