# TODO

We lost factor ~5 in speed and increased allocations by factor ~4.26
between 808cb11 ... 37b595b. This seems a bit extreme.

I did a quick test without the "bad network conditions". I still
observed factor 5 increase in allocations.
