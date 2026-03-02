# Stage 0

Consists of every utility code only.

- index.ml - represents either a 0-based or a 1-based index
- interval.ml - represents a range defined as two indices, or index+length
- location.ml - represents a single source code location: filename + line (index) + interval
- slice.ml - Python-like slices (conversion to intervals available)
- streamutil.ml - Streams, as in a potentially infinite sequence of values; lazy eval (different than Seq)
- common.ml - Miscleania
- smtlib.ml - serialization into SMTLib
- ojson.ml - handling JSON with Option
- rjson.ml - handling JSON with Result
