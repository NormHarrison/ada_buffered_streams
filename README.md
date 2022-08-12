# Ada buffered streams

A personal WIP project meant to hopefully improve the performance of stream-based
communication in other projects. Buffered writing and reading is provided, with
reading handled in two different ways depending on use case.

The root type is declared as abstract inside `buffered_streams.ads` and provides
the implementation of buffered writing that applies to all descendant types.
Buffered writing works by directing all calls to `Write` to an internal buffer,
then sending the buffer's contents across the stream all at once when it's
completely filled, or whenever the `Flush` primitive is explicitly called.

The first implementation of buffered reading is provided by the subtype
`Agnostic_Buffer_Type` inside the package `buffered_streams-agnostic_buffer.ads`.
This type works much like a regular stream, in which any data type can be read
from the stream at any time. Buffering functionality works by calling the
`Prefill` primitive and specifying the size of the type that will be read next.
Calls to `Prefill` are currently always expected to be followed by calls to
`Read`. The way in which buffering works via this type is planned to change in
the future.

The second implementation of buffered reading is provided by the subtype
`Unique_Buffer_Type` inside the package `buffered_streams-unique_buffer.ads`.
This type works differently than traditional streams, as indicated by its name,
it is meant to be used to only read a single data type from the stream, like
characters/strings for example, hence why the package is generic. Through this
restriction, it is able to offer the ability to read until a certain delimiter
is encountered in the buffer via the `Read_Until` primitive, returning all data
that was read up until that point on Ada's secondary stack. Though in some cases
you may need to explicitly allocate on the heap when very large amounts of data
are being dealt with.
