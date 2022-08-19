# TODO:

- In the `Socket_Streamer` package we could rather greatly decrease the amount
of source code inside the package body if we declared both `To_Address` and
`From_Address` inside the base `TCP_Stream_Type`. This would allow us to only
need one overload of the `Root_Stream_Type`'s `Read` and `Write` primitives.
Howver, this means that the `TCP_Stream_Type` would have a discriminant even
though it wouldn't actually be used.

- Could we gain any efficiency by performing copies using identical slices
instead of copying element by element in a loop?

- Instead of converting each element type to a stream element one at a time
via a call to an instance of `Ada.Unchecked_Conversion`, consider utilizing
the `for <identifier>'Address use <identifier>'Address` concept.

- For now, our only custom exception is for indicating when the recursion limit
was reached. We have removed the declaration of the `Index_Type_Too_Small_Error`
exception for now, instead opting to allow the language's automatic checks
to raise `Constraint_Error` in such cases. However, this could be ambigous to
the user, not knowing exactly what the problem was.

- Instead of making the user prefill the agnostic read buffer with the amount
of anticipated data, have them set a discriminant during declaration of an
instance of the type, where they choose in advance how much data will be pulled
into the buffer each time `Read` is called. Though this doesn't really work all
too well, since each component of a record causes a new call to the `Read`
primitive to occur. Though if the size was kept small enough, and if we stopped
copying data into the buffer once it was full (placing it directly into `Item`
instead), it could possibly work.

- Another idea for the behavior of read functionality in the `Agnostic_Buffer`
child-package is allowing multiple calls to `Prefill` to occur back-to-back.
Though due to increasing complexity, the recently attempted implementation
has been scrapped. If we do decide to attempt this again, the solution will
require separate `Last_Index` and `Start_Index` components to be declared
inside `Agnostic_Buffer_Type` (a fact we completely overlooked during the
first implementation attempt).
