# TODO:

- Instead of converting each element type to a stream element one at a time
via a call to an instance of `Ada.Unchecked_Conversion`, consider utilizing
the `for <identifier>'Address use <identifier>'Address` concept.

- Consider adding buffered writing functionality (which shouldn't be
near as complex as reading).

- For now, our only custom exception is for indicating when the recursion limit
was reached. We have removed the declaration of the `Index_Type_Too_Small_Error`
exception for now, instead opting for allowing the languages automatic checks
to raise `Constraint_Error` in such cases. However, this could be ambigous to
the user, not knowing exactly what the problem was.

- Continue determining the best way to handle creating stream access instances.

- Instead of making the user prefill the buffer with the amount of
anticipated data in the `Agnostic_Buffer` package, have them set a
descriminant during declaration of an instance of the type, where
they choose in advance how much data will be pulled into the buffer
each time (though this doesn't really work fully, since each component
of a record causes a new call to the `Read` primitive ot occur, would
we keep track of calls somehow to prevent this?)
