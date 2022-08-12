# TODO:

- Instead of converting each element type to a stream element one at a time
via a call to an instance of `Ada.Unchecked_Conversion`, consider utilizing
the `for <identifier>'Address use <identifier>'Address` concept.

- Consider adding buffered writing functionality (which shouldn't be
near as complex as reading).
