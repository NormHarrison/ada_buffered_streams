with Ada.Streams;
with Ada.Exceptions;


generic
   type Index_Type is (<>);
   type Element_Type is (<>);
   --  `Index_Type` and `Element_Type` must both be a discrete
   --  type (Integer or Enumeration).

   type Array_Type is array (Index_Type range <>) of Element_Type;

   with function "+" (Left, Right : in Index_Type) return Index_Type is <>
     with Warnings => Off;
   --  `+` is expeceted to be a function that performs plain addition between
   --  two values of type `Index_Type`, always producing the same result when
   --  provided a particular set of operands. The language provides an addition
   --  operator for every discrete type by default.


package Buffered_Streams.Unique_Buffer is

   type Unique_Buffer_Type
     (Write_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Read_Buffer_Size  : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit   : Natural)
   is limited new Root_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size) with null record;
   --  An `Unique_Buffer_Type` instance provides a way to continously
   --  collect data from a stream on Ada's secondary stack until a certain
   --  combination of elements (the delimiter) is found. Unlike
   --  `Agnostic_Buffer_Type` instances, instances of this type (and the
   --  entire package) are only meant to buffer/read a single data type (most
   --  commonly strings). The additional `Recursion_Limit` delimiter sets a
   --  safety limit to prevent indefinite recursion in situations in which the
   --  delimiter is never found. Buffered writing functionality is provided by the
   --  `Root_Buffer_Type` that the type descends from.

   BS_Recursion_Limit_Error : exception;
   --  The packages only custom exception, raised to indicate that the
   --  specified delimiter was not found before the `Unique_Buffer_Type`
   --  instance's `Recursion_Limit` was reached.
   --  ! Should we have custom exceptions for when element types are too small to
   --  correctly hold SE elements? This would be okay in some situations, but
   --  index types that are too small would always result in `Constraint_Error`
   --  being raised.

   function To_SEA (Delimiter : in Array_Type)
     return Ada.Streams.Stream_Element_Array with Inline;
   --  Convenience function for converting the type provided for `Arary_Type`
   --  into Ada stream elements, primarily for when using a string type as the
   --  `Array_Type` (to avoid having to manually type out the raw character
   --  codes).

   function Read_Until
     (Self      : in out Unique_Buffer_Type;
      Delimiter : in     Ada.Streams.Stream_Element_Array;
      Error     :    out Ada.Exceptions.Exception_Occurrence) return Array_Type;
   --  `Read_Until` continuously reads from the underlying stream, collecting
   --  data on Ada's secondary stack, until the specified `Delimiter` is
   --  encountered. Once it is, the collected data up until the delimiter is
   --  converted to the type provided for `Array_Type` and returns it from the
   --  function. This returned data is usually used to initialize a variable
   --  which is also being declared (continuing to make of Ada's secondary
   --  stack features). Since exceptions raised by functions invoked inside
   --  declaration regions cannot be handled, all exceptions are caught and
   --  passed back via the `Error` out parameter as an
   --  `Ada.Exceptions.Exception_Occurrence` instance, which can then be
   --  dealt with as needed in user-provided code.

   --  ! Warn of the exception(s?) that can't be caught when using `select`.

   --  ! A rule that we will probably have is that `Reader.Buffer_Size` must
   --  always >= than `Delimiter'Length`. We could formally enforce this with
   --  a predcondition.

private

   overriding procedure Read
     (Self : in out Unique_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset) is null;

end Buffered_Streams.Unique_Buffer;
