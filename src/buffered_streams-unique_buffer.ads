with Ada.Streams;
with Ada.Exceptions;


generic
   type Index_Type is (<>);
   type Element_Type is (<>);
   --  ! Maybe allow `Element_Type` to be private?
   type Array_Type is array (Index_Type range <>) of Element_Type;

   with function "+" (Left, Right : in Index_Type) return Index_Type is <>
     with Warnings => Off;


package Buffered_Streams.Unique_Buffer is

   type Unique_Buffer_Type
     (Write_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Read_Buffer_Size  : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit   : Natural)
   is limited new Root_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size) with null record;

   BS_Recursion_Limit_Error : exception;
   --  Should we have custom exceptions for when element types are too small to
   --  correctly hold SE elements? This would be okay in some situations, but
   --  index types that are too small would always result in `Constraint_Error`
   --  being raised.

   function To_SEA (Delimiter : in Array_Type)
     return Ada.Streams.Stream_Element_Array;

   function Read_Until
     (Self      : in out Unique_Buffer_Type;
      Delimiter : in     Ada.Streams.Stream_Element_Array;
      Error     :    out Ada.Exceptions.Exception_Occurrence) return Array_Type;
   --  ! A rule that we will probably have is that `Reader.Buffer_Size` must
   --  always >= than `Delimiter'Length`. We could formally enforce this with
   --  a predcondition.
   --  ! Make part of future function documentation: "Since exceptions raised
   --  by function invoked inside declaration regions cannot be handled,
   --  all exceptions are conveyed via the `Error` parameter as
   --  `Ada.Exceptions.Exception_Occurrence` instance, which can then be
   --  dealt with as needed externally in user-written code."

private

   overriding procedure Read
     (Self : in out Unique_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset) is null;

end Buffered_Streams.Unique_Buffer;
