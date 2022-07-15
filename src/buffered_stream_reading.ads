with Ada.Streams;


generic
   type Index_Type is (<>);
   type Element_Type is (<>);
   type Array_Type is array (Index_Type range <>) of Element_Type;

   pragma Warnings (Off, "function ""+"" is not referenced");

   with function "+" (Left, Right : in Index_Type) return Index_Type is <>;

package Buffered_Stream_Reading is

   --type Array_Type is array (Positive range <>) of Element_Type;
   --  ! We may have to fallback on this method.

   type Reader_Type
     (Buffer_Size     : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit : Natural) is limited private;
   --  ! Should the stream be set via a discriminant?
   --  ! Should this be a controlled type?

   procedure Set_Stream
     (Reader : in out Reader_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class);


   --  Each value of the `IO_Exception_Kind` enumeration maps to the similarly
   --  named exception of the language-defined package `Ada.IO_Exceptions`.
   --  Since exceptions raised by function invoked inside declaration regions
   --  cannot be handled, IO errors must be conveyed in a different way.

   type Read_Error_Kind is
     (REK_No_Error,
      REK_Stream_Closed,
      REK_Recursion_Limit_Reached);
   --  ! Create a value for indicating an truncated conversion
   --  (when the `Element_Type` the user passed in is smaller than
   --  `Ada.Streams.Stream_Element`) ?

   function To_SEA (Delimiter : in Array_Type)
     return Ada.Streams.Stream_Element_Array;

   function Read
     (Reader    : in out Reader_Type;
      Delimiter : in     Ada.Streams.Stream_Element_Array;
      Error     :    out Read_Error_Kind) return Array_Type;
   --  ! Offer option to start search for the delimiter from the
   --  front or back of the buffer.

private

   subtype SEO_Type is Ada.Streams.Stream_Element_Offset;

   type Reader_Type
     (Buffer_Size     : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit : Natural)
   is limited record
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      --  ! Add a `Connected` component?
      --  ! Add a `Last_Error` component, to replace the `Error` out
      --  parameter of `Read` ?

      Buffer        : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
      Last_Index    : Ada.Streams.Stream_Element_Offset := 0;
      --  ! Change name ^ to something that coincides with `Buffer` more?
      Start_Index   : Ada.Streams.Stream_Element_Offset := 1;
   end record;

end Buffered_Stream_Reading;
