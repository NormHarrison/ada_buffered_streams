package Buffered_Streams.Agnostic_Buffer is

   type Agnostic_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size : Ada.Streams.Stream_Element_Offset)
   is limited new Root_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size) with null record;

   procedure Prefill (Self : in out Agnostic_Buffer_Type; Bits : in Positive);
   --  ! Consider different name.

   procedure Read
     (Self : in out Agnostic_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

end Buffered_Streams.Agnostic_Buffer;
