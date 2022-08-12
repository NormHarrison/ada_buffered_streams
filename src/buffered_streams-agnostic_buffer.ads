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
   --  ! Instead of making the user prefill the buffer with the amount of
   --  anticipated data, have them set a descriminant during declaration
   --  of an instance of the type, where they choose in advance how much
   --  data will be pulled into the buffer each time (though this doesn't
   --  really work fully, since each component of a record causes a new
   --  call to the `Read` primitive ot occur, would we keep track of calls
   --  somehow to prevent this?)

end Buffered_Streams.Agnostic_Buffer;
