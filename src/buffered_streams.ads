with Ada.Streams;


package Buffered_Streams is

   type Root_Buffer_Type
     (Write_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Read_Buffer_Size  : Ada.Streams.Stream_Element_Offset)
   is abstract limited new Ada.Streams.Root_Stream_Type with private;
   --  ! Should the stream be set via a discriminant, allowing us to make
   --  the component in the record non-null?
   --  ! Should we subtype `Stream_Element_Offset` in order to prevent
   --  Very small and/or negative buffer sizes from being used? We could
   --  also use at 0, 1 or anything below as a way to disable a specific
   --  buffer (i.e. always reading/writing with the stream directly)?

   procedure Set_Stream
     (Self   : in out Root_Buffer_Type;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class);
   --  ! Pre-condition to ensure `null` isn't being set? Maybe the
   --  `not null` will suffice for this?

   --  ! Should any special handling be added to the two subprograms below
   --  for when the internal `Stream` component is null, maybe pre-conditions?
   --  (though this won't handle issues that arise from concurrent access.)

   --  ! Create a way to free the stream that was set. It makes more sense to
   --  offer this now since there's less of a reason to keep a separate,
   --  external access value of the stream (since buffered writing is offered
   --  now too).

   procedure Write
     (Self : in out Root_Buffer_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   procedure Flush (Self : in out Root_Buffer_Type);
   --  ! Return a `Boolean` or `Ada.Exceptions.Exception_Occurrence`?

private

   type Root_Buffer_Type
     (Write_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Read_Buffer_Size  : Ada.Streams.Stream_Element_Offset)
   is abstract limited new Ada.Streams.Root_Stream_Type with record
      --  ! Change index-related variables to something that coincides with
      --  their respective buffer more?

      Stream : access Ada.Streams.Root_Stream_Type'Class;

      Write_Buffer : Ada.Streams.Stream_Element_Array (1 .. Write_Buffer_Size);
      Last_Index_W : Ada.Streams.Stream_Element_Offset := 0;

      Read_Buffer   : Ada.Streams.Stream_Element_Array (1 .. Read_Buffer_Size);
      Last_Index_R  : Ada.Streams.Stream_Element_Offset := 0;
      Start_Index_R : Ada.Streams.Stream_Element_Offset := 1;

   end record;

end Buffered_Streams;
