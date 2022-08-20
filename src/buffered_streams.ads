with Ada.Streams;


package Buffered_Streams is

   type Root_Buffer_Type
     (Write_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Read_Buffer_Size  : Ada.Streams.Stream_Element_Offset)
   is abstract limited new Ada.Streams.Root_Stream_Type with private;
   --  `Root_Buffer_Type` provides the basic functionality shared between
   --  all descendant buffer types apart of this library, specifically, setting
   --  the underlying stream, buffered wrtiting abilities and the discriminants
   --  that control internal buffer sizes. Being abstract, instances of
   --  this type cannot be instantiated directly.
   --  ! Should the stream be set via a discriminant?

   procedure Set_Stream
     (Self   : in out Root_Buffer_Type;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class);
   --  Sets the underlying stream to be buffered. Invoking this primitive
   --  resets the internal state of the instance to prevent the mixing of
   --  data from two different streams.
   --  ! Pre-condition to ensure `null` isn't being set? Maybe the
   --  `not null` will suffice for this?

   --  ! Should any special handling be added to the two subprograms below
   --  for when the internal `Stream` component is null, maybe pre-conditions?
   --  (though this won't handle issues that arise from concurrent access.)

   procedure Write
     (Self : in out Root_Buffer_Type;
      Item : in     Ada.Streams.Stream_Element_Array);
   --  Provides buffered writing functionality for all descending buffer types.
   --  Unlike buffered reading functionality, the way in which writes are
   --  buffered remains the same between all buffer types. Calls to `Write`,
   --  either via a direct invocation or a dispatching one from the `'Write`
   --  attribute of a data type, copy `Item` to the instances internal buffer,
   --  and continue to do so until the end of the buffer is reached. Once the
   --  write buffer is full, its contents is flushed to the underlying stream
   --  automatically. Manual control of flushing can be obtained via `Flush`.

   procedure Flush (Self : in out Root_Buffer_Type) with Inline;
   --  Forcefully flush the current contents of the write buffer to the
   --  underlying stream.
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
