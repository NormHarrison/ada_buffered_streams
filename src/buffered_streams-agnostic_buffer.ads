package Buffered_Streams.Agnostic_Buffer is

   type Agnostic_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size : Ada.Streams.Stream_Element_Offset)
   is limited new Root_Buffer_Type
     (Write_Buffer_Size, Read_Buffer_Size) with null record;
   --  An `Agnostic_Buffer_Type` instance provides a way to move data that will
   --  be read in the future via `Read`, into a user-space buffer, preventing
   --  repeated (and usually constly) system calls to access the original
   --  underlying medium (a file, socket, etc). This ability is most useful
   --  when reading records from a stream, as it prevents each component of
   --  the record from accessing the stream directly, using the buffer instead.
   --  Buffered writing functionality is provided by the `Root_Buffer_Type`
   --  that the type descends from.

   --  ! Consider different name.
   procedure Prefill
     (Self : in out Agnostic_Buffer_Type;
      Bits : in     Positive);
   --  A call to `Prefill` transfers `Bits` amount of data from the underlying
   --  stream into the internal buffer. `Bits` is meant to be `'Size` attribute
   --  of the type that will be read next via `Read`. If `Bits`, once converetd
   --  to bytes, is larger than the internal buffer's size, the buffer's size is
   --  used instead and the remaining data is read directly from the stream
   --  during the corresponding callt to `Read`. This primitive is always meant
   --  to be called before `Read` is invoked, so data can be retrieved from the
   --  internal buffer instead of by repeatedly accessing the stream. If the
   --  read buffer is disabled, `Read_Buffer_Size = 0`, Prefill should not be
   --  invoked.

   --  Documentation to add back if the more flexible implementation is
   --  re-attempted in the future: "The return value is the amount of
   --  available space left in the buffer in bytes. Once this reaches 0,
   --  any additional calls to `Prefill` will begin overwriting data at
   --  the front of the buffer."
   --  This function raises `Ada.IO_Exceptions.End_Error`
   --  if invoked after the underlying stream has been closed.

   procedure Read
     (Self : in out Agnostic_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);
   --  A call to `Read`, via either a direct invocation or a dispatching one,
   --  fills `Item` with as many stream elements as it can from the internal
   --  buffer, falling back to reading from the stream when needed, like when
   --  the prefilled data type was too big to fit in the allocated buffer size.
   --  This primitive can raise any exception that the underlying stream type's
   --  `Read` primitive can.


end Buffered_Streams.Agnostic_Buffer;
