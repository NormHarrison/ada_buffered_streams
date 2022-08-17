package body Buffered_Streams is

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out Root_Buffer_Type;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Self.Stream        := Stream;
      Self.Last_Index_W  := 0;
      Self.Last_Index_R  := 0;
      Self.Start_Index_R := Self.Read_Buffer'First;
      --  ! Would assigning an aggregate instead of
      --  individually change anything?
   end Set_Stream;

   ------------
   --  Write --
   ------------

   procedure Write
     (Self : in out Root_Buffer_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Self.Write_Buffer'Length = 0 then
      --  The write buffer is disabled, send `Item` directly across the stream.
         Self.Stream.Write (Item);

      else
      --  The write buffer is enabled...

         if Self.Write_Buffer'Length - Self.Last_Index_W < Item'Length then
         --  But the buffer didn't have enough room to hold `Item`, flush the
         --  buffer and write `Item` directly to the stream.

            Self.Stream.Write (Self.Write_Buffer
              (Self.Write_Buffer'First .. Self.Last_Index_W));

            Self.Stream.Write (Item);

         else
         --  And the buffer has enough room to hold `Item`.

            for Item_Index in Item'Range loop
               Self.Last_Index_W := Self.Last_Index_W + 1;
               Self.Write_Buffer (Self.Last_Index_W) := Item (Item_Index);
            end loop;

            if Self.Last_Index_W = Self.Write_Buffer'Last then
            --  `Item` fit perfectly into the buffer's remaining space, it's
            --  time to flush the buffer to the underlying stream.
               Self.Stream.Write (Self.Write_Buffer);
               Self.Last_Index_W := 0;
            end if;
         end if;

      end if;
   end Write;

   -----------
   -- Flush --
   -----------

   procedure Flush (Self : in out Root_Buffer_Type) is
   begin
      Self.Stream.Write
        (Self.Write_Buffer (Self.Write_Buffer'First .. Self.Last_Index_W));
      --  ! We may want to handle exceptions here.
      Self.Last_Index_W := 0;
   end Flush;

end Buffered_Streams;
