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
      --  ! Would assigning a aggregate instead of individually change anything?
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
      if Self.Write_Buffer'Length - Self.Last_Index_W < Item'Length then

         if Self.Write_Buffer'Length = 0 then
         --  Don't try to send the contents of the write buffer
         --  if it is completely disabled.
            Self.Stream.Write (Self.Write_Buffer
              (Self.Write_Buffer'First .. Self.Last_Index_W));
         end if;

         Self.Stream.Write (Item);
         Self.Last_Index_W := 0;

      else

         for Item_Index in Item'Range loop
            Self.Last_Index_W := Self.Last_Index_W + 1;
            Self.Write_Buffer (Self.Last_Index_W) := Item (Item_Index);
         end loop;

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
