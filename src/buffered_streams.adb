with Ada.Text_IO; use Ada.Text_IO;


package body Buffered_Streams is

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out Root_Buffer_Type;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Self.Stream := Stream;

      --  ! This is not the best method, as the user could free the
      --  underlying access type out from underneath us. One solution
      --  to this could be to have the user pass us the actual object
      --  that represents the `Stream_Access` access underneath along
      --  with the function for creating a `Stream_Access` out of that
      --  object's specific type. In this case, we would be in complete
      --  control of the access to the underlying object.

      Self.Last_Index_W  := Self.Write_Buffer'First;
      Self.Last_Index_R  := Self.Read_Buffer'First;
      Self.Start_Index_R := Self.Read_Buffer'First;
   end Set_Stream;

   ------------
   --  Write --
   ------------

   procedure Write
     (Self : in out Root_Buffer_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Item_Index : Ada.Streams.Stream_Element_Offset := Item'First;

   begin
      Put_Line ("Item'First is:" & Ada.Streams.Stream_Element_Offset'Image
        (Item'First) & " (should always be 0).");

      if Self.Write_Buffer_Size <= 1 then
         Self.Stream.Write (Item);
         return;
      end if;

      loop
         Self.Last_Index_W := Self.Last_Index_W + 1;

         Self.Write_Buffer (Self.Last_Index_W) := Item (Item_Index);

         if Self.Last_Index_W = Self.Write_Buffer'Last then
         --  If our local buffer is full, flush it on behalf of the user.

         --  ! For alternate behavior, if we reach the end of the buffer
         --  like this should we instead flush the buffer AND send the
         --  remainder over the stream directly instead of putting it in
         --  the buffer?

            Self.Stream.Write (Self.Write_Buffer);
            Self.Last_Index_W := 0;
         end if;

         exit when Item_Index = Item'Last;
         --  ! Should be safe for comparison, since `Item` should always be
         --  zero based, while `Self.Write_Buffer` is always 1 based.

         Item_Index := Item_Index + 1;
      end loop;

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
