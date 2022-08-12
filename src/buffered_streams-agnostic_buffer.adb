with Ada.Text_IO; use Ada.Text_IO;


package body Buffered_Streams.Agnostic_Buffer is

   use type Ada.Streams.Stream_Element_Offset;
   --  ! Possibly move to more specific declaration regions?

   -------------
   -- Prefill --
   -------------

   --  ! Consider different name.

   procedure Prefill
     (Self : in out Agnostic_Buffer_Type;
      Bits : in     Positive)
   is
      Type_Size_In_Bytes : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset (Bits / 8);

   begin
      Self.Start_Index_R := Self.Read_Buffer'First;
      --  ! Should we always restart at the beginning of the buffer like this?
      --  Doing so means the user must always follow a call to `Prefill` with a
      --  call to `Read`

      Self.Stream.Read
        (Item => Self.Read_Buffer (Self.Start_Index_R .. Type_Size_In_Bytes),
         Last => Self.Last_Index_R);
      --  ! How should we handle errors here, allowing the `Ada.IO_Exceptions`
      --  exceptions to bubble upwards?
   end Prefill;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self : in out Agnostic_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Put_Line ("Item'Last is:"
        & Ada.Streams.Stream_Element_Offset'Image (Item'Last));

      Last := Item'First;

      if Item'Length <= (Self.Last_Index_R - Self.Start_Index_R) then
      --  If the size of the data type being read is <= the size of the
      --  data currently available in the buffer, we can read all needed
      --  data from it. `Item'Length` and `Self.Last_Index_R` can be correctly
      --  compared because `Self.Last_Index_R` represents the last index of a
      --  buffer whose first index begins at 1 (so it is equivalent to the
      --  buffer's length).

         loop
            Item (Last) := Self.Read_Buffer (Self.Start_Index_R);
            --  ! Can/should we do this via the `'Address` attribute?

            exit when Self.Start_Index_R = Item'Length or else
                      Self.Start_Index_R = Self.Last_Index_R;

            Last := Last + 1;
            Self.Start_Index_R := Self.Start_Index_R + 1;
         end loop;
      end if;

      if Last = Item'Last then
      --  There was enough data in the buffer to satify the read.
         Self.Start_Index_R := Self.Read_Buffer'First;
         --  ! If we change the behavior of `Prefill`, this will need to be
         --  set to `Self.Last_Index_R` + 1.
      else
      --  More data was needed than was available in the buffer.

         --  ! We need to decide how we want to handle this situation,
         --  we could indicate to the user that they `Prefill` should
         --  always be called with the same type that will be read next
         --  and not handle reading more data from the stream if needed.
         --  Or, we can handle the situation where we read some data for
         --  the type from the buffer, and the rest from the stream.
         --  If this is implemented, we should try keeping it all contained
         --  the loop above.

         Self.Stream.Read (Item, Last);
         --  ! This currently just overwrites anything placed inside `Item` so
         --  far, which is not correct behavior at all. We need to implement
         --  one of the two options in the comment above.
      end if;

   end Read;

end Buffered_Streams.Agnostic_Buffer;
