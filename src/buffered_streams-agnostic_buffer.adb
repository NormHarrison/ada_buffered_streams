--with Ada.Text_IO; use Ada.Text_IO;


package body Buffered_Streams.Agnostic_Buffer is

   use type Ada.Streams.Stream_Element_Offset;

   -------------
   -- Prefill --
   -------------

   --  ! Consider different name.

   procedure Prefill
     (Self : in out Agnostic_Buffer_Type;
      Bits : in     Positive)
   is
--      Actual_Buffer_Length : constant Ada.Streams.Stream_Element_Offset :=
--        Self.Read_Buffer'Length - (if Self.Start_Index_R = 1 then
--                                      0
--                                   else
--                                      Self.Start_Index_R);

      Type_Size_In_Bytes : Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset (Bits / 8);

   begin
      --Self.Start_Index_R := Self.Read_Buffer'First;
      --  ! Old behavior was to always start filling the buffer from its start,
      --  overwriting whatever was present. This demanded a usage pattern of
      --  following every call to `Prefill` with a call to `Read`. We may
      --  return to this behavior eventually.

      --  ! Silently return, or raise an exception if the read buffer is
      --  disabled (`Self.Read_Buffer'Length = 0`) ?

      if Type_Size_In_Bytes > Self.Read_Buffer'Length then
      --  The size of the buffer isn't large enough to hold the data type being
      --  read, only read as much as were able to (the remaining buffer space).
         Type_Size_In_Bytes := Self.Read_Buffer'Length;
      end if;

      Self.Stream.Read
        (Last => Self.Last_Index_R,
         Item => Self.Read_Buffer
                   (Self.Read_Buffer'First .. Type_Size_In_Bytes));

--      if Self.Last_Index_R = Self.Read_Buffer'First - 1 then
--         raise Ada.IO_Exceptions.End_Error;
--      end if;

--      Self.Start_Index_R := Self.Last_Index_R + 1;
--      if Self.Start_Index_R > Self.Read_Buffer'Last then
      --  The buffer is now full, any more calls to `Prefill` will result in
      --  the loss of data.

      --  ! Maybe we should set `Self.Start_Index` to a negative number until
      --  it is reset to 1 by a call to `Read`. That way we can raise an
      --  exception or otherwise warn the user if they are overfilling the
      --  buffer and losing data.
--         Self.Start_Index_R := Self.Read_Buffer'First;
--      end if;

--      return Self.Write_Buffer'Length - Self.Last_Index_R;
      --  ! How should we handle errors here, allowing any exceptions raised by
      --  the underlying `Read` implementation to bubble upwards?
   end Prefill;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self : in out Agnostic_Buffer_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
--      Actual_Buffer_Length : constant Ada.Streams.Stream_Element_Offset :=
--        Self.Last_Index_R - (if Self.Start_Index_R = 1 then
--                                0
--                             else
--                                Self.Start_Index_R);

   begin
      --  ! This entire function should probably re-written, our comments
      --  talk about the code as if it handles the scenario where some data
      --  is read from the buffer and the rest from the stream (a scenario
      --  that would really only occur when the end of the buffer was reached
      --  during a call to `Prefill`), but it doesn't seem like we actually
      --  handled this situation properly.

      if Self.Read_Buffer'Length = 0 then
      --  Read buffer is disabled, obtain `Item` directly from the
      --  underlying stream.
         Self.Stream.Read (Item, Last);

      else
      --  Read buffer is enabled, obtain `Item` from it, using the underlying
      --  stream too if needed.

         Self.Start_Index_R := Self.Read_Buffer'First;
         Last := Item'First;
         --  Confusing naming, but we do this to avoid declaring
         --  another variable.

         loop
            Item (Last) := Self.Read_Buffer (Self.Start_Index_R);

            exit when Self.Start_Index_R = Item'Length or else
                      Self.Start_Index_R = Self.Last_Index_R;

            Last := Last + 1;
            Self.Start_Index_R := Self.Start_Index_R + 1;
         end loop;


         if Last /= Item'Last then
         --  More data was needed than was available in the buffer, obtain
         --  the remaining data directly from the stream.

            Self.Stream.Read
              (Item => Item (Last + 1 .. Item'Last),
               Last => Last);
            --  ! Will `Last` being altered by `Read` affect the slice of `Item`
            --  we pass in?

--            if Last = Item'First - 1 then
--               raise Ada.IO_Exceptions.End_Error;
--            end if;

         end if;
      end if;

--      if Item'Length <= Actual_Buffer_Length then
      --  If the size of the data type being read is <= the size of the
      --  data currently available in the buffer, we can read all needed
      --  data from it. `Item'Length` and `Self.Last_Index_R` can be correctly
      --  compared because `Self.Last_Index_R` represents the last index of a
      --  buffer whose first index begins at 1 (so it is equivalent to the
      --  buffer's length).

--         loop
--            Item (Last) := Self.Read_Buffer (Self.Start_Index_R);
--            --  ! Can/should we do this via the `'Address` attribute?
--
--            exit when Self.Start_Index_R = Item'Length or else
--                      Self.Start_Index_R = Self.Last_Index_R;
--
--            Last := Last + 1;
--            Self.Start_Index_R := Self.Start_Index_R + 1;
--         end loop;
--
--         if Last /= Item'Last then
         --  `Last` must be incremented 1 index past the most recent insertion
         --  when more data must be acquired directly from the stream.
--            Last := Last + 1;
--         end if;
--
--      end if;
--
--      if Last = Item'Last then
      --  There was enough data in the buffer to satify the read.

--         if Self.Start_Index_R = Self.Read_Buffer'Last then
--            Self.Start_Index_R := Self.Read_Buffer'First;
--         else
--            Self.Start_Index_R := Self.Start_Index_R + 1;
--         end if;
--
--      else
      --  More data was needed than was available in the buffer, obtain
      --  the remaining data directly from the stream.

--         Self.Stream.Read
--           (Item => Item (Last .. Item'Last),
--            Last => Last);
         --  ! Will `Last` being altered by `Read` affect the slice of `Item`
         --  we pass in?

--      end if;

   end Read;

end Buffered_Streams.Agnostic_Buffer;
