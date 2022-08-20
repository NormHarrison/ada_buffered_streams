--with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.IO_Exceptions;


package body Buffered_Streams.Unique_Buffer is

   subtype SEO_Type is Ada.Streams.Stream_Element_Offset;

   --------------
   -- From_SEA --
   --------------

   --  ! Possibly move inside declaration region of `Read`.

   function From_SEA (Elements : in Ada.Streams.Stream_Element_Array)
     return Array_Type
   is
      -------------------
      -- SE_To_Element --
      -------------------

      function SE_To_Element is new Ada.Unchecked_Conversion
        (Source => Ada.Streams.Stream_Element,
         Target => Element_Type)
      with Warnings => Off;

      ------------------
      -- SEO_To_Index --
      ------------------

      function SEO_To_Index is new Ada.Unchecked_Conversion
        (Source => SEO_Type,
         Target => Index_Type)
      with Warnings => Off;

      use type Ada.Streams.Stream_Element_Offset;

      Converted : Array_Type (Index_Type'First .. Index_Type'First
        + SEO_To_Index (Elements'Length - 1));
        --  ! Does subtracting 1 here work in every scenario?

      Elements_Index  : SEO_Type   := Elements'First;
      Converted_Index : Index_Type := Index_Type'First;

   begin
      Conversion : loop
         Converted (Converted_Index) := SE_To_Element
           (Elements (Elements_Index));

         exit Conversion when Elements_Index = Elements'Last;

         Elements_Index  := SEO_Type'Succ   (Elements_Index);
         Converted_Index := Index_Type'Succ (Converted_Index);
      end loop Conversion;

      return Converted;
   end From_SEA;

   -----------------------------------
   --  Start of public subprograms. --
   -----------------------------------

   ------------
   -- To_SEA --
   ------------

   function To_SEA (Delimiter : in Array_Type)
     return Ada.Streams.Stream_Element_Array
   is
      Converted : Ada.Streams.Stream_Element_Array (1 .. Delimiter'Length);
      for Converted'Address use Delimiter'Address;

   begin
      return Converted;
   end To_SEA;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Self      : in out Unique_Buffer_Type;
      Delimiter : in     Ada.Streams.Stream_Element_Array;
      Error     :    out Ada.Exceptions.Exception_Occurrence) return Array_Type
   is
      ------------------
      -- Index_To_SEO --
      ------------------

      function Index_To_SEO is new Ada.Unchecked_Conversion
        (Source => Index_Type,
         Target => SEO_Type)
      with Warnings => Off;

      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;
      --  ! Consider `use`ing these elsewhere, possibly in private part of
      --  specification?

      ------------------------
      -- Contains_Delimiter --
      ------------------------

      function Contains_Delimiter
        (Elements : in     Ada.Streams.Stream_Element_Array;
         Position :    out SEO_Type) return Boolean
      is
         Slice_Start : SEO_Type := Elements'First;
         Slice_End   : SEO_Type := Elements'First + (Delimiter'Length - 1);

         Found : Boolean := False;

      begin
         Position := -1;

--         Put_Line ("Inside `Contains_Delimiter`:"
--           & Character'Val (10)
--           & "`Slice_Start` is"
--           & SEO_Type'Image (Slice_Start)
--           & Character'Val (10)
--           & "`Slice_End` is:"
--           & SEO_Type'Image (Slice_End)
--           & Character'Val (10)
--           & "`Elements'Last` is"
--           & SEO_Type'Image (Elements'Last)
--           & Character'Val (10)
--           & "Elements'First is"
--           & SEO_Type'Image (Elements'First));

         if Delimiter'Length <= Elements'Length then
            Search : loop

               if Elements (Slice_Start .. Slice_End) = Delimiter then
                  Position := Slice_Start;
                  Found    := True;

--                  Put_Line ("Delimiter WAS found.");

                  if Slice_End /= Elements'Last then
                  --  The data that was available for us to consume
                  --  consisted of more than 1 logical message, we need to
                  --  start the next read from where we left off in the buffer.

                     Self.Start_Index_R := Slice_End + 1;
                     --  ! If the user's delimiter is longer than 1, the
                     --  situation where `Self.Start_Index_R` ends up being
                     --  `Self.Buffer'Last` can cause an unnecessary search
                     --  through `Self.Buffer` upon the next read even though
                     --  it's not possible for the delimiter to be inside it.
                     --  ! But the first character of the next message can
                     --  reside inside the last index of the read buffer,
                     --  warranting it to be searched.
                  else
                  --  All the data that was available for us to consume
                  --  was part of the same message, we can start the next
                  --  read directly from the stream.

                     Self.Start_Index_R := Self.Read_Buffer'First;
                  end if;

                  exit Search;
               end if;

               exit Search when Slice_End = Elements'Last;

               Slice_Start := Slice_Start + 1;
               Slice_End   := Slice_End   + 1;

            end loop Search;
         end if;

         return Found;
      end Contains_Delimiter;

      ----------------
      -- Read_Again --
      ----------------

      function Read_Again
        (Previous_Elements : in Ada.Streams.Stream_Element_Array;
         Recursion_Count   : in Natural) return Ada.Streams.Stream_Element_Array
      is
         Delim_Position : SEO_Type;

      begin
         Self.Stream.Read
           (Item => Self.Read_Buffer,
            Last => Self.Last_Index_R);

--         if Self.Last_Index_R = Self.Read_Buffer'First - 1 then
--            raise Ada.IO_Exceptions.End_Error with
--              "Stream was closed during read attempt.";
--         end if;
--
--         Put_Line ("Inside `Read_Again` at recursion level"
--           & Natural'Image (Recursion_Count));
--
--         Put_Line ("Contents of `Previous_Elements` inside `Read_Again`.");
--         for Index in Previous_Elements'Range loop
--            Put (Ada.Streams.Stream_Element'Image (Previous_Elements (Index)));
--            Put (',');
--         end loop;
--         New_Line;
--
--         Put_Line ("Contents of `Self.Read_Buffer` inside `Read_Again`.");
--         for Index in Self.Read_Buffer
--           (Self.Start_Index_R .. Self.Last_Index_R)'Range
--         loop
--            Put (Ada.Streams.Stream_Element'Image (Self.Read_Buffer (Index)));
--            Put (',');
--         end loop;
--         New_Line;

         --------------------------------

         declare
            Efficient_Start : constant SEO_Type :=
              Previous_Elements'Last - (Delimiter'Length - 1);
              --  ! If `Self.Buffer'Length` is 1 and `Delimiter'Length` is 4,
              --  then `Efficient_Start` is -1, which doesn't work.

            --  ! This is where things start to become incompatible when
            --  `Self.Buffer'Length`' is less than `Delimiter'Length`.
            --  We could either try to make what we currrently have compatible,
            --  branch to different code when `Self.Buffer'Length`' <
            --  `Delimiter'Length`, or simply disallow making the buffer this
            --  small, raising an exception when it is, or preventing it some
            --  other way. (See example above)

            Combo_Buffer : constant Ada.Streams.Stream_Element_Array :=
              Previous_Elements & Self.Read_Buffer
                (Self.Read_Buffer'First .. Self.Last_Index_R);

         begin
--            Put_Line ("Inside `Read_Again`:"
--              & Character'Val (10)
--              & "`Combo_Buffer'First` is"
--              & SEO_Type'Image (Combo_Buffer'First)
--              & Character'Val (10)
--              & "`Efficient_Start` is:"
--              & SEO_Type'Image (Efficient_Start)
--              & Character'Val (10)
--              & "`Combo_Buffer'Last` is"
--              & SEO_Type'Image (Combo_Buffer'Last));

            if Contains_Delimiter
              (Elements => Combo_Buffer (Efficient_Start .. Combo_Buffer'Last),
               Position => Delim_Position)
            then
               return Combo_Buffer (Combo_Buffer'First .. Delim_Position - 1);
            else
--               Put_Line ("Delimiter NOT found.");

               if Recursion_Count = Self.Recursion_Limit then
               --  We've reached the recursion limit set on this instance of
               --  the `Buffered_Stream_Type`, return all data collected so far.
                  raise BS_Recursion_Limit_Error with
                    "Delimiter was not found before recursion limit of"
                      & Positive'Image (Self.Recursion_Limit);
                  --  ! Raising an exception like this prevents us from being
                  --  able to return the data collected so far. Should we
                  --  revert to assigning directly to the `Error` out parameter
                  --  of the `Read_Until` subprogram, and then returning?
               end if;

               return Read_Again
                 (Previous_Elements => Previous_Elements & Self.Read_Buffer,
                  Recursion_Count   => Recursion_Count + 1);
            end if;
         end;
      end Read_Again;

   --  Start of `Read_Until`.

      Null_User_Array : Array_Type (Index_Type'Last .. Index_Type'First)
        with Warnings => Off;

      Null_SE_Array   : Ada.Streams.Stream_Element_Array
        (SEO_Type'Last .. SEO_Type'First) with Warnings => Off;

      Delim_Position     :          SEO_Type;
      Active_Start_Index : constant SEO_Type := Self.Start_Index_R;
      --  We must make a local copy of `Self.Start_Index_R` before the call to
      --  `Contains_Delimiter` below updates it for the next invocation of
      --  `Read`. This value is only made use of when the delimiter is found
      --  without the need for recursive invocations (i.e. on the first read).

   begin
      Ada.Exceptions.Save_Occurrence
        (Source => Ada.Exceptions.Null_Occurrence,
         Target => Error);

      if Self.Start_Index_R = Self.Read_Buffer'First then
      --  Only read more data from the stream right away if the last read
      --  was "clean" and didn't result in more data after the delimiter.

         Self.Stream.Read
           (Item => Self.Read_Buffer,
            Last => Self.Last_Index_R);

         if Self.Read_Buffer'First - 1 = Self.Last_Index_R then
            raise Ada.IO_Exceptions.End_Error with
              "Stream was closed during the first read attempt.";
         end if;
      end if;

      if Contains_Delimiter
        (Position => Delim_Position,
         Elements => Self.Read_Buffer
                       (Self.Start_Index_R .. Self.Last_Index_R))
      then
      --  The delimiter was found on the first read.

--         Put_Line ("Inside `Read`:"
--           & Character'Val (10)
--           & "`Self.Buffer'Length` is"
--           & Integer'Image (Self.Read_Buffer'Length)
--           & Character'Val (10)
--           & "`Self.Start_Index_R` is:"
--           & SEO_Type'Image (Self.Start_Index_R)
--           & Character'Val (10)
--           & "`Delim_Position - 1` is"
--           & SEO_Type'Image (Delim_Position - 1));

      --if Index_Type'Base'Last < Elements'Length then
      --  ! Was our idea here to try and raise an exception if the data type
      --  the user provided for `Index_Type` was too small to hold the amount
      --  of elements collected from the stream?

         return From_SEA
           (Self.Read_Buffer (Active_Start_Index .. Delim_Position - 1));
            --  Subtract by 1 to omit the first character of the delimiter
            --  from the resulting array.
      else
      --  More data is needed, the delimiter was not found.

--         Put_Line ("Delimiter NOT found, continuing to read.");

         declare
            subtype Slice_Range is SEO_Type range
              Self.Start_Index_R .. Self.Read_Buffer'Last;

            New_Buffer : constant Ada.Streams.Stream_Element_Array := Read_Again
              (Recursion_Count   => 0,
               Previous_Elements => Self.Read_Buffer (Slice_Range) & Null_SE_Array);
            --  `Self.Buffer` is concatenated with `Null_SE_Array` in
            --  order to force a copy to occur.
            --  ! Should we move the slicing and concatenation of
            --  `Self.Read_Buffer` to the declaration region above?
            --  Does this have any implications on performance?

         begin
            return From_SEA (New_Buffer);
         end;
      end if;

   exception
      when Error_Instance : others =>
      --  Handle and gracefully passback all exceptions that are raised within
      --  the `Read` function and all subprograms it invokes (though this
      --  excludes exceptions that are raised in function invocations that
      --  occur inside declaration regions, we can't handle those).

         Ada.Exceptions.Save_Occurrence
           (Source => Error_Instance,
            Target => Error);

         return Null_User_Array;

   end Read_Until;

end Buffered_Streams.Unique_Buffer;
