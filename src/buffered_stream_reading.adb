with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Unchecked_Conversion;


package body Buffered_Stream_Reading is

   pragma Warnings (Off, "types for unchecked conversion have different sizes");

   -------------------
   -- Element_To_SE --
   -------------------

   function Element_To_SE is new Ada.Unchecked_Conversion
     (Source => Element_Type,
      Target => Ada.Streams.Stream_Element);

   --  ! Would placing these inside the declaration region of `To_SEA`
   --  cause a performance impact?

   -------------------
   -- SE_To_Element --
   -------------------

   function SE_To_Element is new Ada.Unchecked_Conversion
     (Source => Ada.Streams.Stream_Element,
      Target => Element_Type);

   ------------------
   -- Index_To_SEO --
   ------------------

--   function Index_To_SEO is new Ada.Unchecked_Conversion
--     (Source => Index_Type,
--      Target => SEO_Type);

   --  ! If converting between types this way causes performance issues,
   --  our only other currently known solutions are only supporting strings,
   --  or defining our own array type that users would have to use instead
   --  of the original one they are imitating (this would be quite inconvenient
   --  on their side and require needless conversions to make work, unless
   --  `Ada.Unchecked_Conversion` is actually smart enough to avoid copying
   --  for trivial conversions, like converting between two array types with
   --  the same element type).

   ------------------
   -- SEO_To_Index --
   ------------------

   function SEO_To_Index is new Ada.Unchecked_Conversion
     (Source => SEO_Type,
      Target => Index_Type);

   pragma Warnings (On, "types for unchecked conversion have different sizes");

   --------------
   -- From_SEA --
   --------------

   --  ! Possibly move inside declaration region of `Read`.

   function From_SEA (Elements : in Ada.Streams.Stream_Element_Array)
     return Array_Type
   is
      use type Ada.Streams.Stream_Element_Offset;

      Elements_Index  : SEO_Type   := Elements'First;
      Converted_Index : Index_Type := Index_Type'First;

      Converted : Array_Type
        (Index_Type'First .. Index_Type'First + SEO_To_Index (Elements'Length));
      --  ! Solution possibly not final.
      --  ! Using `Index_Type'First` could cause problems in the future.

   begin
      loop
         Converted (Converted_Index) :=
           SE_To_Element (Elements (Elements_Index));

         exit when Elements_Index = Elements'Last;

         Elements_Index  := SEO_Type'Succ   (Elements_Index);
         Converted_Index := Index_Type'Succ (Converted_Index);
      end loop;

      return Converted;
   end From_SEA;

   -----------------------------------
   --  Start of public subprograms. --
   -----------------------------------

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Reader : in out Reader_Type;
      Stream : access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Reader.Stream := Stream;
      --  ! This is not the best method, as the user could free the
      --  underlying access type out from underneath us. One solution
      --  to this could be to have the user pass us the actual object
      --  that represents the `Stream_Access` access underneath along
      --  with the function for creating a `Stream_Access` out of that
      --  object's specific type. In this case, we would be in complete
      --  control of the access to the underlying object.
   end Set_Stream;

   ------------
   -- To_SEA --
   ------------

   function To_SEA (Delimiter : in Array_Type)
     return Ada.Streams.Stream_Element_Array
   is
      Converted : Ada.Streams.Stream_Element_Array (1 .. Delimiter'Length);

      Converted_Index : SEO_Type   := Converted'First;
      Delimiter_Index : Index_Type := Delimiter'First;

   begin
      loop
         Converted (Converted_Index) :=
           Element_To_SE (Delimiter (Delimiter_Index));

         exit when Delimiter_Index = Delimiter'Last;

         Converted_Index := SEO_Type'Succ   (Converted_Index);
         Delimiter_Index := Index_Type'Succ (Delimiter_Index);
      end loop;

      return Converted;
   end To_SEA;

   ----------
   -- Read --
   ----------

   function Read
     (Reader    : in out Reader_Type;
      Delimiter : in     Ada.Streams.Stream_Element_Array;
      Error     :    out Read_Error_Kind) return Array_Type
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;
      --  ! Consider `use`ing these elsewhere, possibly in private part of
      --  specification?

      pragma Warnings
        (Off, "variable ""Null_User_Array"" is read but never assigned");

      pragma Warnings
        (Off, "variable ""Null_SE_Array"" is read but never assigned");

      Null_User_Array : Array_Type (Index_Type'Last .. Index_Type'First);
      Null_SE_Array   : Ada.Streams.Stream_Element_Array
        (SEO_Type'Last .. SEO_Type'First);

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

         Put_Line ("Inside `Contains_Delimiter`:"
           & Character'Val (10)
           & "`Slice_Start` is"
           & SEO_Type'Image (Slice_Start)
           & Character'Val (10)
           & "`Slice_End` is:"
           & SEO_Type'Image (Slice_End)
           & Character'Val (10)
           & "`Elements'Last` is"
           & SEO_Type'Image (Elements'Last)
           & Character'Val (10)
           & "Elements'First is"
           & SEO_Type'Image (Elements'First));

         if Delimiter'Length <= Elements'Length then
         --  !                  ^ Used to be `Reader.Last_Index`.
            Search : loop

               if Elements (Slice_Start .. Slice_End) = Delimiter then
                  Position := Slice_Start;
                  Found    := True;

                  Put_Line ("Delimiter WAS found.");

                  if Slice_End /= Elements'Last then
                  --  The data that was available for us to consume
                  --  consisted of more than 1 logical message, we need to
                  --  start the next read from where we left off in the buffer.

                     Reader.Start_Index := Slice_End + 1;
                     --  ! If the user's delimiter is longer than 1, the
                     --  situation where `Reader.Start_Index` ends up being
                     --  `Reader.Buffer'Last` can cause an unnecessary search
                     --  through `Reader.Buffer` upon the next read even though
                     --  it's not possible for the delimiter to be inside it.
                  else
                  --  All the data that was available for us to consume
                  --  was part of the same message, we can start the next
                  --  read directly from the stream.

                     Reader.Start_Index := Reader.Buffer'First;
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
         --  ! Is this ^ a copy?
         Recursion_Count   : in Natural) return Ada.Streams.Stream_Element_Array
      is
         Delim_Position : SEO_Type;

      begin
         Reader.Stream.Read
           (Item => Reader.Buffer,
            Last => Reader.Last_Index);

         if Reader.Last_Index = Reader.Buffer'First - 1 then
            Error := REK_Stream_Closed;
            return Null_SE_Array;
         end if;

         Put_Line ("Inside `Read_Again` at recursion level"
           & Natural'Image (Recursion_Count));

         Put_Line ("Contents of `Previous_Elements` inside `Read_Again`.");
         for Index in Previous_Elements'Range loop
            Put (Ada.Streams.Stream_Element'Image (Previous_Elements (Index)));
            Put (',');
         end loop;
         New_Line;

         Put_Line ("Contents of `Reader.Buffer` inside `Read_Again`.");
         for Index in Reader.Buffer
           (Reader.Start_Index .. Reader.Last_Index)'Range
         loop
            Put (Ada.Streams.Stream_Element'Image (Reader.Buffer (Index)));
            Put (',');
         end loop;
         New_Line;

         --------------------------------

         declare
            Efficient_Start : constant SEO_Type :=
              Previous_Elements'Last - (Delimiter'Length - 1);
              --  ! If `Reader.Buffer'Length` is 1 and `Delimiter'Length` is 4,
              --  then `Efficient_Start` is -1, which doesn't work.

            --  ! This is where things start to become incompatible when
            --  `Reader.Buffer'Length`' is less than `Delimiter'Length`.
            --  We could either try to make what we currrently have compatible,
            --  branch to different code when `Reader.Buffer'Length`' <
            --  `Delimiter'Length`, or simply disallow making the buffer this
            --  small, raising an exception is so, or preventing it some other
            --  way. (See example above)

            Combo_Buffer : constant Ada.Streams.Stream_Element_Array :=
              Previous_Elements
                & Reader.Buffer (Reader.Buffer'First  .. Reader.Last_Index);

         begin
            Put_Line ("Inside `Read_Again`:"
              & Character'Val (10)
              & "`Combo_Buffer'First` is"
              & SEO_Type'Image (Combo_Buffer'First)
              & Character'Val (10)
              & "`Efficient_Start` is:"
              & SEO_Type'Image (Efficient_Start)
              & Character'Val (10)
              & "`Combo_Buffer'Last` is"
              & SEO_Type'Image (Combo_Buffer'Last));

            if Contains_Delimiter
              (Elements => Combo_Buffer (Efficient_Start .. Combo_Buffer'Last),
               Position => Delim_Position)
            then
               return Combo_Buffer (Combo_Buffer'First .. Delim_Position - 1);
            else
               Put_Line ("Delimiter NOT found.");

               if Recursion_Count = Reader.Recursion_Limit then
               --  We've reached the recursion limit set on this instance
               --  of the `Reader_Type`, return all data collected so far.

                  Error := REK_Recursion_Limit_Reached;
                  return Previous_Elements & Null_SE_Array;
               end if;

               return Read_Again
                 (Previous_Elements => Previous_Elements & Reader.Buffer,
                  Recursion_Count   => Recursion_Count + 1);
            end if;
         end;
      end Read_Again;

   --  Start of `Read`.

      Delim_Position     :          SEO_Type;
      Active_Start_Index : constant SEO_Type := Reader.Start_Index;
      --  We must make a local copy of `Reader.Start_Index` before the call to
      --  `Contains_Delimiter` below modifies it for the next invocation of
      --  `Read`. This value is only made use of when the delimiter is found
      --  without the need for recursive invocations.

   begin
      Error := REK_No_Error;

      if Reader.Start_Index = Reader.Buffer'First then
      --  Only read more data from the stream right away if the last read
      --  was "clean" and didn't result in more data after the delimiter.

         Put_Line ("Buffer WAS clean.");

         Reader.Stream.Read
           (Item => Reader.Buffer,
            Last => Reader.Last_Index);

         if Reader.Buffer'First - 1 = Reader.Last_Index then
            Error := REK_Stream_Closed;
            return Null_User_Array;
         end if;
      else
         Put_Line ("Buffer was NOT clean.");
      end if;

      if Contains_Delimiter
        (Elements => Reader.Buffer (Reader.Start_Index .. Reader.Last_Index),
         Position => Delim_Position)
      then
         Put_Line ("Delimiter FOUND on first read.");

         Put_Line ("Inside `Read`:"
           & Character'Val (10)
           & "`Reader.Buffer'Length` is"
           & Integer'Image (Reader.Buffer'Length)
           & Character'Val (10)
           & "`Reader.Start_Index` is:"
           & SEO_Type'Image (Reader.Start_Index)
           & Character'Val (10)
           & "`Delim_Position - 1` is"
           & SEO_Type'Image (Delim_Position - 1));

         return From_SEA
           (Reader.Buffer (Active_Start_Index .. Delim_Position - 1));
            --  Subtract by 1 to omit the first character of the delimiter
            --  from the resulting array.
      else
         --  ! Consider using a `declare` statement here to improve readability.

         Put_Line ("Delimiter NOT found, continuing to read.");

         return From_SEA (Read_Again
           (Recursion_Count   => 0,
            Previous_Elements => Reader.Buffer
              (Reader.Start_Index .. Reader.Buffer'Last) & Null_SE_Array));
               --  `Reader.Buffer` is concatenated with `Null_SE_Array` in
               --  order to force a copy to occur.

         --  ! Is there a better way to concatenate the first read inside
         --  `Reader.Buffer`? Should we use `&` and pass in `Null_Array`
         --  for `Previous_Elements` instead?
      end if;
   end Read;

end Buffered_Stream_Reading;
