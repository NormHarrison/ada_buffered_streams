--with Ada.Text_IO;              use Ada.Text_IO;
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

   function From_SEA
     (Elements : in Ada.Streams.Stream_Element_Array;
      --  ! Pass in a slice to prevent the need for the `Stop_At` parameter?
      Stop_At  : in SEO_Type) return Array_Type
   is
      use type Ada.Streams.Stream_Element_Offset;

      Elements_Index  : SEO_Type   := Elements'First;
      Converted_Index : Index_Type := Index_Type'First;

      Converted : Array_Type
        (Index_Type'First .. Index_Type'First + SEO_To_Index (Stop_At));
      --  ! Solution possibly not final.
      --  ! Using `Index_Type'First` could cause problems in the future.

   begin
      loop
         Converted (Converted_Index) :=
           SE_To_Element (Elements (Elements_Index));

         exit when Elements_Index = Stop_At;

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

      ------------------------
      -- Contains_Delimiter --
      ------------------------

      function Contains_Delimiter (Position : out SEO_Type) return Boolean is
      --  ! Remember, the buffer of elements we scan is always the one inside
      --  the `Reader` instance, which always contains the most recently
      --  acquired data (which is what we want).

         Slice_Start : SEO_Type := Delimiter'First;
         Slice_End   : SEO_Type := Delimiter'Last;

         Found : Boolean := False;

      begin
         Position := -1;

         if Delimiter'Length <= Reader.Last_Index then
            loop

               if Reader.Buffer (Slice_Start .. Slice_End) = Delimiter then
                  Position := Slice_Start;
                  Found    := True;
                  exit;
               end if;

               exit when Slice_End = Reader.Last_Index;

               Slice_Start := Slice_Start + 1;
               Slice_End   := Slice_End   + 1;

            end loop;
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
      begin
         null;
      end Read_Again;

   --  Start of `Read`.

      pragma Warnings
        (Off, "variable ""Null_Array"" is read but never assigned");

      Null_Array : Array_Type (Index_Type'Last .. Index_Type'First);

      Delim_Position : SEO_Type;

   begin
      Error := REK_No_Error;

      Reader.Stream.Read
        (Item => Reader.Buffer,
         Last => Reader.Last_Index);

      if Reader.Buffer'First - 1 = Reader.Last_Index then
         Error := REK_Stream_Closed;
         return Null_Array;
      end if;

      if Contains_Delimiter (Delim_Position) then
         return From_SEA
           (Elements => Reader.Buffer,
            Stop_At  => Delim_Position - 1);
            --  Subtract by 1 to omit the first character of the delimiter
            --  in the resulting array.
      else
         --  ! Begin recursion.
         return Null_Array;
      end if;
   end Read;

end Buffered_Stream_Reading;
