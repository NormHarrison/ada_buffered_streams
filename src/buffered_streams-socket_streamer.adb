package body Buffered_Streams.Socket_Streamer is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Stream : in out TCP_Stream_Type;
      Socket : in     GNAT.Sockets.Socket_Type)
   is
   begin
      Stream.Socket := Socket;
   end Initialize;

   ---------------

   procedure Initialize
     (Stream     : in out UDP_Stream_Type;
      Socket     : in     GNAT.Sockets.Socket_Type;
      To_Address : in     GNAT.Sockets.Sock_Addr_Type)
   is
   begin
      Stream.To_Address := To_Address;
      Stream.Socket     := Socket;
   end Initialize;

   ---------------
   -- To_Socket --
   ---------------

   function To_Socket (Stream : in TCP_Stream_Type)
     return GNAT.Sockets.Socket_Type is (Stream.Socket);

   ---------------
   -- To_Access --
   ---------------

   function To_Access (Stream : in out TCP_Stream_Type)
     return GNAT.Sockets.Stream_Access is (Stream'Unrestricted_Access);

   ---------------------------
   -- Get_Last_From_Address --
   ---------------------------

   function Get_Last_From_Address (Stream : in UDP_Stream_Type)
     return GNAT.Sockets.Sock_Addr_Type is (Stream.From_Address);

   --------------------
   -- Set_To_Address --
   --------------------

   procedure Set_To_Address
     (Stream     : in out UDP_Stream_Type;
      To_Address : in     GNAT.Sockets.Sock_Addr_Type)
   is
   begin
      Stream.To_Address := To_Address;
   end Set_To_Address;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self : in out TCP_Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Self.Socket,
         Item   => Item,
         Last   => Last,
         Flags  => GNAT.Sockets.No_Request_Flag);
   end Read;

   -------------------------

   overriding procedure Read
     (Self : in out UDP_Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      GNAT.Sockets.Receive_Socket
        (Socket => Self.Socket,
         Item   => Item,
         Last   => Last,
         From   => Self.From_Address,
         Flags  => GNAT.Sockets.No_Request_Flag);
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : in out TCP_Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Last : Ada.Streams.Stream_Element_Offset;

   begin
      loop

         GNAT.Sockets.Send_Socket
           (Socket => Self.Socket,
            Item   => Item,
            Last   => Last,
            To     => null,
            Flags  => GNAT.Sockets.No_Request_Flag);

         exit when Last = Item'Last or else Last = Item'First - 1;

      end loop;
   end Write;

   --------------------------

   overriding procedure Write
     (Self : in out UDP_Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Last : Ada.Streams.Stream_Element_Offset;

   begin
      loop

         GNAT.Sockets.Send_Socket
           (Socket => Self.Socket,
            Item   => Item,
            Last   => Last,
            To     => Self.To_Address'Access,
            Flags  => GNAT.Sockets.No_Request_Flag);

         exit when Last = Item'Last or else Last = Item'First - 1;

      end loop;
   end Write;

end Buffered_Streams.Socket_Streamer;
