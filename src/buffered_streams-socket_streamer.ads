with GNAT.Sockets;

private with Ada.Streams;


package Buffered_Streams.Socket_Streamer is

   --  ! We could rather greatly decrease the amount of source code
   --  inside the package body if we declared both `To_Address` and
   --  `From_Address` inside the base `TCP_Stream_Type`. This would
   --  allow us to only need one overload of the `Root_Stream_Type`'s
   --  `Read` and `Write` primitives. Howver, this means that the
   --  `TCP_Stream_Type` would have a discriminant even though it
   --  wouldn't actually be used.

   type TCP_Stream_Type is tagged limited private;

   procedure Create_Stream
     (Stream : in out TCP_Stream_Type;
      Socket : in     GNAT.Sockets.Socket_Type);

   function To_Socket (Stream : in TCP_Stream_Type)
     return GNAT.Sockets.Socket_Type;

   function To_Access (Stream : in out TCP_Stream_Type)
     return GNAT.Sockets.Stream_Access;

   --  ! Consider making these types non-limited.

   type UDP_Stream_Type (Address_Family : GNAT.Sockets.Family_Type) is
     new TCP_Stream_Type with private;

   procedure Create_Stream
     (Stream     : in out UDP_Stream_Type;
      Socket     : in     GNAT.Sockets.Socket_Type;
      To_Address : in     GNAT.Sockets.Sock_Addr_Type);

   function Get_Last_From_Address (Stream : in UDP_Stream_Type)
     return GNAT.Sockets.Sock_Addr_Type;

   procedure Set_To_Address
     (Stream     : in out UDP_Stream_Type;
      To_Address : in     GNAT.Sockets.Sock_Addr_Type);

private

   type TCP_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Socket : GNAT.Sockets.Socket_Type;
   end record;

   overriding procedure Read
     (Self : in out TCP_Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out TCP_Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);


   type UDP_Stream_Type
     (Address_Family : GNAT.Sockets.Family_Type)
   is new TCP_Stream_Type with record
      To_Address   : aliased GNAT.Sockets.Sock_Addr_Type (Address_Family);
      From_Address :         GNAT.Sockets.Sock_Addr_Type (Address_Family);
   end record;

   overriding procedure Read
     (Self : in out UDP_Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Self : in out UDP_Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);


end Buffered_Streams.Socket_Streamer;
