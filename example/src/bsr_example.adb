with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with GNAT.Sockets; use GNAT.Sockets;

with Buffered_Stream_Reading;


procedure BSR_Example is

   package Buffered_String_Reading is new Buffered_Stream_Reading
     (Index_Type   => Positive,
      Element_Type => Character,
      Array_Type   => String,
      "+"          => "+");

   package BSR renames Buffered_String_Reading;

   Delimiter : constant Ada.Streams.Stream_Element_Array :=
     Buffered_String_Reading.To_SEA ("STOP");

   Server_Address : constant Sock_Addr_Type :=
     (Family => Family_Inet,
      Port   => 55211,
      Addr   => Loopback_Inet_Addr);

   ------------------------
   -- Socket_Client_Task --
   ------------------------

   task Socket_Client_Task is
      entry Start;
   end Socket_Client_Task;

   task body Socket_Client_Task is
      Socket : Socket_Type;
      Stream : Stream_Access;

   begin
      Create_Socket (Socket, Family_Inet, Socket_Stream);

      accept Start;

      Connect_Socket (Socket, Server_Address);

      Stream := GNAT.Sockets.Stream (Socket);

      String'Write (Stream, "Hello there, test 12345678.STOP");

      Free (Stream);
      Close_Socket (Socket);

   exception
      when Error : others =>
         Put_Line (Ada.Exceptions.Exception_Information (Error));

   end Socket_Client_Task;

--  Start of main procedure.

   Server_Sock     : Socket_Type;
   Connection_Sock : Socket_Type;

   Peer_Address : Sock_Addr_Type (Family => Family_Inet);

   Reader : BSR.Reader_Type
     (Buffer_Size     => 300,
      Recursion_Limit => 5);

begin
   Create_Socket (Server_Sock, Family_Inet, Socket_Stream);
   Bind_Socket (Server_Sock, Server_Address);
   Listen_Socket (Server_Sock);

   Socket_Client_Task.Start;

   Forever : loop
      Accept_Socket
        (Server  => Server_Sock,
         Socket  => Connection_Sock,
         Address => Peer_Address);

      BSR.Set_Stream (Reader, GNAT.Sockets.Stream (Connection_Sock));
      --  ! Currently lacking a way to free the stream.

      declare
         Result :          BSR.Read_Error_Kind;
         Data   : constant String := BSR.Read (Reader, Delimiter, Result);

      begin
         Put_Line ("Received data: """ & Data & """");
      end;

      Close_Socket (Connection_Sock);
   end loop Forever;

exception
   when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));

end BSR_Example;
