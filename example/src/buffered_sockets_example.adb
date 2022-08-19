with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with GNAT.Sockets; use GNAT.Sockets;

with Buffered_Streams.Unique_Buffer;
with Buffered_Streams.Socket_Streamer;


procedure Buffered_Sockets_Example is

   --subtype Small_Range is Integer range -20 .. 0;

   --type Byte is range 0 .. 255;
   --for Byte'Size use 8;

   --type Byte_Array is array (Positive range <>) of Byte;

   package Buffered_Strings is new Buffered_Streams.Unique_Buffer
     (Index_Type   => Positive,
      Element_Type => Character,
      Array_Type   => String,
      "+"          => "+");

   Delimiter : constant Ada.Streams.Stream_Element_Array :=
     Buffered_Strings.To_SEA ("STOP");

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
      Stream := GNAT.Sockets.Stream (Socket);

      accept Start;

      Connect_Socket (Socket, Server_Address);

      String'Write (Stream, "Hello there, test 12345678.STOP");

      Close_Socket (Socket);
      Free (Stream);

   exception
      when Error : others =>
         Put_Line (Ada.Exceptions.Exception_Information (Error));

   end Socket_Client_Task;

--  Start of main procedure.

   Server_Sock       : Socket_Type;
   Connection_Sock   : Socket_Type;
   Connection_Stream : Buffered_Streams.Socket_Streamer.TCP_Stream_Type;

   Peer_Address : Sock_Addr_Type (Family => Family_Inet);

   Buffered_Socket : aliased Buffered_Strings.Unique_Buffer_Type
     (Read_Buffer_Size  => 300,
      Write_Buffer_Size => 0,
      Recursion_Limit   => 5);

begin
   Create_Socket (Server_Sock, Family_Inet, Socket_Stream);
   Bind_Socket   (Server_Sock, Server_Address);
   Listen_Socket (Server_Sock);

   Socket_Client_Task.Start;

   Forever : loop
      Accept_Socket
        (Server  => Server_Sock,
         Socket  => Connection_Sock,
         Address => Peer_Address);

      Connection_Stream.Create_Stream (Connection_Sock);
      Buffered_Socket.Set_Stream      (Connection_Stream.To_Access);

      declare
         use type Ada.Exceptions.Exception_ID;

         Error :          Ada.Exceptions.Exception_Occurrence;
         Data  : constant String := Buffered_Socket.Read_Until
                                      (Delimiter, Error);

      begin
         if Ada.Exceptions.Exception_Identity (Error) /=
           Ada.Exceptions.Null_ID
         then
            Put_Line (Ada.Exceptions.Exception_Information (Error));
            exit Forever;
         else
            Put_Line ("Received data ->" & Data & "<-");
            New_Line;
         end if;
      end;

      Close_Socket (Connection_Sock);
   end loop Forever;

exception
   when Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information (Error));

end Buffered_Sockets_Example;
