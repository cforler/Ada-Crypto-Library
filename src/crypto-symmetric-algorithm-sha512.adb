-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

-- All the procedures of this package based on FIPS 180-2


with Ada.Streams.Stream_IO;

package body Crypto.Symmetric.Algorithm.SHA512 is

   procedure Init(Hash_Value : out DW_Block512) is
   begin
      Init_SHA2;
      Hash_Value(0) := 16#6a09e667f3bcc908#;
      Hash_Value(1) := 16#bb67ae8584caa73b#;
      Hash_Value(2) := 16#3c6ef372fe94f82b#;
      Hash_Value(3) := 16#a54ff53a5f1d36f1#;
      Hash_Value(4) := 16#510e527fade682d1#;
      Hash_Value(5) := 16#9b05688c2b3e6c1f#;
      Hash_Value(6) := 16#1f83d9abfb41bd6b#;
      Hash_Value(7) := 16#5be0cd19137e2179#;

   end Init;

   ---------------------------------------------------------------------------

   procedure Round(Message_Block : in DW_Block1024;
                   Hash_Value    : in out DW_Block512) is
   begin
      Round_SHA2(Message_Block, Hash_Value);
   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block  : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024;
                        Hash_Value          : DW_Block512)
                        return DW_Block512 Is
   begin
      return Final_Round_SHA2(Last_Message_Block,
                              Last_Message_Length,
                              Hash_Value);

   end Final_Round;

   ---------------------------------------------------------------------------

   procedure Init(This : in out Sha512_Context) is
   begin
      This.Utils_Context.Init_SHA2;
      This.Hash_Value(0) := 16#6a09e667f3bcc908#;
      This.Hash_Value(1) := 16#bb67ae8584caa73b#;
      This.Hash_Value(2) := 16#3c6ef372fe94f82b#;
      This.Hash_Value(3) := 16#a54ff53a5f1d36f1#;
      This.Hash_Value(4) := 16#510e527fade682d1#;
      This.Hash_Value(5) := 16#9b05688c2b3e6c1f#;
      This.Hash_Value(6) := 16#1f83d9abfb41bd6b#;
      This.Hash_Value(7) := 16#5be0cd19137e2179#;

   end Init;

   ---------------------------------------------------------------------------

   procedure Round(This 	: in out 	Sha512_Context;
                   Message_Block: in 		DW_Block1024) is
   begin
      This.Utils_Context.Round_SHA2(Message_Block, This.Hash_Value);
   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(This 		    : in out Sha512_Context;
                        Last_Message_Block  : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024)
                        return DW_Block512 is
         begin
      return This.Utils_Context.Final_Round_SHA2(Message_Block        => Last_Message_Block,
                                                   Message_Block_Length => Last_Message_Length,
                                                   Hash_Value           => This.Hash_Value);

   end Final_Round;

   ---------------------------------------------------------------------------


   procedure Hash(Message : in Bytes; Hash_Value : out DW_Block512) is
      K : constant Natural :=  Message'Length/128;
      L : constant Natural :=  Message'Length mod 128;
      LM : Natural := Message'First;
      M : DWords(DW_Block1024'Range) := (others=>0);
   begin
      Init(Hash_Value);

      for I in 1..K loop
         declare
            T : constant DWords :=  To_DWords(Message(LM..LM+127));
         begin
            Round(DW_Block1024(T), Hash_Value);
         end;
         LM := LM+128;
      end loop;

      if L /=  0 then
         LM := L/8;
         if L mod 8 = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) := To_DWords(Message(Message'Last-(L-1)..Message'Last));
      end if;
      Hash_Value := Final_Round(DW_Block1024(M), L, Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in String; Hash_Value : out DW_Block512) is
   begin
      Hash(To_Bytes(Message), Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure F_Hash(Filename : String; Hash_Value : out DW_Block512) is
      use Ada.Streams.Stream_IO;

      H        : DW_Block512;                -- Hashvalue
      Buf      : Bytes(1..128);
      Fd       : File_Type;
      Fmode    : constant File_Mode := In_File;
      Remaining_Bytes : Natural := 0;
   begin
      Open(File => Fd,
           Mode => Fmode,
           Name => Filename);

      if not Is_Open(Fd) then
         raise File_Open_Error;
      end if;

      Init(H);

      while not End_Of_File(Fd) loop
         Remaining_Bytes := Natural(Size(Fd) - (Index(Fd)-1));
         if (Remaining_Bytes > Buf'Last ) then
           Bytes'Read(Stream(Fd), Buf);
           Round_SHA2( DW_Block1024(To_DWords(Buf(1..128)) ), H);
         else
            declare
               Last_Bytes : Bytes(1..Integer(Remaining_Bytes));
            begin
               Bytes'Read(Stream(Fd), Last_Bytes);
               --last block
               Buf := (others => 0);
               Buf(1..Last_Bytes'Last) := Last_Bytes;
               Hash_Value := Final_Round( DW_Block1024(To_DWords(Buf(1..128)) ),
                                          Message_Block_Length1024(Remaining_Bytes),
                                          H);
            end;
         end if;
      end loop;

      Close(Fd);
   end F_Hash;


end  Crypto.Symmetric.Algorithm.SHA512;





