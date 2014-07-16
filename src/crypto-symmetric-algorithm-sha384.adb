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

with Crypto.Symmetric.Algorithm.SHA_Utils;
use  Crypto.Symmetric.Algorithm.SHA_Utils;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body  Crypto.Symmetric.Algorithm.SHA384 is

   procedure Init(Hash_Value : out DW_Block512) is
   begin
      Init_SHA2;
      Hash_Value(0) := 16#cbbb9d5dc1059ed8#;
      Hash_Value(1) := 16#629a292a367cd507#;
      Hash_Value(2) := 16#9159015a3070dd17#;
      Hash_Value(3) := 16#152fecd8f70e5939#;
      Hash_Value(4) := 16#67332667ffc00b31#;
      Hash_Value(5) := 16#8eb44a8768581511#;
      Hash_Value(6) := 16#db0c2e0d64f98fa7#;
      Hash_Value(7) := 16#47b5481dbefa4fa4#;
   end Init;

   ---------------------------------------------------------------------------

   procedure Round(Message_Block : in DW_Block1024;
                   Hash_Value : in out DW_Block512) is
   begin
      Round_SHA2(Message_Block, Hash_Value);
   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block   : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024;
                        Hash_Value           : DW_Block512)
                       return DW_Block384 is
      H : constant DWords(DW_Block384'Range) := DWords(Final_Round_SHA2(Last_Message_Block,
									Last_Message_Length,
									Hash_Value))(0..5);
   begin
      return DW_Block384(H);
   end Final_Round;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in Bytes; Hash_Value : out DW_Block384) is
      K : constant Natural :=  Message'Length/128;
      L : constant Natural :=  Message'Length mod 128;
      LM : Natural := Message'First;
      H : DW_Block512;
      M : DWords(DW_Block1024'Range) := (others=>0);

   begin
      Init(H);

      for I in 1..K loop
	 declare
	    T : constant DWords := To_DWords(Message(LM .. LM+127));
	    begin
	       Round_SHA2(DW_Block1024(T),H);
	    end;
	 LM := LM+128;
      end loop;

      if L /=  0 then
         LM := L/8;
         if L mod 8 = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) :=
           To_DWords(Message(Message'Last-(L-1)..Message'Last));
      end if;
      Hash_Value := Final_Round(DW_Block1024(M), L, H);
   end Hash;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in String; Hash_Value : out DW_Block384) is
   begin
      Hash(To_Bytes(Message), Hash_Value);
   end Hash;
   ---------------------------------------------------------------------------

   procedure F_Hash(Filename : in String; Hash_Value : out DW_Block384) is
      Size     : Integer;
      H        : DW_Block512;                -- Hashvalue
      Mcounter : Message_Counter_Type:=0;
      Bcounter : To_DWord_Counter_Type:=0;
      M        : DW_Block1024 := (others=>0);  -- Message_Block
      B        : Byte_DWord := (others=>0);
      Buf      : Bytes(0..128);
      Fd       : File_Descriptor;
      FilePath : constant string := Filename & ASCII.NUL;

   begin
      Fd :=  Open_Read (FilePath'Address, Binary);
      if fd = invalid_fd then
         raise FILE_OPEN_ERROR;
      end if;

      Init(H);

      loop
         Size := Read(Fd, Buf'Address , Buf'Last);

         if Size = Buf'Last then
	    declare
	       T : constant Dwords := To_DWords(Buf(0..127));
	    begin
	       Round_SHA2(DW_Block1024(T),H);
	       end;
         elsif Size < 0 then raise FILE_READ_ERROR;
         else
            for I in 0..Size-1 loop
               B(Integer(Bcounter)):= Buf(I);
               Bcounter:=Bcounter+1;
               if Bcounter = 0 then
                  M(Integer(Mcounter)):= To_DWord(B);
                  B:=(others=>0);
                  Mcounter:= Mcounter+1;
               end if;
            end loop;
            exit;
         end if;
      end loop;

      Close(Fd);

      if BCounter /= 0 then
         M(Integer(MCounter)):= To_DWord(B);
      end if;

      Hash_Value := Final_Round(M, Message_Block_Length1024(Size), H);

   end F_Hash;

   procedure Init(This 		: in out SHA384_Context) is
   begin
      This.Utils_Context.Init_SHA2;
      This.Hash_Value(0) := 16#cbbb9d5dc1059ed8#;
      This.Hash_Value(1) := 16#629a292a367cd507#;
      This.Hash_Value(2) := 16#9159015a3070dd17#;
      This.Hash_Value(3) := 16#152fecd8f70e5939#;
      This.Hash_Value(4) := 16#67332667ffc00b31#;
      This.Hash_Value(5) := 16#8eb44a8768581511#;
      This.Hash_Value(6) := 16#db0c2e0d64f98fa7#;
      This.Hash_Value(7) := 16#47b5481dbefa4fa4#;
   end Init;

   procedure Round(This 	: in out 	SHA384_Context;
                   Message_Block: in 		DW_Block1024) is
   begin
      This.Utils_Context.Round_SHA2(Message_Block, This.Hash_Value);
   end Round;

   function Final_Round (This 		    : in out SHA384_Context;
                        Last_Message_Block  : DW_Block1024;
                        Last_Message_Length : Message_Block_Length1024)
                         return DW_Block384 is
      H : constant DWords(DW_Block384'Range) := DWords(
                                                       Final_Round_SHA2(
                                                         This.Utils_Context,
                                                         Last_Message_Block,
                                                         Last_Message_Length,
                                                         This.Hash_Value)
                                                      )(0..5);

   begin
      return DW_Block384(H);
   end Final_Round;

   ---------------------------------------------------------------------------

end  Crypto.Symmetric.Algorithm.SHA384;

