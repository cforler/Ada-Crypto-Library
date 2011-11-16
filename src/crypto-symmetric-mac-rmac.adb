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


package body Crypto.Symmetric.MAC.RMAC is

   T    : C.Block;
   Key  : C.Key_Type;

   ---------------------------------------------------------------------------

   procedure Init(Key1, Key2 : in Key_Type) is
   begin
      C.Prepare_Key(Key1);
      Key := Key2;
      T := T xor T; -- Reset RMAC
   end Init;

   ---------------------------------------------------------------------------

   procedure Sign(Message_Block : in Block) is
   begin
      C.Encrypt(T xor Message_Block, T);
   end Sign;

   ---------------------------------------------------------------------------

   procedure Final_Sign(Final_Message_Block : in Block;
                        R   : out Key_Type;
                        Tag : out Block) is
   begin
      C.Encrypt(T xor Final_Message_Block, T);
      Read(R);
      C.Prepare_Key(Key xor R);
      C.Encrypt(T, Tag);
      T := T xor T; -- Reset RMAC
   end Final_Sign;

   ---------------------------------------------------------------------------

   procedure Verify(Message_Block : in Block) is
   begin
      C.Encrypt(T xor Message_Block, T);
   end Verify;

   ---------------------------------------------------------------------------

   function Final_Verify(Final_Message_Block : in Block;
                         R : in Key_Type;
                         Tag : in Block)
                        return Boolean is
      Result : Boolean;
   begin
      C.Encrypt(T xor Final_Message_Block, T);
      C.Prepare_Key(Key xor R);
      C.Encrypt(T, T);
      Result := (Tag = T);
      T := T xor T; -- Reset RMAC
      return Result;
   end  Final_Verify;

   ---------------------------------------------------------------------------

   procedure Sign(Message : in Blocks;
                 Key1, Key2 : in Key_Type;
                 R   : out Key_Type;
                 Tag : out Block) is
   begin
      Init(Key1,Key2);

      for I in Message'Range loop
         Sign(Message(I));
      end loop;
      Read(R);
      C.Prepare_Key(Key xor R);
      C.Encrypt(T, Tag);
      T := T xor T; -- Reset RMAC
   end Sign;

   ---------------------------------------------------------------------------

   function Verify(Message : in Blocks;
                   Key1, Key2 : in Key_Type;
                   R   : in Key_Type;
                   Tag : in Block) return Boolean is
      Result : Boolean;
   begin
      Init(Key1,Key2);

      for I in Message'Range loop
         Verify(Message(I));
      end loop;
      C.Prepare_Key(Key xor R);
      C.Encrypt(T, T);
      Result := (Tag = T);
      T := T xor T; -- Reset RMAC
      return Result;
   end Verify;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.MAC.RMAC;
