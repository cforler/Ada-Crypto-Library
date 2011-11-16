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

package body Crypto.Symmetric.Mac.Hmac is

   K : Message_Type;

   ---------------------------------------------------------------------------

   procedure Reset is
      Ipad : Message_Type;
   begin
      Fill36(Ipad);
      Init;
      Round(K xor Ipad);
   end Reset;

   ---------------------------------------------------------------------------

   procedure Init(Key : in Message_Type) is
   begin
      K := Key;
      Reset;
   end Init;

   ---------------------------------------------------------------------------

   procedure Sign(Message_Block : in Message_Type) is
   begin
      Round(Message_Block);
   end Sign;

   ---------------------------------------------------------------------------

   procedure Final_Sign(Final_Message_Block : in Message_Type;
                        Final_Message_Block_Length :
                        in Message_Block_Length_Type;
                        Tag : out Hash_Type) is
      Opad  : Message_Type;
      Final : Message_Type;
      Len   : Message_Block_Length_Type;
   begin
      Tag :=  Final_Round(Final_Message_Block, Final_Message_Block_Length);

      Fill5C(Opad);
      Init;
      Round(K xor Opad);

      Copy(Tag,Final);
      Len :=  M_B_Length(Tag);

      Tag :=  Final_Round(Final, Len);

      Reset;
   end Final_Sign;

   ---------------------------------------------------------------------------

   procedure Verify(Message_Block : in Message_Type) is
   begin
      Round(Message_Block);
   end Verify;

   ---------------------------------------------------------------------------

   function Final_Verify(Final_Message_Block : in Message_Type;
                         Final_Message_Block_Length :
                         in Message_Block_Length_Type;
                         Tag : in Hash_Type)
                        return Boolean is
      Opad  : Message_Type;
      Final : Message_Type;
      Len   : Message_Block_Length_Type;
      Tag2  : Hash_Type :=
        Final_Round(Final_Message_Block, Final_Message_Block_Length);
   begin
      Fill5C(Opad);
      Init;
      Round(K xor Opad);

      Copy(Tag2,Final);
      Len := M_B_Length(Tag);

      Tag2 :=  Final_Round(Final, Len);

      Reset;

      return Tag = Tag2;

   end Final_Verify;


   ---------------------------------------------------------------------------


end  Crypto.Symmetric.Mac.Hmac;
