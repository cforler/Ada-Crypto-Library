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

with  Crypto.Symmetric.Hashfunction;

generic
   with package H is new Crypto.Symmetric.Hashfunction(<>);

   with function "xor"
     (Left, Right : H.Message_Type)  return H.Message_Type is <>;
   with procedure Fill36 (Ipad : out  H.Message_Type) is <>;
   with procedure Fill5C (Opad : out  H.Message_Type) is <>;
   with procedure Copy
     (Source : in H.Hash_Type; Dest : out H.Message_Type) is <>;

package Crypto.Symmetric.Mac.Hmac is
   use H;

    procedure Init(Key : in Message_Type);

    procedure Sign(Message_Block : in Message_Type);

    procedure Final_Sign
      (Final_Message_Block        : in Message_Type;
       Final_Message_Block_Length : in Message_Block_Length_Type;
       Tag                        : out Hash_Type);


    procedure Verify(Message_Block : in Message_Type);

    function Final_Verify
      (Final_Message_Block        : Message_Type;
       Final_Message_Block_Length : Message_Block_Length_Type;
       Tag                        : Hash_Type) return Boolean;


    ---------------------------------------------------------------------------
    --------------------------------PRIVATE------------------------------------
    ---------------------------------------------------------------------------
private
   pragma Inline (Init, Sign);
   pragma Optimize (Time);

end crypto.Symmetric.Mac.Hmac;
