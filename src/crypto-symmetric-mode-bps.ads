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

with  Crypto.Symmetric.Blockcipher;
with Crypto.Types.Big_Numbers;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>); 
   with function Length(Block : in BC.Block) return Positive is  <>;
   with function To_Block(Input : in Bytes; OutLen : in Positive ) 
			 return BC.Block is  <>;
   with function To_Bytes(Input : in BC.Block) return Bytes is  <>;
   with procedure Set_Zero(Block : out BC.Block) is <>;

package Crypto.Symmetric.Mode.BPS is
   package Big is new Crypto.Types.Big_Numbers(1024);
   use Big;
   use Big.Utils;
   use Big.Mod_Utils;
   use BC;
   

   Radix : constant Positive := 10;    
   type Numeral is mod Radix;
   type Numerals is array (Integer range <>) of Numeral;
   
   procedure Init(Key : in Key_Type; Initial_Value : in B_Block64);
   procedure Set_IV(Initial_Value : in B_Block64);
   
   procedure Encrypt(Plaintext  : in Numerals; Ciphertext : out Numerals);
   procedure Decrypt(Ciphertext : in Numerals; Plaintext  : out Numerals);
   
   ---------------------------------------------------------------------------
   -----------------------------PRIVATE---------------------------------------
   ---------------------------------------------------------------------------

private
   function "+"(Left, Right : Numerals) return Numerals;
   function "-"(Left, Right : Numerals) return Numerals;
   function BC_Encrypt(Plaintext : in Numerals) return Numerals;
   function BC_Decrypt(Ciphertext : in Numerals) return Numerals;
   function To_Big_Unsigned(N : Numerals) return Big_Unsigned;
   S : Big_Unsigned := Big_Unsigned_Zero + Mod_Type(Radix);
   Rounds : constant Positive := 8;
   
   pragma Inline (Init, Encrypt, Decrypt, Set_IV);
   pragma Optimize (Time);

   end  Crypto.Symmetric.Mode.BPS;
