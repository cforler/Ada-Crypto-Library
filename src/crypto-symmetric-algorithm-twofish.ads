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


-- Towfish is a 128-bit symmetric block cipher
-- This implementation of Twofish only allows key with the following length:
-- 128-bit, 192-bit and 256-bit
-- The implementation based on the twofisch specification
-- "Twofish: A 128-Bit Block Cipher" from Bruce Schneier, John Kelsey,
-- Doug Whiting, David Wagner, Chriss Hall and Niels Ferguson.
-- A few codelines are borrrowed from Niels Fergusons beautiful
-- twofish implementation. ;)

-- NEVER use this API directly.
-- Please, read the user manuale.


package Crypto.Symmetric.Algorithm.Twofish is

   -- Cipherkey Prototyp
   type Cipherkey_Twofish is private;

   -- Cipherkeys
   subtype Cipherkey_Twofish128 is Cipherkey_Twofish;
   subtype Cipherkey_Twofish192 is Cipherkey_Twofish;
   subtype Cipherkey_Twofish256 is Cipherkey_Twofish;

   ---------------------------------------------------------------------------

   -- Exceptions that can not occure ;-)
   -- The only reason why this exception exist
   -- is that gnat interprets sometimes
   -- subtype foobar is Byte range 2..4 as subtype foobar is Byte
   Constrained_Error  : exception;

   ---------------------------------------------------------------------------
   --------------------------------------------------------------------------

   procedure Prepare_Key128(Key       : in B_Block128;
                            Cipherkey : out Cipherkey_Twofish128);

   procedure Prepare_Key192(Key       : in B_Block192;
                            Cipherkey : out Cipherkey_Twofish192);

   procedure Prepare_Key256(Key       : in B_Block256;
                            Cipherkey : out Cipherkey_Twofish256);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Encrypt128(Cipherkey  : in  Cipherkey_Twofish128;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128);

   procedure Encrypt192(Cipherkey  : in  Cipherkey_Twofish192;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128);

   procedure Encrypt256(Cipherkey  : in  Cipherkey_Twofish256;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   procedure Decrypt128(Cipherkey  : in  Cipherkey_Twofish128;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128);


   procedure Decrypt192(Cipherkey  : in  Cipherkey_Twofish192;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128);


   procedure Decrypt256(Cipherkey  : in  Cipherkey_Twofish256;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128);

   ---------------------------------------------------------------------------
   ---------------------------PRIVATE-----------------------------------------
   ---------------------------------------------------------------------------


private
     -- for 128-bit, 192-bit and 256-bit Keys.
   subtype Roundkey_Twofish is Words(0..39);

   
   procedure Prepare_Key(Key : in Bytes; Roundkey : out Roundkey_Twofish);
   procedure Encrypt(Roundkey   : in  Roundkey_Twofish;
		     Plaintext  : in  B_Block128;
                     Ciphertext : out B_Block128);
    procedure Decrypt(Roundkey   : in  Roundkey_Twofish;
                     Ciphertext : in  B_Block128;
                     Plaintext  : out B_Block128);
     
 
   -- Cipherkey
   type Cipherkey_Twofish is record
      Roundkey :  Roundkey_Twofish;
   end record;

   -- Number of Key-Cycles
   subtype Key_Cycles_Type is Byte range 2..4;

   pragma Inline (Prepare_Key128, Prepare_Key192, Prepare_Key256);
   pragma Inline (Encrypt128, Encrypt192, Encrypt256);
   pragma Inline (Decrypt128, Decrypt192, Decrypt256);

   pragma Optimize (Time);

end  Crypto.Symmetric.Algorithm.Twofish;
