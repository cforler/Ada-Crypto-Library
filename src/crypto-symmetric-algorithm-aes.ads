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

-- This AES implemaentation is based on Daemen's and and Rijman's
-- "AES Proposal: Rijndael" and the fips-197



package Crypto.Symmetric.Algorithm.AES is
   type Cipherkey_AES128 is private;
   type Cipherkey_AES192 is private;
   type Cipherkey_AES256 is private;
   ---------------------------------------------------------------------------

   procedure Prepare_key128(Key        : in B_Block128;
                             Cipherkey : out Cipherkey_AES128);

   procedure Prepare_key192(Key        : in B_Block192;
                             Cipherkey : out Cipherkey_AES192);

   procedure Prepare_key256(Key        : in B_Block256;
                             Cipherkey : out Cipherkey_AES256);

   ---------------------------------------------------------------------------

   procedure Encrypt128(Cipherkey  : in Cipherkey_AES128;
                        Plaintext  : in B_Block128;
                        Ciphertext : out B_Block128);

   procedure Encrypt192(Cipherkey  : in Cipherkey_AES192;
                        Plaintext  : in B_Block128;
                        Ciphertext : out B_Block128);

   procedure Encrypt256(Cipherkey  : in Cipherkey_AES256;
                        Plaintext  : in B_Block128;
                        Ciphertext : out B_Block128);

   ---------------------------------------------------------------------------

   procedure Decrypt128(Cipherkey  : in Cipherkey_AES128;
                        Ciphertext : in B_Block128;
                        Plaintext  : out B_Block128);

   procedure Decrypt192(Cipherkey  : in  Cipherkey_AES192;
                        Ciphertext : in B_Block128;
                        Plaintext  : out B_Block128);

   procedure Decrypt256(Cipherkey  : in Cipherkey_AES256;
                        Ciphertext : in B_Block128;
                        Plaintext  : out B_Block128);

   ---------------------------------------------------------------------------
   --------------------------PRIVATE------------------------------------------
   ---------------------------------------------------------------------------

private

   -- Number of 32-bit words comprising the (Cipher) Key
   Nk128 : constant Positive:=4;
   Nk192 : constant Positive:=6;
   Nk256 : constant Positive:=8;

   -- Number of columns
   Nb :  constant Positive:=4;

   -- Number of rounds
   Nr128 :  constant Positive:=10+1;
   Nr192 :  constant Positive:=12+1;
   Nr256 :  constant Positive:=14+1;

   -- Number of Roundkeys
   Nr_Key128 : constant Positive:= Nr128*4-1;
   Nr_Key192 : constant Positive:= Nr192*4-1;
   Nr_Key256 : constant Positive:= Nr256*4-1;


   -- Roundkeys for 128-bit blocks
   subtype Key_Range128 is Integer range  0..Nr_Key128;
   subtype Key_Range192 is Integer range  0..Nr_Key192;
   subtype Key_Range256 is Integer range  0..Nr_Key256;
   
   type Roundkey_AES128 is array(Key_Range128) of Word;
   type Roundkey_AES192 is array(Key_Range192) of Word;
   type Roundkey_AES256 is array(Key_Range256) of Word;


   -- Cipherkeys
   type Cipherkey_AES128 is record
      Encrypt_Key : Roundkey_AES128;
      Decrypt_Key : Roundkey_AES128;
   end record;

   type Cipherkey_AES192 is record
      Encrypt_Key : Roundkey_AES192;
      Decrypt_Key : Roundkey_AES192;
   end record;

   type Cipherkey_AES256 is record
      Encrypt_Key : Roundkey_AES256;
      Decrypt_Key : Roundkey_AES256;
   end record;

   -- Make the following function and procedure visible for the oneway part
   procedure Build_Encrypt_Key(Key : in Bytes; Encrypt_Key : in out Words;
                                               Nk : in Positive);
   procedure Build_Decrypt_Key(Encrypt_Key : in Words;
                               Decrypt_Key : out Words);
   function Encrypt(Encrypt_Key : in Words; Plaintext : in B_Block128)
                   return  B_Block128;

   function Decrypt(Decrypt_Key : in Words; Ciphertext : B_Block128)
		   return B_Block128;
   
   pragma Inline (Prepare_key128, Prepare_key192,  Prepare_key256);
   pragma Inline (Encrypt128, Encrypt192, Encrypt256);
   pragma Inline (Decrypt128, Decrypt192, Decrypt256);
   pragma Optimize (Time);
end Crypto.Symmetric.Algorithm.AES;
