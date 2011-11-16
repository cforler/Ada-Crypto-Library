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


package body Crypto.Symmetric.Algorithm.Tripledes.Oneway is


   procedure Prepare_Oneway_Key(Key       : in B_Block192;
                                Cipherkey : out Cipherkey_Oneway_TDES) is
   begin
      -- Build an EDE-key
      Build_Encrypt_Key(B_Block64(Key( 0.. 7)), Cipherkey(0)); -- E
      Build_Decrypt_Key(B_Block64(Key( 8..15)), Cipherkey(1)); -- D
      Build_Encrypt_Key(B_Block64(Key(16..23)), Cipherkey(2)); -- E

   end Prepare_Oneway_Key;


      procedure Encrypt_Oneway(Cipherkey  : in  Cipherkey_Oneway_TDES;
                               Plaintext  : in  B_Block64;
                               Ciphertext : out B_Block64) is
      begin
         Ciphertext := Des(Cipherkey(2),
                           Des(Cipherkey(1), Des(Cipherkey(0), Plaintext)));
      end Encrypt_Oneway;

      --------------------------Obsolete-----------------------------------

      procedure Obsolete_Prepare_Oneway_Key
        (Key : in B_Block192;
         Cipherkey : out Obsolete_Cipherkey_Oneway_TDES) is
      begin
         Obsolete_Build_Encrypt_Key(B_Block64(Key( 0.. 7)), Cipherkey(0));
         Obsolete_Build_Encrypt_Key(B_Block64(Key( 8..15)), Cipherkey(1));
         Obsolete_Build_Encrypt_Key(B_Block64(Key(16..23)), Cipherkey(2));

         -- Switch Cipherkey(1) to get an EDE-Key
         Obsolete_Build_Decrypt_Key(Cipherkey(1), Cipherkey(1));
      end Obsolete_Prepare_Oneway_Key;


      procedure Obsolete_Encrypt_Oneway
        (Cipherkey  : in  Obsolete_Cipherkey_Oneway_TDES;
         Plaintext  : in  B_Block64;
         Ciphertext : out B_Block64) is
      begin
         Ciphertext := Obsolete_Des
           (Cipherkey(2),
            Obsolete_Des(Cipherkey(1),
                          Obsolete_Des(Cipherkey(0), Plaintext)));
      end Obsolete_Encrypt_Oneway;

end Crypto.Symmetric.Algorithm.Tripledes.Oneway;
