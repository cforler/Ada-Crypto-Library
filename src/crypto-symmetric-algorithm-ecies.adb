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

with Crypto.Hashfunction_SHA256; use Crypto.Hashfunction_SHA256;
with Crypto.Symmetric.Blockcipher_AES256; 
use Crypto.Symmetric.Blockcipher_AES256;
with Crypto.Symmetric.Mac.Hmac_SHA256; use Crypto.Symmetric.Mac.Hmac_SHA256;

--with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


package body Crypto.Symmetric.Algorithm.ECIES is

-------------------------------------------------------------------------------

   procedure Message_Prepare(Plain 	: out Plain_ECIES;
			     Message	: in String) is
      tmp_B	  : Bytes(Message'Range);
      Tmp_M_Block : B_Block128 := ((0),(0),(0),(0), (0),(0),(0),(0),
				   (0),(0),(0),(0), (0),(0),(0),(0));
      tmp_N 		: Natural;
   begin
      tmp_B := To_Bytes(Message);
      
      for I in 1..(tmp_B'Length) loop
	 tmp_N := I mod 16;
	 tmp_M_Block(tmp_N-1) := tmp_B(I);
	 if tmp_N = 15 then
	    Plain.Message_Map.Insert(Plain.Message_Block_Count, tmp_M_Block);
	    Plain.Message_Block_Count := Plain.Message_Block_Count + 1;
	 elsif (I = tmp_B'Length) then
	    Plain.Message_Map.Insert(Plain.Message_Block_Count, tmp_M_Block);
			end if;
			
      end loop;
   end Message_Prepare;

-------------------------------------------------------------------------------

   procedure Key_Prepare(AES_Key 		: out B_Block256;
			 Mac_Key 		: out W_Block512;
			 Shared_Key	: in Shared_Key_ECDH) is
      tmp_BU_X 	: constant Big_Unsigned := Shared_Key.W.X;
      tmp_BU_L_X	: constant Natural := Length_In_Bytes(tmp_BU_X)-1;
      tmp_B_BU_X	: Bytes(0 .. tmp_BU_L_X);
      
      tmp_BU_Y 	: constant Big_Unsigned := Shared_Key.W.Y;
      tmp_BU_L_Y	: constant Natural := Length_In_Bytes(tmp_BU_Y)-1;
      tmp_B_BU_Y	: Bytes(0 .. tmp_BU_L_Y);
      
      tmp_Hash_in	: Bytes(0 .. (tmp_BU_L_X+tmp_BU_L_Y+5));
      tmp_B_4		: Bytes(0 .. 3) := ((0),(0),(0),(0));
      tmp_B_One	: constant Byte := 1;
      
      tmp_W256		: W_Block256;
      tmp_B256		: B_Block256;
      
   begin
      tmp_B_BU_X:= To_Bytes(tmp_BU_X);
      tmp_B_BU_Y:= To_Bytes(tmp_BU_Y);
      
      
      for I in 0..tmp_BU_L_X loop
	 tmp_Hash_in(I) := tmp_B_BU_X(I);
      end loop;
	   
      for I in 0..tmp_BU_L_Y loop
	 tmp_Hash_in(I + tmp_BU_L_X+1) := tmp_B_BU_Y(I);
      end loop;
      
      -- Generate AES_Key
      tmp_B_4 := tmp_B_4 + tmp_B_One;
      
      tmp_Hash_in(tmp_Hash_in'Length-4)	:= tmp_B_4(0);
      tmp_Hash_in(tmp_Hash_in'Length-3)	:= tmp_B_4(1);
      tmp_Hash_in(tmp_Hash_in'Length-2)	:= tmp_B_4(2);
      tmp_Hash_in(tmp_Hash_in'Length-1)	:= tmp_B_4(3);
      
      
      tmp_W256 := Hash(tmp_Hash_in);
      for I in 0 .. 7 loop
	 tmp_B256(I*4 + 0) := Byte0(tmp_W256(I));
	 tmp_B256(I*4 + 1) := Byte1(tmp_W256(I));
	 tmp_B256(I*4 + 2) := Byte2(tmp_W256(I));
	 tmp_B256(I*4 + 3) := Byte3(tmp_W256(I));
      end loop;
      
      AES_Key:= tmp_B256;
      
      -- Generate Mac_Key
      tmp_B_4 := tmp_B_4 + tmp_B_One;
      tmp_Hash_in(tmp_Hash_in'Length-4)	:= tmp_B_4(0);
      tmp_Hash_in(tmp_Hash_in'Length-3)	:= tmp_B_4(1);
      tmp_Hash_in(tmp_Hash_in'Length-2)	:= tmp_B_4(2);
      tmp_Hash_in(tmp_Hash_in'Length-1)	:= tmp_B_4(3);
      tmp_W256 := Hash(tmp_Hash_in);
      
      for I in 0 .. 7 loop
	 Mac_Key(I) := tmp_W256(I);
      end loop;
      
      tmp_B_4 := tmp_B_4 + tmp_B_One;
      tmp_Hash_in(tmp_Hash_in'Length-4)	:= tmp_B_4(0);
      tmp_Hash_in(tmp_Hash_in'Length-3)	:= tmp_B_4(1);
      tmp_Hash_in(tmp_Hash_in'Length-2)	:= tmp_B_4(2);
      tmp_Hash_in(tmp_Hash_in'Length-1)	:= tmp_B_4(3);
      tmp_W256 := Hash(tmp_Hash_in);
      
      for I in 0 .. 7 loop
	 Mac_Key(I+8) := tmp_W256(I);
      end loop;
   end Key_Prepare;
   
------------------------------------------------------------------------------
   
   procedure Mac_Compute(Mac_Key : in W_Block512;
			 Cipher  : in Cipher_ECIES;
			 Mac     : out W_Block256) is
      tmp_B128: B_Block128 := ((0),(0),(0),(0), (0),(0),(0),(0),
			       (0),(0),(0),(0), (0),(0),(0),(0));
      Tmp_W512 : W_Block512;
      tmp_N    : Natural := 1;
      tmp_C    : Natural := 0;
   begin
      Init(Mac_Key);
      if (tmp_N + 4) < Cipher.Message_Block_Count then
	 for I in 0 .. 3 loop
	    tmp_B128 := Cipher.Cipher_Map.Element(tmp_N + I);
	    
	    tmp_W512(I*4 +0) := To_Word(tmp_B128(0) , tmp_B128(1) ,
					tmp_B128(2) , tmp_B128(3) );
	    tmp_W512(I*4 +1) := To_Word(tmp_B128(4) , tmp_B128(5) ,
					tmp_B128(6) , tmp_B128(7) );
	    tmp_W512(I*4 +2) := To_Word(tmp_B128(8) , tmp_B128(9) ,
					tmp_B128(10), tmp_B128(11));
	    tmp_W512(I*4 +3) := To_Word(tmp_B128(12), tmp_B128(13),
					tmp_B128(14), tmp_B128(15));
	 end loop;
	 
	 Sign(tmp_W512);
	 tmp_N := tmp_N +4;
      else
	 tmp_C := Cipher.Message_Block_Count - tmp_N;
	 for I in 0 .. tmp_C  loop
	    tmp_B128 := Cipher.Cipher_Map.Element(tmp_N + I);
	    tmp_W512(I*4 +0) := To_Word(tmp_B128(0) , tmp_B128(1) ,
					tmp_B128(2) , tmp_B128(3) );
	    tmp_W512(I*4 +1) := To_Word(tmp_B128(4) , tmp_B128(5) ,
					tmp_B128(6) , tmp_B128(7) );
	    tmp_W512(I*4 +2) := To_Word(tmp_B128(8) , tmp_B128(9) ,
					tmp_B128(10), tmp_B128(11));
	    tmp_W512(I*4 +3) := To_Word(tmp_B128(12), tmp_B128(13),
					tmp_B128(14), tmp_B128(15));
	 end loop;
	 
	 for I in tmp_C .. 3 loop
	    tmp_B128 := ((0),(0),(0),(0), (0),(0),(0),(0), 
			 (0),(0),(0),(0), (0),(0),(0),(0));
	    tmp_W512(I*4 +0) := To_Word(tmp_B128(0) , tmp_B128(1) ,
					tmp_B128(2) , tmp_B128(3) );
	    tmp_W512(I*4 +1) := To_Word(tmp_B128(4) , tmp_B128(5) ,
					tmp_B128(6) , tmp_B128(7) );
	    tmp_W512(I*4 +2) := To_Word(tmp_B128(8) , tmp_B128(9) ,
					tmp_B128(10), tmp_B128(11));
	    tmp_W512(I*4 +3) := To_Word(tmp_B128(12), tmp_B128(13),
					tmp_B128(14), tmp_B128(15));
	 end loop;
	 
	 Final_Sign(tmp_W512, (tmp_C*16), Mac);
      end if;
   end Mac_Compute;
   
   ---------------------------------------------------------------------------
   
   procedure Encrypt(Public_Key_A: in Public_Key_ECDH;
		     Shared_Key 	: in Shared_Key_ECDH;
		     Plaintext	: in String;
		     Cipher		: out Cipher_ECIES) is
      tmp_M_Block	: B_Block128;
      Plain 		: Plain_ECIES;
   begin
      Cipher.Public_Point := Public_Key_A.Q;
      
      Message_Prepare(Plain, Plaintext);
      Key_Prepare(Plain.AES_Key, Plain.Mac_Key, Shared_Key);
      
      Prepare_Key(Plain.AES_Key);
      
      for I in 1 .. Plain.Message_Block_Count loop
	 Encrypt(Plain.Message_Map.Element(I), tmp_M_Block);

	 Cipher.Message_Block_Count := Cipher.Message_Block_Count + 1;
	 Cipher.Cipher_Map.Insert(Cipher.Message_Block_Count, tmp_M_Block);
	 
	 Ada.Strings.Unbounded.Append(Cipher.Cipher, 
				      To_String(Bytes(Tmp_M_Block)));
      end loop;
      
      Mac_Compute(Plain.Mac_Key, Cipher, Cipher.Mac);
      
   end Encrypt;
   
   
   
   procedure Decrypt(Shared_Key	: in Shared_Key_ECDH;
		     Cipher	: in Cipher_ECIES;
		     Plaintext	: out Unbounded_String) is
      tmp_M_Block	: B_Block128;
      tmp_Mac		: W_Block256;
      Plain 		: Plain_ECIES;
      MAC_EX		: exception;
   begin
      Key_Prepare(Plain.AES_Key, Plain.Mac_Key, Shared_Key);
      Prepare_Key(Plain.AES_Key);
      
      for I in 1 .. Cipher.Message_Block_Count loop
	 Decrypt(Cipher.Cipher_Map.Element(I) ,tmp_M_Block);
	 Plain.Message_Map.Insert(I, tmp_M_Block);
	 Ada.Strings.Unbounded.Append(Plain.Message, 
				      To_String(Bytes(Tmp_M_Block)));
      end loop;
      
      Mac_Compute(Plain.Mac_Key, Cipher, tmp_Mac);
      
      --Verify
      if Cipher.Mac =  tmp_Mac then
	 Plaintext:= Plain.Message;
      else
	 raise MAC_EX with "Found different MACs.";
      end if;
   end Decrypt;

-------------------------------------------------------------------------------

   procedure Encrypt(Public_Key_A  : in  Public_Key_ECDH;
                     Public_Key_B  : in  Public_Key_ECDH;
		     Private_Key_A : in  Private_Key_ECDH;
                     Plaintext	  : in  String;
                     Cipher        : out Cipher_ECIES) is
      Shared_Key : ECDH.Shared_Key_ECDH;
      CURVE_EX : exception;
   begin
      if ECDH.equal_Public_Key_Curve(Public_Key_A, Public_Key_B) then
         ECDH.Gen_Shared_Private_Key(Public_Key_B, Private_Key_A, Shared_Key);
	 Encrypt(Public_Key_A, Shared_Key, Plaintext, Cipher);
      else
         raise CURVE_EX with "Found different curve or domainparameter.";
      end if;
   end;

-------------------------------------------------------------------------------

   procedure Decrypt(Public_Key_B  : in  Public_Key_ECDH;
                     Private_Key_A : in  Private_Key_ECDH;
                     Cipher        : in  Cipher_ECIES;
                     Plaintext     : out Unbounded_String) is
      Shared_Key : ECDH.Shared_Key_ECDH;
   begin
      ECDH.Gen_Shared_Private_Key(Public_Key_B, Private_Key_A, Shared_Key);
      Decrypt(Shared_Key, Cipher, Plaintext);
   end;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.ECIES;
