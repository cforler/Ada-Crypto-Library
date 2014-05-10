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

with Ada.Text_IO;

package body Crypto.Symmetric.KDF_PBKDF2 is


   --Interface function for static 64 Bytes Output
   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes) is
   begin
      PBKDF2(This     => This,
             Salt     => Salt,
             Password => Password,
             Key      => Key,
             DK_Len   => This.Key_Length);
   end Derive;


   procedure Initialize(This	: out PBKDF2_KDF;
                        Key_Length: in Natural) is
   begin
      This.Key_Length:=Key_Length;
   end Initialize;


   --function for setting Key Length and security parameter, used here for setting round count in F_Function
   procedure Initialize(This		: out PBKDF2_KDF;
                        Key_Length	: in Natural;
                        Round_Count	: in Natural) is
   begin
      This.Key_Length:=Key_Length;
      This.Round_Count:=Round_Count;
   end Initialize;



   --actual derivation function, pure PBKDF2
   procedure PBKDF2(This	: in out PBKDF2_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural) is

      hlen : Integer := Hmac_Package.H.Hash_Type'Size/8;
      DK_Block_Count : Natural;
      Rest : Natural;
      Result_Bytes : Bytes(0..DK_Len-1) := (others => 0 );
      Temp_Bytes : Bytes(0..hlen-1);

   begin

      Error_Output.Put_Line("Password: " & To_String(Password));
      Error_Output.Put_Line("Salt: " & To_String(Salt));
      Error_Output.Put_Line("Parameter: " & Integer'Image(This.Round_Count));

      Error_Output.Put_Line("HLEN: " & Integer'Image(hlen));

      Error_Output.Put_Line("Key :" & Integer'Image(Key'Length) & "Result_Bytes :" & Integer'Image(Result_Bytes'Length));

      --calculating amount of blocks required to fill key given the hash length
      DK_Block_Count := Integer(Float'Ceiling(Float(DK_Len) / Float(hlen)));
      Rest := DK_Len - (DK_Block_Count-1) * hlen;

      Error_Output.Put_Line("DKBK :" & Integer'Image(DK_Block_Count) & "Rest :" & Integer'Image(Rest));

      --looping through blocks of the key, applying F_Function
      for I in 0..DK_Block_Count-1 loop

         if(I = DK_Block_Count-1)
         then
            Temp_Bytes := To_Bytes(F_Function(Salt     => Salt,
                                           	 Password => Password,
                                           	 Count    => This.Round_Count,
                                                 Round    => I+1));
            Result_Bytes(I*hlen..I*hlen+Rest-1) := Temp_Bytes(0..Rest-1);
         else
            Result_Bytes(I*hlen..I*hlen+hlen-1) := To_Bytes(F_Function(Salt     => Salt,
                                           	 Password => Password,
                                           	 Count    => This.Round_Count,
                                                 Round    => I+1));
         end if;

      end loop;
      Error_Output.Put_Line("Key :" & Integer'Image(Key'Length) & " " & "Result_Bytes :" & Integer'Image(Result_Bytes'Length));
      Key := Result_Bytes;
   end PBKDF2;

   --Internal function for applying PRF multiple times
   function F_Function(Salt	: in 	Bytes;
                       Password	: in	Bytes;
                       Count	: in 	Natural;
                       Round	: in 	Natural) return Hmac_Package.H.Hash_Type is
      Result_Block : Hmac_Package.H.Hash_Type ;
      Temp_Block : Hmac_Package.H.Hash_Type;
      mlen : Integer := Hmac_Package.H.Message_Type'Size/8;
      Temp_Bytes : Bytes(0..mlen-1) := (others =>0);
      Temp_Bytes_Old : Bytes(0..mlen-1) := (others =>0);
      hlen : Integer := Hmac_Package.H.Hash_Type'Size/8;
      Salt_Bytes : Bytes(0..Salt'Length+4-1);

      use Hmac_Package;
      Context : HMAC_Context;

      Position : Natural := 0;

   begin

      Temp_Bytes(0..Password'Length-1):=Password;
      Context.Init(Key => To_Message_Type(Temp_Bytes));

      Salt_Bytes := (others => 0);
      Salt_Bytes(0..Salt'Length-1) := Salt;
      Salt_Bytes(Salt'Length..Salt'Length+3):= To_Bytes(Word(Round))(0..3);

      while Position + 64 < Salt_Bytes'Length loop
         Temp_Bytes(0..63) := Salt_Bytes(Position..Position+63);
         Context.Sign(Message_Block => To_Message_Type(Temp_Bytes));
         Position := Position+64;
      end loop;

      Temp_Bytes := (others => 0);
      Temp_Bytes(0..Salt_Bytes'Length - Position -1) := Salt_Bytes(Position..Salt_Bytes'Length-1);
      Context.Final_Sign(Final_Message_Block        => To_Message_Type(Temp_Bytes),
                              Final_Message_Block_Length => Hmac_Package.H.Message_Block_Length_Type(Salt_Bytes'Length - Position),
                              Tag                        => Temp_Block);

      Result_Block:= Temp_Block;

      for I in 2..Count loop
         Temp_Bytes := (others => 0);
         Temp_Bytes(0..hlen-1) := Crypto.Symmetric.KDF_PBKDF2.To_Bytes(Temp_Block);
         Context.Final_Sign(Final_Message_Block        => To_Message_Type(Temp_Bytes),
                                 Final_Message_Block_Length => Hmac_Package.H.Message_Block_Length_Type(hlen),
                                 Tag                        => Temp_Block);

         Result_Block := Result_Block xor Temp_Block;

      end loop;

      return Result_Block;
   end;

   --function for utility, accepting strings and key length (in Bytes)
   procedure Derive(This	: in out PBKDF2_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	Bytes;
                    DK_Len	: in 	Natural) is
   begin
      PBKDF2(This     => This,
             Salt     => To_Bytes(Salt),
             Password => To_Bytes(Password),
             Key      => Key,
             DK_Len   => DK_Len);

   end Derive;




end Crypto.Symmetric.KDF_PBKDF2;
