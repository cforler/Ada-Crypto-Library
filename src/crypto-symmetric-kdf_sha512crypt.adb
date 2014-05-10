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

with Ada; use Ada;
with Crypto.Symmetric.Algorithm.SHA512;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Crypto.Debug_Put;


package body Crypto.Symmetric.KDF_SHA512Crypt is

   --Interface and core function
   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Base64_String) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;

      Salt_Bytes : Bytes(0..Salt'Length-1) := Salt(Salt'Range);
      Password_Bytes : Bytes(0..Password'Length-1) := Password(Password'Range);

      Digest_A_Bytes : Bytes(0..127):= (others =>0);
      Digest_A_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Context;
      Digest_A_Hash_Result  : DW_Block512;
      Digest_A_Length: Natural := 0;

      Digest_B_Bytes : Bytes(0..127):= (others =>0);
      Digest_B_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Context;
      Digest_B_Hash_Result : DW_Block512;
      Digest_B_Length: Natural := 0;

      Digest_C_Bytes : Bytes(0..127):= (others =>0);
      Digest_C_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Context;
      Digest_C_Hash_Result  : DW_Block512;
      Digest_C_Length: Natural := 0;

      Bytes_For_Rounds : Bytes(0..63);

      Digest_DP_Bytes : Bytes(0..127):= (others =>0);
      Digest_DP_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Context;
      Digest_DP_Hash_Result : DW_Block512;
      Digest_DP_Hash_Result_Bytes : Bytes(0..63);
      Digest_DP_Length: Natural := 0;

      Digest_DS_Bytes : Bytes(0..127):= (others =>0);
      Digest_DS_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Context;
      Digest_DS_Hash_Result : DW_Block512;
      Digest_DS_Hash_Result_Bytes : Bytes(0..63);
      Digest_DS_Length: Natural := 0;

      Big_B_Block : DW_Block1024;
      Sixtyfour_Bytes : Bytes(0..63);

      P_Value : Bytes(Password_Bytes'Range);

      S_Value : Bytes(Salt_Bytes'Range);

      Single_Byte : Byte;

      Final_String : String(1..90) := (others=>'_');

      Final_Input_Bytes : Bytes(0..2);

      Return_Block : W_Block512 := (others =>0);

      Cnt : Natural;

      Temp_Bytes : Bytes(0..63);

   begin

      Digest_A_Hash.Init;


      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      Error_Output.Put_Line("ADDING:");
      for I in 0..Password_Bytes'Length-1 loop
         Error_Output.Put(To_Hex(Password_Bytes(I)));
      end loop;
      Error_Output.New_Line;

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      Error_Output.Put_Line("ADDING:");
      for I in Salt_Bytes'Range loop
         Error_Output.Put(To_Hex(Salt_Bytes(I)));
      end loop;
      Error_Output.New_Line;


      Digest_B_Hash.Init;

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_B_Bytes,
                Digest_Bytes_Length => Digest_B_Length,
                Digest_Hash         => Digest_B_Hash);

      Digest_B_Hash_Result := Digest_B_Hash.Final_Round(Last_Message_Block  => To_DW_Block1024(Digest_B_Bytes),
                                                        Last_Message_Length => Digest_B_Length);


      Error_Output.Put_Line("Point A :");
      for I in to_bytes(Digest_B_Hash_Result)'range loop
         Error_Output.Put(To_Hex(to_bytes(Digest_B_Hash_Result)(I)));
      end loop;
      Error_Output.New_Line;

      Cnt := Password_Bytes'Length;

      while Cnt>64 loop
         Add_Bytes(Bytes_To_Add        => To_Bytes(Digest_B_Hash_Result),
                   Digest_Bytes        => Digest_A_Bytes,
                   Digest_Bytes_Length => Digest_A_Length,
                   Digest_Hash         => Digest_A_Hash);
         Error_Output.Put_Line("ADDING:");
         for I in To_Bytes(Digest_B_Hash_Result)'Range loop
            Error_Output.Put(To_Hex(To_Bytes(Digest_B_Hash_Result)(I)));
         end loop;
         Error_Output.New_Line;

         Cnt := Cnt - 64;
      end loop;

      Temp_Bytes := To_Bytes(Digest_B_Hash_Result);

      Add_Bytes(Bytes_To_Add        => Temp_Bytes(0..Cnt-1),
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);
      Error_Output.Put_Line("ADDING:");
      for I in 0..Cnt-1 loop
         Error_Output.Put(To_Hex(Temp_Bytes(I)));
      end loop;
      Error_Output.New_Line;

      Sixtyfour_Bytes := To_Bytes(D => Digest_B_Hash_Result);


      for I in reverse To_Binary(N => Password_Bytes'Length)'Range loop
         if To_Binary(N => Password_Bytes'Length)(I) = '1' then

            Add_Bytes(Bytes_To_Add        => Sixtyfour_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);
            Error_Output.Put_Line("ADDING:");
            for I in 0..Sixtyfour_Bytes'Length-1 loop
               Error_Output.Put(To_Hex(Sixtyfour_Bytes(I)));
            end loop;
            Error_Output.New_Line;
            Error_Output.Put_Line("1");
         else

            Add_Bytes(Bytes_To_Add        => Password_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);
            Error_Output.Put_Line("ADDING:");
            for I in 0..Password_Bytes'Length-1 loop
               Error_Output.Put(To_Hex(Password_Bytes(I)));
            end loop;
            Error_Output.New_Line;
            Error_Output.Put_Line("0");


         end if;
      end loop;

      -- Finish Digest A

      Big_B_Block := To_DW_Block1024(B => Digest_A_Bytes);

      Digest_A_Hash_Result := Digest_A_Hash.Final_Round(Last_Message_Block  => Big_B_Block,
                                                        Last_Message_Length => Digest_A_Length);

      Error_Output.Put_Line("Point C : ");
      for I in To_Bytes(Digest_A_Hash_Result)'Range loop
         Error_Output.Put(To_Hex(To_Bytes(Digest_A_Hash_Result)(I)));
      end loop;
      Error_Output.New_Line;

      -- Initialize Digest DP
      Digest_DP_Hash.Init;


      -- Add Password password'length times to DP
      for I in 0..Password_Bytes'Length-1 loop
         Add_Bytes(Bytes_To_Add        => Password_Bytes,
                   Digest_Bytes        => Digest_DP_Bytes,
                   Digest_Bytes_Length => Digest_DP_Length,
                   Digest_Hash         => Digest_DP_Hash);
      end loop;

      Big_B_Block := To_DW_Block1024(B => Digest_DP_Bytes);


      -- Finish Digest DP
      Digest_DP_Hash_Result := Digest_DP_Hash.Final_Round(Last_Message_Block  => Big_B_Block,
                                                          Last_Message_Length => Digest_DP_Length);

      Error_Output.Put_Line("Point D : ");
      for I in To_Bytes(Digest_DP_Hash_Result)'Range loop
         Error_Output.Put(To_Hex(To_Bytes(Digest_DP_Hash_Result)(I)));
      end loop;

      Digest_DP_Hash_Result_Bytes := To_Bytes(Digest_DP_Hash_Result);

      for I in P_Value'Range loop
         Error_Output.Put_Line(I'Img);
         P_Value(I) := Digest_DP_Hash_Result_Bytes(I mod 64);
      end loop;


      Error_Output.Put_Line("Point E:");
      for I in P_Value'Range loop
         Error_Output.Put(To_Hex(P_Value(I)));
      end loop;
      Error_Output.New_Line;

      Digest_DS_Hash.Init;

      Sixtyfour_Bytes := To_Bytes(D => Digest_A_Hash_Result);
      Single_Byte := Sixtyfour_Bytes(0);

      for I in 0..16+Single_Byte-1 loop
         Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                   Digest_Bytes        => Digest_DS_Bytes,
                   Digest_Bytes_Length => Digest_DS_Length,
                   Digest_Hash         => Digest_DS_Hash);
      end loop;

      Big_B_Block := To_DW_Block1024(B => Digest_DS_Bytes);

      -- Finish Digest DS
      Digest_DS_Hash_Result := Digest_DS_Hash.Final_Round(Last_Message_Block  => Big_B_Block,
                                                          Last_Message_Length => Digest_DS_Length);

      Error_Output.Put_Line("Point F : ");
      for I in to_bytes(Digest_DS_Hash_Result)'Range loop
         Error_Output.Put(To_Hex(to_bytes(Digest_DS_Hash_Result)(I)));
      end loop;

      Digest_DS_Hash_Result_Bytes := To_Bytes(Digest_DS_Hash_Result);

      for I in S_Value'Range loop
         Error_Output.Put_Line(I'Img);
         S_Value(I) := Digest_DS_Hash_Result_Bytes(I mod 64);
      end loop;

      Error_Output.Put_Line("Point G:");
      for I in S_Value'Range loop
         Error_Output.Put(To_Hex(S_Value(I)));
      end loop;
      Error_Output.New_Line;

      Bytes_For_Rounds := To_Bytes(Digest_A_Hash_Result);

      for I in 0..This.Round_Count-1 loop
         -- a) start digest C
         Digest_C_Hash.Init;

         -- b) for odd round numbers add the byte sequense P to digest C
         if I mod 2 /= 0 then
            Error_Output.Put_Line("Case A");
            Add_Bytes(Bytes_To_Add        => P_Value,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- c) for even round numbers add digest A/C
         if I mod 2 = 0 then
            Error_Output.Put_Line("Case B");
            Add_Bytes(Bytes_To_Add        => Bytes_For_Rounds,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- d) for all round numbers not divisible by 3 add the byte sequence S
         if I mod 3 /= 0 then
            Add_Bytes(Bytes_To_Add        => S_Value,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- e) for all round numbers not divisible by 7 add the byte sequence P
         if I mod 7 /= 0 then
            Add_Bytes(Bytes_To_Add        => P_Value,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- f) for odd round numbers add digest A/C
         if I mod 2 /= 0 then
            Error_Output.Put_Line("Case C");
            Add_Bytes(Bytes_To_Add        => Bytes_For_Rounds,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- g) for even round numbers add the byte sequence P
         if I mod 2 = 0 then
            Error_Output.Put_Line("Case D");
            Add_Bytes(Bytes_To_Add        => P_Value,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- h) finish digest C
         Big_B_Block := To_DW_Block1024(B => Digest_C_Bytes);
         Digest_C_Hash_Result :=
           Digest_C_Hash.Final_Round(Last_Message_Block  => Big_B_Block,
                                     Last_Message_Length => Digest_C_Length);

         Bytes_For_Rounds := To_Bytes(Digest_C_Hash_Result);

         Digest_C_Bytes := (others => 0);
         Digest_C_Length := 0;

      end loop;

      Error_Output.Put_Line("Point H:");
      for I in Bytes_For_Rounds'Range loop
         Error_Output.Put(To_Hex(Bytes_For_Rounds(I)));
      end loop;
      Error_Output.New_Line;


      Final_Input_Bytes := (Bytes_For_Rounds(0),Bytes_For_Rounds(21),Bytes_For_Rounds(42));
      Final_String(1..4) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(22),Bytes_For_Rounds(43),Bytes_For_Rounds(1));
      Final_String(5..8) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(44),Bytes_For_Rounds(2),Bytes_For_Rounds(23));
      Final_String(9..12) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(3),Bytes_For_Rounds(24),Bytes_For_Rounds(45));
      Final_String(13..16) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(25),Bytes_For_Rounds(46),Bytes_For_Rounds(4));
      Final_String(17..20) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(47),Bytes_For_Rounds(5),Bytes_For_Rounds(26));
      Final_String(21..24) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(6),Bytes_For_Rounds(27),Bytes_For_Rounds(48));
      Final_String(25..28) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(28),Bytes_For_Rounds(49),Bytes_For_Rounds(7));
      Final_String(29..32) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(50),Bytes_For_Rounds(8),Bytes_For_Rounds(29));
      Final_String(33..36) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(9),Bytes_For_Rounds(30),Bytes_For_Rounds(51));
      Final_String(37..40) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(31),Bytes_For_Rounds(52),Bytes_For_Rounds(10));
      Final_String(41..44) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(53),Bytes_For_Rounds(11),Bytes_For_Rounds(32));
      Final_String(45..48) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(12),Bytes_For_Rounds(33),Bytes_For_Rounds(54));
      Final_String(49..52) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(34),Bytes_For_Rounds(55),Bytes_For_Rounds(13));
      Final_String(53..56) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(56),Bytes_For_Rounds(14),Bytes_For_Rounds(35));
      Final_String(57..60) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(15),Bytes_For_Rounds(36),Bytes_For_Rounds(57));
      Final_String(61..64) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(37),Bytes_For_Rounds(58),Bytes_For_Rounds(16));
      Final_String(65..68) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(59),Bytes_For_Rounds(17),Bytes_For_Rounds(38));
      Final_String(69..72) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(18),Bytes_For_Rounds(39),Bytes_For_Rounds(60));
      Final_String(73..76) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(40),Bytes_For_Rounds(61),Bytes_For_Rounds(19));
      Final_String(77..80) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(62),Bytes_For_Rounds(20),Bytes_For_Rounds(41));
      Final_String(81..84) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (0,0,Bytes_For_Rounds(63));
      Final_String(85..88) := Bytes_To_String(B => Final_Input_Bytes);

      Error_Output.Put_Line(Final_String);


      Key := Final_String(1..86);


   end Derive;



   --function for setting Key Length
   procedure Initialize(This	: out SHA512Crypt_KDF;
                        Key_Length: in Natural) is
   begin
      if(Key_Length>86) then
         Ada.Text_IO.Put_Line("Keys longer than 86 are not supported, reducing to 86");
         This.Key_Length := 86;
      else
         This.Key_Length := Key_Length;
      end if;

   end Initialize;


   --function for setting Key Length and security parameter, used here for setting round count in F_Function
   procedure Initialize(This		: out SHA512Crypt_KDF;
                        Key_Length	: in Natural;
                        Round_Count	: in Natural) is
   begin
      if Round_Count < 1000 then This.Round_Count := 1000;
      else if Round_Count > 999999999 then This.Round_Count := 999999999;
         else
            This.Round_Count := Round_Count;
         end if;
      end if;
      This.Initialize(Key_Length => Key_Length);

   end Initialize;


   --Adding Bytes to Digest
   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	Crypto.Symmetric.Algorithm.SHA512.Sha512_Context) is
      Rest_Space : Natural;
      Debug : Boolean := false;
   begin

      Rest_Space := 128 - Digest_Bytes_Length;

      Error_Output.Put_line("Rest Space: ");
      Error_Output.Put(Integer(Rest_Space));
      Error_Output.New_Line;

      Error_Output.Put_line("Add Length: ");
      Error_Output.Put(Integer(Bytes_To_Add'Length));
      Error_Output.New_Line;

      Error_Output.Put_line("Bytes to add: ");
      for I in Bytes_To_Add'Range loop
         Error_Output.Put(To_Hex(Bytes_To_Add(I)));
      end loop;

      Error_Output.New_Line;



      if Rest_Space > Bytes_To_Add'Length then
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Bytes_To_Add'Length-1)
           := Bytes_To_Add;
         Digest_Bytes_Length := Digest_Bytes_Length+Bytes_To_Add'Length;
      else
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Rest_Space-1)
           := Bytes_To_Add(0..Rest_Space-1);

                  Error_Output.Put_line("Digest_Bytes : ");
                  for I in Digest_Bytes'Range loop
                     Error_Output.Put(To_Hex(Digest_Bytes(I)));
                  end loop;


                  Error_Output.Put_Line("ROUND!");

         Digest_Hash.Round(Message_Block => To_DW_Block1024(B => Digest_Bytes));
         Digest_Bytes := (others => 0);
         if debug then

            Error_Output.Put_line("digest_byte range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-Rest_Space-1));
            Error_Output.New_Line;

            Error_Output.Put_line("from range");
            Error_Output.Put(Integer(Rest_Space));
            Error_Output.New_Line;

            Error_Output.Put_line("to range");
            Error_Output.Put(Integer(Bytes_To_Add'Length-1));
            Error_Output.New_Line;
         end if;


         Digest_Bytes(0..Bytes_To_Add'Length-Rest_Space-1) := Bytes_To_Add(Rest_Space .. Bytes_To_Add'Length-1);
         Digest_Bytes_Length := Bytes_To_Add'Length-Rest_Space;
         if debug then
            Error_Output.Put("new digest bytes: ");
            for I in Digest_Bytes'Range loop
               Error_Output.Put(To_Hex(Digest_Bytes(I)));
            end loop;
         end if;

      end if;

   end Add_Bytes;


   --translates 3 bytes into base64 4 char string
   function Bytes_To_String(B : bytes) return String is
      Bits_String : String(1..24) := "000000000000000000000000";
      Bits_1 : String(1..8) := (others=>'0');
      Bits_2 : String(1..8) := (others=>'0');
      Bits_3 : String(1..8) := (others=>'0');
      Bits_1_Reverse : String(1..8);
      Bits_2_Reverse : String(1..8);
      Bits_3_Reverse : String(1..8);

      Return_String : String(1..4);
   begin
      Bits_1(9-To_Binary(N => Natural(B(0)))'Length..8) := To_Binary(N => Natural(B(0)));
      Bits_2(9-To_Binary(N => Natural(B(1)))'Length..8) := To_Binary(N => Natural(B(1)));
      Bits_3(9-To_Binary(N => Natural(B(2)))'Length..8) := To_Binary(N => Natural(B(2)));


      Error_Output.Put_Line("Three Binarys :");
      Error_Output.Put_Line(Integer(B(0))'Img);
      Error_Output.Put_Line(Bits_1);
      Error_Output.Put_Line(Integer(B(1))'Img);
      Error_Output.Put_Line(Bits_2);
      Error_Output.Put_Line(Integer(B(2))'Img);
      Error_Output.Put_Line(Bits_3);

      for I in 1..8 loop
         Bits_1_Reverse(9-I) := Bits_1(I);
         Bits_2_Reverse(9-I) := Bits_2(I);
         Bits_3_Reverse(9-I) := Bits_3(I);
      end loop;



      Bits_String(1..Bits_1'Length):=Bits_1;
      Bits_String(9..8+Bits_2'Length):=Bits_2;
      Bits_String(17..16+Bits_3'Length):=Bits_3;

      Return_String(4):= To_Base64(N => To_Natural(S => Bits_String(1..6)));
      Return_String(3):= To_Base64(N => To_Natural(S => Bits_String(7..12)));
      Return_String(2):= To_Base64(N => To_Natural(S => Bits_String(13..18)));
      Return_String(1):= To_Base64(N => To_Natural(S => Bits_String(19..24)));

      return Return_String;
   end;


   --returns a string of 1 and 0 from a number
   function To_Binary(N: Natural) return String is
      S: String(1 .. 1000); -- more than plenty!
      Left:  Positive := S'First;
      Right: Positive := S'Last;
      package IIO is new Ada.Text_IO.Integer_IO(Integer);
   begin
      IIO.Put(To => S, Item => N, Base => 2);
      while S(Left) /= '#' loop
         Left := Left + 1;
      end loop;
      while S(Right) /= '#' loop
         Right := Right - 1;
      end loop;
      return S(Left+1 .. Right-1);
   end To_Binary;


   --returns a number from a string of 1 and 0
   function To_Natural(S : String) return Natural is
      Output: Natural:= 0;
   begin
      Error_Output.Put_Line(S);
      for I in S'Range loop
         if S(I) = '1' then
            Output:= Output + 2**((S'Length+1-I)-1+S'First-1);
         end if;
      end loop;
      Error_Output.Put_Line(Integer'Image(Output));
      return Output;
   end;


   --translates number into base64 string
   function To_Base64(N : Natural) return Character is
      Base : String := "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   begin
      return Base(N+1);
   end;


end Crypto.Symmetric.KDF_SHA512Crypt;
