with Ada; use Ada;
with Crypto.Symmetric.Algorithm.SHA512;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Crypto.Non_Debug; use Crypto.Non_Debug;


package body Crypto.Symmetric.KDF_SHA512Crypt is



   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	S5C_String) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;



      Salt_Bytes : Bytes(0..Salt'Length-1) := To_Bytes(Salt);
      Password_Bytes : Bytes(0..Password'Length-1) := To_Bytes(Password);

      Digest_A_Bytes : Bytes(0..127):= (others =>0);
      Digest_A_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface;
      Digest_A_Hash_Result  : DW_Block512;
      Digest_A_Length: Natural := 0;

      Digest_B_Bytes : Bytes(0..127):= (others =>0);
      Digest_B_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface;
      Digest_B_Hash_Result : DW_Block512;
      Digest_B_Length: Natural := 0;

      Digest_C_Bytes : Bytes(0..127):= (others =>0);
      Digest_C_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface;
      Digest_C_Hash_Result  : DW_Block512;
      Digest_C_Length: Natural := 0;

      Bytes_For_Rounds : Bytes(0..63);

      Digest_DP_Bytes : Bytes(0..127):= (others =>0);
      Digest_DP_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface;
      Digest_DP_Hash_Result : DW_Block512;
      Digest_DP_Hash_Result_Bytes : Bytes(0..63);
      Digest_DP_Length: Natural := 0;

      Digest_DS_Bytes : Bytes(0..127):= (others =>0);
      Digest_DS_Hash  : Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface;
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

      Put_Line("ADDING:");
      for I in 0..Password_Bytes'Length-1 loop
         Put(To_Hex(Password_Bytes(I)));
      end loop;
      New_Line;

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      Put_Line("ADDING:");
      for I in Salt_Bytes'Range loop
         Put(To_Hex(Salt_Bytes(I)));
      end loop;
      New_Line;


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


      Put_Line("Point A :");
      for I in to_bytes(Digest_B_Hash_Result)'range loop
         Put(To_Hex(to_bytes(Digest_B_Hash_Result)(I)));
      end loop;
      New_Line;

      Cnt := Password_Bytes'Length;

      while Cnt>64 loop
         Add_Bytes(Bytes_To_Add        => To_Bytes(Digest_B_Hash_Result),
                   Digest_Bytes        => Digest_A_Bytes,
                   Digest_Bytes_Length => Digest_A_Length,
                   Digest_Hash         => Digest_A_Hash);
         Put_Line("ADDING:");
         for I in To_Bytes(Digest_B_Hash_Result)'Range loop
            Put(To_Hex(To_Bytes(Digest_B_Hash_Result)(I)));
         end loop;
         New_Line;

         Cnt := Cnt - 64;
      end loop;

      Temp_Bytes := To_Bytes(Digest_B_Hash_Result);

      Add_Bytes(Bytes_To_Add        => Temp_Bytes(0..Cnt-1),
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);
      Put_Line("ADDING:");
      for I in 0..Cnt-1 loop
         Put(To_Hex(Temp_Bytes(I)));
      end loop;
      New_Line;


      --        Digest_A_Hash_Result :=
      --          Digest_A_Hash.Final_Round(Last_Message_Block  => To_DW_Block1024(Digest_A_Bytes),
      --                                    Last_Message_Length => Digest_A_Length);
      --
      --        Put_Line("Point B:");
      --        for I in To_Bytes(Digest_A_Hash_Result)'Range loop
      --           Put(To_Hex(To_Bytes(Digest_A_Hash_Result)(I)));
      --        end loop;
      --        New_Line;




      Sixtyfour_Bytes := To_Bytes(D => Digest_B_Hash_Result);


      for I in reverse To_Binary(N => Password_Bytes'Length)'Range loop
         if To_Binary(N => Password_Bytes'Length)(I) = '1' then

            Add_Bytes(Bytes_To_Add        => Sixtyfour_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);
            Put_Line("ADDING:");
            for I in 0..Sixtyfour_Bytes'Length-1 loop
               Put(To_Hex(Sixtyfour_Bytes(I)));
            end loop;
            New_Line;
            Put_Line("1");
         else

            Add_Bytes(Bytes_To_Add        => Password_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);
            Put_Line("ADDING:");
            for I in 0..Password_Bytes'Length-1 loop
               Put(To_Hex(Password_Bytes(I)));
            end loop;
            New_Line;
            Put_Line("0");


         end if;
      end loop;

      -- Finish Digest A

      Big_B_Block := To_DW_Block1024(B => Digest_A_Bytes);

      Digest_A_Hash_Result := Digest_A_Hash.Final_Round(Last_Message_Block  => Big_B_Block,
                                                        Last_Message_Length => Digest_A_Length);

      Put_Line("Point C : ");
      for I in To_Bytes(Digest_A_Hash_Result)'Range loop
         Put(To_Hex(To_Bytes(Digest_A_Hash_Result)(I)));
      end loop;
      New_Line;

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

      Put_Line("Point D : ");
      for I in To_Bytes(Digest_DP_Hash_Result)'Range loop
         Put(To_Hex(To_Bytes(Digest_DP_Hash_Result)(I)));
      end loop;

      -----------------Korrekt---------------------------------------------

      Digest_DP_Hash_Result_Bytes := To_Bytes(Digest_DP_Hash_Result);

      for I in P_Value'Range loop
         Put_Line(I'Img);
         P_Value(I) := Digest_DP_Hash_Result_Bytes(I mod 64);
      end loop;


      Put_Line("Point E:");
      for I in P_Value'Range loop
         Put(To_Hex(P_Value(I)));
      end loop;
      New_Line;

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

      Put_Line("Point F : ");
      for I in to_bytes(Digest_DS_Hash_Result)'Range loop
         Put(To_Hex(to_bytes(Digest_DS_Hash_Result)(I)));
      end loop;


      Digest_DS_Hash_Result_Bytes := To_Bytes(Digest_DS_Hash_Result);

      for I in S_Value'Range loop
         Put_Line(I'Img);
         S_Value(I) := Digest_DS_Hash_Result_Bytes(I mod 64);
      end loop;


      Put_Line("Point G:");
      for I in S_Value'Range loop
         Put(To_Hex(S_Value(I)));
      end loop;
      New_Line;


      Bytes_For_Rounds := To_Bytes(Digest_A_Hash_Result);

      for I in 0..This.Security_Parameter-1 loop
         -- a) start digest C
         Digest_C_Hash.Init;

         -- b) for odd round numbers add the byte sequense P to digest C
         if I mod 2 /= 0 then
            Put_Line("Case A");
            Add_Bytes(Bytes_To_Add        => P_Value,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- c) for even round numbers add digest A/C
         if I mod 2 = 0 then
            Put_Line("Case B");
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
            Put_Line("Case C");
            Add_Bytes(Bytes_To_Add        => Bytes_For_Rounds,
                      Digest_Bytes        => Digest_C_Bytes,
                      Digest_Bytes_Length => Digest_C_Length,
                      Digest_Hash         => Digest_C_Hash);
         end if;

         -- g) for even round numbers add the byte sequence P
         if I mod 2 = 0 then
            Put_Line("Case D");
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

      Put_Line("Point H:");
      for I in Bytes_For_Rounds'Range loop
         Put(To_Hex(Bytes_For_Rounds(I)));
      end loop;
      New_Line;


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

      Put_Line(Final_String);


      Key := Final_String(1..86);


   end Derive;

   ----------------------------- Add Bytes -------------------------------------


   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	Crypto.Symmetric.Algorithm.SHA512.Sha512_Interface) is
      Rest_Space : Natural;
      Debug : Boolean := false;
   begin

      --Ada.Integer_Text_IO.Put(Bytes_To_Add'Length);

      Rest_Space := 128 - Digest_Bytes_Length;

      if debug then
         Put_line("Rest Space: ");
         Ada.Integer_Text_IO.Put(Integer(Rest_Space));
         New_Line;

         Put_line("Add Length: ");
         Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length));
         New_Line;

         Put_line("Bytes to add: ");
         if True then
            for I in Bytes_To_Add'Range loop
               Put(To_Hex(Bytes_To_Add(I)));
            end loop;
         end if;

         New_Line;
      end if;



      if Rest_Space > Bytes_To_Add'Length then
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Bytes_To_Add'Length-1)
           := Bytes_To_Add;
         Digest_Bytes_Length := Digest_Bytes_Length+Bytes_To_Add'Length;
      else
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Rest_Space-1)
           := Bytes_To_Add(0..Rest_Space-1);

         --           Put_line("Digest_Bytes : ");
         --           for I in Digest_Bytes'Range loop
         --              Put(To_Hex(Digest_Bytes(I)));
         --           end loop;


         --           Put_Line("ROUND!");

         Digest_Hash.Round(Message_Block => To_DW_Block1024(B => Digest_Bytes));
         Digest_Bytes := (others => 0);
         if debug then

            Put_line("digest_byte range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-Rest_Space-1));
            New_Line;

            Put_line("from range");
            Ada.Integer_Text_IO.Put(Integer(Rest_Space));
            New_Line;

            Put_line("to range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-1));
            New_Line;
         end if;


         Digest_Bytes(0..Bytes_To_Add'Length-Rest_Space-1) := Bytes_To_Add(Rest_Space .. Bytes_To_Add'Length-1);
         Digest_Bytes_Length := Bytes_To_Add'Length-Rest_Space;
         if debug then
            Put("new digest bytes: ");
            for I in Digest_Bytes'Range loop
               Put(To_Hex(Digest_Bytes(I)));
            end loop;
         end if;

      end if;

   end Add_Bytes;

   ----------------- Bytes to String -------------------------------------------

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


      Put_Line("Three Binarys :");
      Put_Line(Integer(B(0))'Img);
      Put_Line(Bits_1);
      Put_Line(Integer(B(1))'Img);
      Put_Line(Bits_2);
      Put_Line(Integer(B(2))'Img);
      Put_Line(Bits_3);

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



   -------------- To Binary ----------------------------------------------------

   function To_Binary(N: Natural) return String is
      S: String(1 .. 1000); -- more than plenty!
      Left:  Positive := S'First;
      Right: Positive := S'Last;
      package IIO is new Ada.Text_IO.Integer_IO(Integer);
   begin
      IIO.Put(To => S, Item => N, Base => 2); -- This is the conversion!
      -- Now S is a String with many spaces and some "2#...#" somewhere.
      -- We only need the "..." part without spaces or base markers.
      while S(Left) /= '#' loop
         Left := Left + 1;
      end loop;
      while S(Right) /= '#' loop
         Right := Right - 1;
      end loop;
      return S(Left+1 .. Right-1);
   end To_Binary;

   -------- To Natural ---------------------------------------------------------


   function To_Natural(S : String) return Natural is
      Output: Natural:= 0;
   begin
      Put_Line(S);
      for I in S'Range loop
         if S(I) = '1' then
            Output:= Output + 2**((S'Length+1-I)-1+S'First-1);
         end if;
      end loop;
      Put_Line(Integer'Image(Output));
      return Output;
   end;

   ---------------------- To Base 64 -------------------------------------------
   function To_Base64(N : Natural) return Character is
      Base : String := "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   begin
      return Base(N+1);
   end;


   ----------------------------Initialize---------------------------------------

   function Initialize(This		: out SHA512Crypt_KDF;
                       Parameter	: in	Natural) return Boolean is
   begin
      if Parameter < 1000 then This.Security_Parameter := 1000;
      else if parameter > 999999999 then This.Security_Parameter := 999999999;
         else
            This.Security_Parameter := Parameter;
         end if;
      end if;
      return true;
   end;



end Crypto.Symmetric.KDF_SHA512Crypt;
