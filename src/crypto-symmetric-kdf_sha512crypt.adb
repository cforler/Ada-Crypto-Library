with Ada; use Ada;
with Crypto.Symmetric.Algorithm.SHA512;
with Ada.Text_IO;
with Ada.Integer_Text_IO;


package body Crypto.Symmetric.KDF_SHA512Crypt is



   procedure Derive(This	: in out SHA512Crypt_KDF;
                    Salt	: in 	String;
                    Password	: in	String;
                    Key		: out	W_Block512) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;


--        Salt_Bytes : Bytes(0..Salt'Length-1) := To_Bytes(Salt);
--        Password_Bytes : Bytes(0..Password'Length-1) := To_Bytes(Password);
      Salt_Bytes : Bytes(0..15) := To_Bytes("saltstringsaltst");
      Password_Bytes : Bytes(0..11) := To_Bytes("Hello World!");

      Digest_A_Bytes : Bytes(0..127):= (others =>0);
      Digest_A_Hash  : DW_Block512;
      Digest_A_Length: Natural := 0;

      Digest_B_Bytes : Bytes(0..127):= (others =>0);
      Digest_B_Hash  : DW_Block512;
      Digest_B_Length: Natural := 0;

      Digest_C_Bytes : Bytes(0..127):= (others =>0);
      Digest_C_Hash  : DW_Block512;
      Digest_C_Length: Natural := 0;

      Bytes_For_Rounds : Bytes(0..63);

      Digest_DP_Bytes : Bytes(0..127):= (others =>0);
      Digest_DP_Hash  : DW_Block512;
      Digest_DP_Length: Natural := 0;

      Digest_DS_Bytes : Bytes(0..127):= (others =>0);
      Digest_DS_Hash  : DW_Block512;
      Digest_DS_Length: Natural := 0;

      Big_B_Block : DW_Block1024;
      Sixtyfour_Bytes : Bytes(0..63);

      P_Value : Bytes(0.. Password_Bytes'Length);

      S_Value : Bytes(0.. Password_Bytes'Length);

      Single_Byte : Byte;

      Final_String : String(1..86) := (others=>'_');

      Final_Input_Bytes : Bytes(0..2);

      Return_Block : W_Block512 := (others =>0);



   begin
      SHA512.Init(Hash_Value => Digest_A_Hash);

      Add_Bytes(Bytes_To_Add        => Password_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);



      SHA512.Init(Hash_Value => Digest_B_Hash);

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


      Ada.Text_IO.Put_Line("BUFFER CHECK");
      for I in Digest_B_Bytes'Range loop
         Ada.Text_IO.Put(To_Hex(Digest_B_Bytes(I)));
      end loop;
      Ada.Text_IO.New_Line;


      Big_B_Block := To_DW_Block1024(B => Digest_B_Bytes);

      Digest_B_Hash :=
      SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
                         Last_Message_Length => Digest_B_Length,
                         Hash_Value          => Digest_B_Hash);

      Ada.Text_IO.Put_Line("FIRST CHECK");
      for I in To_Bytes(Digest_B_Hash)'Range loop
         Ada.Text_IO.Put(To_Hex(To_Bytes(Digest_B_Hash)(I)));
      end loop;


      for I in 0..Integer(Float'Floor(Float(Password_Bytes 'Length)/64.0))-1 loop
         Add_Bytes(Bytes_To_Add        => To_Bytes(Digest_B_Hash),
                   Digest_Bytes        => Digest_A_Bytes,
                   Digest_Bytes_Length => Digest_A_Length,
                   Digest_Hash         => Digest_A_Hash);
      end loop;

      Sixtyfour_Bytes := To_Bytes(D => Digest_B_Hash);


      Add_Bytes(Bytes_To_Add        => Sixtyfour_Bytes(0..Password_Bytes'Length mod 64),
                Digest_Bytes        => Digest_A_Bytes,
                Digest_Bytes_Length => Digest_A_Length,
                Digest_Hash         => Digest_A_Hash);

      for I in reverse To_Binary(N => Password_Bytes'Length)'Range loop
         if To_Binary(N => Password_Bytes'Length)(I) = '1' then

            Add_Bytes(Bytes_To_Add        => Sixtyfour_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);
         else

            Add_Bytes(Bytes_To_Add        => Password_Bytes,
                      Digest_Bytes        => Digest_A_Bytes,
                      Digest_Bytes_Length => Digest_A_Length,
                      Digest_Hash         => Digest_A_Hash);


         end if;
      end loop;



      -- Finish Digest A

      Big_B_Block := To_DW_Block1024(B => Digest_A_Bytes);

      Digest_A_Hash := SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
                                          Last_Message_Length => Digest_A_Length,
                                          Hash_Value          => Digest_A_Hash);

      -- Initialize Digest DS
      SHA512.Init(Hash_Value => Digest_DP_Hash);


      -- Add Password password'length times to DS
      for I in 0..Password_Bytes'Length-1 loop
         Add_Bytes(Bytes_To_Add        => Password_Bytes,
                   Digest_Bytes        => Digest_DP_Bytes,
                   Digest_Bytes_Length => Digest_DP_Length,
                   Digest_Hash         => Digest_DP_Hash);
      end loop;

      Big_B_Block := To_DW_Block1024(B => Digest_DP_Bytes);


      -- Finish Digest DP
      Digest_DP_Hash := SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
                                           Last_Message_Length => Digest_DP_Length,
                                           Hash_Value          => Digest_DP_Hash);

      for I in 0..Integer(Float'Floor(Float(Password_Bytes 'Length)/64.0))-1 loop
         P_Value(I*20..I*20+19):=To_Bytes(Digest_DP_Hash);
      end loop;

      Sixtyfour_Bytes := To_Bytes(D => Digest_DP_Hash);

      P_Value(Integer(Float'Floor(Float(Password_Bytes 'Length)/64.0))*20..
              Integer(Float'Floor(Float(Password_Bytes 'Length)/64.0))+(Password_Bytes'Length mod 64)-1):=
        Sixtyfour_Bytes(0..(Password_Bytes'Length mod 64)-1);

      for I in P_Value'Range loop
         Ada.Text_IO.Put_Line(To_Hex(P_Value(I)));
      end loop;

      SHA512.Init(Hash_Value => Digest_DS_Hash);

      for I in  To_Bytes(Digest_A_Hash)'range loop
         ada.Text_IO.Put(to_hex(To_Bytes(Digest_A_Hash)(I)));
      end loop;

      Sixtyfour_Bytes := To_Bytes(D => Digest_A_Hash);
      Single_Byte := Sixtyfour_Bytes(0);

      for I in 0..16+Single_Byte-1 loop
         Add_Bytes(Bytes_To_Add        => Salt_Bytes,
                   Digest_Bytes        => Digest_DS_Bytes,
                   Digest_Bytes_Length => Digest_DS_Length,
                   Digest_Hash         => Digest_DS_Hash);
      end loop;

      Big_B_Block := To_DW_Block1024(B => Digest_DS_Bytes);

      -- Finish Digest DS
      Digest_DS_Hash := SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
                                           Last_Message_Length => Digest_DS_Length,
                                           Hash_Value          => Digest_DS_Hash);


--        -- Fill DS in S_Value
--        for I in 0..Integer(Float'Floor(Float(Salt_Bytes 'Length)/64.0))-1 loop
--           S_Value(I*20..I*20+19):=To_Bytes(Digest_DS_Hash);
--        end loop;
--
--        Sixtyfour_Bytes := To_Bytes(D => Digest_DS_Hash);
--
--        S_Value(Integer(Float'Floor(Float(Salt_Bytes 'Length)/64.0))*20..
--                Integer(Float'Floor(Float(Salt_Bytes 'Length)/64.0))+(Salt_Bytes'Length mod 64)-1):=
--          Sixtyfour_Bytes(0..(Salt_Bytes'Length mod 64)-1);
--
--        Bytes_For_Rounds := To_Bytes(Digest_A_Hash);
--
--        for I in 0..4999 loop
--           -- a) start digest C
--           SHA512.Init(Digest_C_Hash);
--
--           -- b) for odd round numbers add the byte sequense P to digest C
--           if not ((I mod 2) = 0) then
--              Add_Bytes(Bytes_To_Add        => P_Value,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--           -- c) for even round numbers add digest A/C
--           if ((I mod 2) = 0) then
--              Add_Bytes(Bytes_To_Add        => Bytes_For_Rounds,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--
--           -- d) for all round numbers not divisible by 3 add the byte sequence S
--           if not((I mod 3) = 0) then
--              Add_Bytes(Bytes_To_Add        => S_Value,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--
--           -- e) for all round numbers not divisible by 7 add the byte sequence P
--           if not((I mod 7) = 0) then
--              Add_Bytes(Bytes_To_Add        => P_Value,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--           -- f) for odd round numbers add digest A/C
--           if not ((I mod 2) = 0) then
--              Add_Bytes(Bytes_To_Add        => Bytes_For_Rounds,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--           -- g) for even round numbers add the byte sequence P
--           if ((I mod 2) = 0) then
--              Add_Bytes(Bytes_To_Add        => P_Value,
--                        Digest_Bytes        => Digest_C_Bytes,
--                        Digest_Bytes_Length => Digest_C_Length,
--                        Digest_Hash         => Digest_C_Hash);
--           end if;
--
--           -- h) finish digest C
--           Big_B_Block := To_DW_Block1024(B => Digest_C_Bytes);
--        	 Digest_C_Hash :=
--             SHA512.Final_Round(Last_Message_Block  => Big_B_Block,
--                                Last_Message_Length => Digest_C_Length,
--                                Hash_Value          => Digest_C_Hash);
--
--           Bytes_For_Rounds := To_Bytes(Digest_C_Hash);
--
--
--        end loop;

      Final_Input_Bytes := (Bytes_For_Rounds(42),Bytes_For_Rounds(21),Bytes_For_Rounds(0));
      Final_String(1..4) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(1),Bytes_For_Rounds(43),Bytes_For_Rounds(22));
      Final_String(5..8) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(23),Bytes_For_Rounds(2),Bytes_For_Rounds(44));
      Final_String(9..12) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(45),Bytes_For_Rounds(24),Bytes_For_Rounds(3));
      Final_String(13..16) := Bytes_To_String(B => Final_Input_Bytes);

      Final_Input_Bytes := (Bytes_For_Rounds(4),Bytes_For_Rounds(46),Bytes_For_Rounds(25));
      Final_String(17..20) := Bytes_To_String(B => Final_Input_Bytes);

      ada.Text_IO.Put_Line(Final_String);


      Key := Return_Block;


   end Derive;

   ----------------------------- Add Bytes -------------------------------------


   procedure Add_Bytes(Bytes_To_Add		: in 		Bytes;
                       Digest_Bytes		: in out 	Bytes;
                       Digest_Bytes_Length	: in out	Natural;
                       Digest_Hash		: in out	DW_Block512) is
      package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;
      Rest_Space : Natural;
      Debug : Boolean := false;
   begin

      --Ada.Integer_Text_IO.Put(Bytes_To_Add'Length);

      Rest_Space := 128 - Digest_Bytes_Length;

      if debug then
         Ada.Text_IO.Put_line("Rest Space: ");
      	 Ada.Integer_Text_IO.Put(Integer(Rest_Space));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_line("Add Length: ");
         Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put_line("Bytes to add: ");
            if True then
               for I in Bytes_To_Add'Range loop
                  Ada.Text_IO.Put(To_Hex(Bytes_To_Add(I)));
               end loop;
      	    end if;

         Ada.Text_IO.New_Line;
      end if;



      if Rest_Space > Bytes_To_Add'Length then
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Bytes_To_Add'Length-1)
           := Bytes_To_Add;
         Digest_Bytes_Length := Digest_Bytes_Length+Bytes_To_Add'Length;
      else
         Digest_Bytes(Digest_Bytes_Length..Digest_Bytes_Length+Rest_Space-1)
           := Bytes_To_Add(0..Rest_Space-1);
         SHA512.Round(Message_Block => To_DW_Block1024(B => Digest_Bytes),
                      Hash_Value    => Digest_Hash);
         Digest_Bytes := (others => 0);
         if debug then

            Ada.Text_IO.Put_line("digest_byte range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-Rest_Space-1));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_line("from range");
            Ada.Integer_Text_IO.Put(Integer(Rest_Space));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_line("to range");
            Ada.Integer_Text_IO.Put(Integer(Bytes_To_Add'Length-1));
            Ada.Text_IO.New_Line;
         end if;


         Digest_Bytes(0..Bytes_To_Add'Length-Rest_Space-1) := Bytes_To_Add(Rest_Space .. Bytes_To_Add'Length-1);
         Digest_Bytes_Length := Bytes_To_Add'Length-Rest_Space;
         if debug then
            Ada.Text_IO.Put("new digest bytes: ");
            for I in Digest_Bytes'Range loop
               Ada.Text_IO.Put(To_Hex(Digest_Bytes(I)));
            end loop;
         end if;

      end if;

   end Add_Bytes;

   ----------------- Bytes to String -------------------------------------------

   function Bytes_To_String(B : bytes) return String is
      Bits_String : String(1..24) := "000000000000000000000000";
      Bits_1 : String := To_Binary(N => Natural(B(0)));
      Bits_2 : String := To_Binary(N => Natural(B(1)));
      Bits_3 : String := To_Binary(N => Natural(B(2)));
      Return_String : String(1..4);
   begin
      Bits_String(1..Bits_1'Length):=Bits_1;
      Bits_String(9..8+Bits_2'Length):=Bits_2;
      Bits_String(17..16+Bits_3'Length):=Bits_3;

      Return_String(1):= To_Base64(N => To_Natural(S => Bits_String(1..6)));
      Return_String(2):= To_Base64(N => To_Natural(S => Bits_String(7..12)));
      Return_String(3):= To_Base64(N => To_Natural(S => Bits_String(13..18)));
      Return_String(4):= To_Base64(N => To_Natural(S => Bits_String(19..24)));

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
      Ada.Text_IO.Put_Line(S);
      for I in S'Range loop
         if S(I) = '1' then
            Output:= Output + 2**((S'Length+1-I)-1+S'First-1);
         end if;
      end loop;
      Ada.Text_IO.Put_Line(Integer'Image(Output));
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
      This.Security_Parameter := Parameter;
      return true;
   end;



end Crypto.Symmetric.KDF_SHA512Crypt;
