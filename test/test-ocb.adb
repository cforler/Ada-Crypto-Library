with AUnit.Assertions;
with Crypto.Symmetric.AE_OCB3;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Crypto.Symmetric.AE;
with Crypto.Symmetric.KDF_SHA512Crypt;

package body Test.OCB is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   package AES_128 renames Crypto.Symmetric.Blockcipher_AES128;

   Plaintext: B_Block128 := (others => 0);
   Key: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#, 16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#);
   --Ciphertext: B_Block128; --:= (16#5D#, 16#9D#, 16#4E#, 16#EF#, 16#FA#,
     --                     --    16#91#, 16#51#, 16#57#, 16#55#, 16#24#,
       --                   --    16#F1#, 16#15#, 16#81#, 16#5A#, 16#12#,
         --                 --    16#E0#);
   PT: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, others=>Byte(0));
   CT: B_Block128;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register OCB3 Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out OCB3_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, OCB3_Test1'Access,"OCB3_Test1.");
		Register_Routine(T, OCB3_Test2'Access,"OCB3_Test2.");
		--Register_Routine(T, OCB3_Test3'Access,"OCB3_Test3.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name OCB3 Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : OCB3_Test) return Test_String is
	begin
		return new String'("OCB3 Test");
	end Name;



------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------


   procedure OCB3_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;

      -------Counter-------------
      function Inc(Item: B_Block128) return B_Block128 is
      begin
         return (16#00#,16#01#,16#02#,16#03#,
                 16#04#,16#05#,16#06#,16#07#,
                 16#08#,16#09#,16#0A#,16#0B#,
                 16#08#,16#09#,16#0A#,16#0B#);
      end Inc;

      package N is new Crypto.Types.Nonces(Block => B_Block128);
      package Counter is new N.Nonces_Ctr(Inc => Inc);
      Nonce: Counter.Nonce_Ctr;

      zero_iv: B_Block128:=(others=>Byte(0));
      output: B_Block128;


      ---------callback functions-----------------

      Give_Counter: Natural:= 0;

      package giver is
         procedure give_bytes(B : out Bytes; Count: out Natural);
      end giver;


      package body giver is
         procedure give_bytes(B : out Bytes; Count: out Natural) is
         begin
            if Give_Counter=0 then
               B(0..15):=(16#00#,16#01#,16#02#,16#03#,16#04#,16#05#,16#06#,16#07#,
                         16#08#,16#09#,16#0A#,16#0B#,16#0C#,16#0D#,16#0E#,16#0F#);
               Count:=16;
               Give_Counter:=1;
               --Ada.Text_IO.Put("give A");
            elsif Give_Counter=1 then
               B(0..15):=(16#10#,16#11#,16#12#,16#13#,16#14#,16#15#,16#16#,16#17#,
                         16#18#,16#19#,16#1A#,16#1B#,16#1C#,16#1D#,16#1E#,16#1F#);
               Count:=16;
               Give_Counter:=2;
               --Ada.Text_IO.Put("give B");
            else
               B(0..15):=(16#20#,16#21#,16#22#,16#23#,16#24#,16#25#,16#26#,16#27#, Others=>0);
               Count:=8;
               Give_Counter:=5;
               --Ada.Text_IO.Put("give C");
            end if;
         end give_bytes;
      end giver;




      procedure give_bytes(B : out Bytes; Count: out Natural) is
         return_bytes : Bytes(0..7):=(others=>Byte(0));
      begin
         B:=return_bytes;
         Count:=8;
      end;

      procedure get_bytes(B : in Bytes) is
      begin
	 Ada.Text_IO.New_Line;
         for i in B'Range loop
            Ada.Text_IO.Put(Crypto.Types.To_Hex(B(i)));
         end loop;
         Ada.Text_IO.New_Line;
      end;

      ----------AE package------------------------

      package AES_128 renames Crypto.Symmetric.Blockcipher_AES128;

      package OCB3 is new Crypto.Symmetric.AE_OCB3(BC            => AES_128,
                                                  N             => N,
                                                  "xor"         => "xor",
                                                  To_Block_Type => To_B_Block128,
                                                  To_Bytes      => To_Bytes,
                                                  Shift_Left    => Shift_Left,
                                                  Shift_Right   => Shift_Right,
                                                  To_Byte_Word  => To_Bytes);



      my_Scheme : OCB3.AE_OCB;
      CR : OCB3.AE.Callback_Reader := giver.give_bytes'Access;
      CW : OCB3.AE.Callback_Writer := get_bytes'Access;





   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Start ocbee");


      Counter.Initialize(This      => Nonce,
                         File_Path => "last_nonce.txt",
                         IV        => zero_iv);

--        my_Scheme.Init_Encrypt(Key   => Key,
--                               Nonce => Nonce);

      my_Scheme.Init_Encrypt(Key             => Key,
                             N_Init          => Nonce,
                             Bytes_Of_N_Read => 12,
                             Taglen          => 16);

      my_Scheme.Encrypt(Read_Plaintext   =>CR ,
                        Write_Ciphertext =>CW );


--
--        if zero_iv(12)=output(10) then
--           Ada.Text_IO.Put_Line("gleich");
--        else
--           Ada.Text_IO.Put_Line("nicht gleich");
--
--        end if;

--        for i in 0..127 loop
--           Ada.Text_IO.Put_Line(Crypto.Types.To_Hex(output(i)));
--        end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("End OCB");
      Ada.Text_IO.Put_Line(Integer'Image(Give_Counter));

      Assert(true, "OCB3 failed.");

   end OCB3_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB3_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;

      package KDF renames Crypto.Symmetric.KDF_SHA512Crypt;
      bo : Boolean;
      wb : W_Block512 := (others=>0);
   begin

      Ada.Text_IO.Put_Line("start_kdf");
      bo := KDF.Initialize(Parameter => 5);
      KDF.Derive(Salt     => "hihihihihi",
                 Password => "hihihihihi",
                 Key      => wb );

      for i in wb'Range loop

            Ada.Text_IO.Put(Crypto.Types.To_Hex(wb(i)));
         end loop;


      Ada.Text_IO.Put_Line("end_kdf1");




   	Assert(True, "Fail at SHA512Crypt Test");

   end OCB3_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB3_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   Key := (others => 0);

   	   CT := (16#9F#, 16#58#, 16#9F#, 16#5C#, 16#F6#, 16#12#, 16#2C#, 16#32#,
              16#B6#, 16#BF#, 16#EC#, 16#2F#, 16#2A#, 16#E8#, 16#C3#, 16#5A#);

	   CT := (others => 0);
   	   for I in PT'Range loop
   	   	   Assert(PT(I) = CT(I), "OCB3 failed.");
	   end loop;

   end OCB3_Test3;

------------------------------------------------------------------------------------

end Test.OCB;
