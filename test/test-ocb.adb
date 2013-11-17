with AUnit.Assertions;
with Crypto.Symmetric.AE_OCB3;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Ada.Text_IO;

package body Test.OCB is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   package AES_128 renames Crypto.Symmetric.Blockcipher_AES128;
--     package N is new Crypto.Types.Nonces(Block=>Crypto.Types.B_Block128);
--     package OCB is new Crypto.Symmetric.AE_OCB3(BC            => AES_128,
--                                                N             => N,
--                                                "xor"         => "xor",
--                                                To_Block_Type => To_B_Block128,
--                                                To_Bytes      => To_Bytes,
--                                                Shift_Left    => Shift_Left,
--                                                Shift_Right   => Shift_Right,
--                                                To_Byte_Word  => To_Bytes);

   Static_Nonce: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#, 16#09#, 16#0A#, 16#0B#, others=>Byte(0));
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
		--Register_Routine(T, OCB3_Test2'Access,"OCB3_Test2.");
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

      function Inc(Item: B_Block128) return B_Block128 is
         use Crypto.Types;
         Result: B_Block128 := Item;
      begin
         Result := Result;
         return Result;
      end Inc;



      package N is new Crypto.Types.Nonces(Block => B_Block128);
      package Counter is new N.Nonces_Ctr(Inc => Inc);


      Nonce: Counter.Nonce_Ctr;
      zero_iv: B_Block128:=(16#00#,16#01#,16#02#,16#03#,
                            16#04#,16#05#,16#06#,16#07#,
                            16#08#,16#09#,16#0A#,16#0B#,
                            16#08#,16#09#,16#0A#,16#0B#);

      output: B_Block128;


   begin

      Ada.Text_IO.Put_Line("Counter Test");

      Counter.Initialize(This      => Nonce,
                         File_Path => "nonce_for_ocb.txt",
                         IV        => zero_iv );

      output:=Counter.Update(Nonce);
      Ada.Text_IO.Put_Line(To_String(To_Bytes(output)));

      Ada.Text_IO.Put_Line("Counter Test Ende");

      Assert(true, "OCB3 failed.");

   end OCB3_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB3_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;    begin

   	   Key := (16#BC#, 16#A7#, 16#24#, 16#A5#, 16#45#, 16#33#, 16#C6#, 16#98#,
               16#7E#, 16#14#, 16#AA#, 16#82#, 16#79#, 16#52#, 16#F9#, 16#21#);

       CT := (16#6B#, 16#45#, 16#92#, 16#86#, 16#F3#, 16#FF#, 16#D2#, 16#8D#,
              16#49#, 16#F1#, 16#5B#, 16#15#, 16#81#, 16#B0#, 16#8E#, 16#42#);


   	   for I in PT'Range loop
   	   	   Assert(PT(I) = CT(I), "OCB3 failed.");
	   end loop;

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
