with AUnit.Assertions;
with Crypto.Symmetric.AE_OCB;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;
with Crypto.Types.Nonces;

package body Test.OCB is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   package AES_128 renames Crypto.Symmetric.Blockcipher_AES128;
   package N is new Crypto.Types.Nonces(Block=>Crypto.Types.B_Block128);
  -- package OCB is new Crypto.Symmetric.AE_OCB(BC            => AES_128,
  --                                            N             => N,
  --                                            "xor"         => "xor",
  --                                            To_Block_Type => Crypto.Types.To_B_Block128,
  --                                            To_Bytes      => Crypto.Types.To_Bytes,
  --                                            Shift_Left    => Crypto.Types.Shift_Left,
  --                                            Shift_Right   => Crypto.Types.shift_,
  --                                            To_Byte_Word  => Crypto.Types.To_Bytes);

   Plaintext: B_Block128 := (others => 0);
   Key: B_Block128 := (others => 0);
   Ciphertext: B_Block128 := (16#5D#, 16#9D#, 16#4E#, 16#EF#, 16#FA#,
                              16#91#, 16#51#, 16#57#, 16#55#, 16#24#,
                              16#F1#, 16#15#, 16#81#, 16#5A#, 16#12#,
                              16#E0#);
   PT: B_Block128 := Plaintext;
   CT: B_Block128;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register Twofish128 Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out Twofish_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, Twofish128_Test1'Access,"Twofish128_Test1.");
		Register_Routine(T, Twofish128_Test2'Access,"Twofish128_Test2.");
		Register_Routine(T, Twofish128_Test3'Access,"Twofish128_Test3.");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Twofish128 Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Twofish_Test) return Test_String is
	begin
		return new String'("Twofish128 Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Twofish128_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   for I in 1 .. 49 loop
   	   	   Key := PT;
   	   	   PT  := CT;
   	   end loop;

   	   for I in CT'Range loop
   	   	   Assert(CT(I) = Ciphertext(I), "Twofish128 failed.");
	   end loop;
   end Twofish128_Test1;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Twofish128_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;    begin

   	   Key := (16#BC#, 16#A7#, 16#24#, 16#A5#, 16#45#, 16#33#, 16#C6#, 16#98#,
               16#7E#, 16#14#, 16#AA#, 16#82#, 16#79#, 16#52#, 16#F9#, 16#21#);

       CT := (16#6B#, 16#45#, 16#92#, 16#86#, 16#F3#, 16#FF#, 16#D2#, 16#8D#,
              16#49#, 16#F1#, 16#5B#, 16#15#, 16#81#, 16#B0#, 16#8E#, 16#42#);


   	   for I in PT'Range loop
   	   	   Assert(PT(I) = CT(I), "Twofish128 failed.");
	   end loop;

   end Twofish128_Test2;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Twofish128_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
   begin

   	   Key := (others => 0);

   	   CT := (16#9F#, 16#58#, 16#9F#, 16#5C#, 16#F6#, 16#12#, 16#2C#, 16#32#,
              16#B6#, 16#BF#, 16#EC#, 16#2F#, 16#2A#, 16#E8#, 16#C3#, 16#5A#);

	   CT := (others => 0);
   	   for I in PT'Range loop
   	   	   Assert(PT(I) = CT(I), "Twofish128 failed.");
	   end loop;

   end Twofish128_Test3;

------------------------------------------------------------------------------------

end Test.OCB;
