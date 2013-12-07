with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Symmetric.AE_OCB3;
with Crypto.Symmetric.Blockcipher_AES128;
with Crypto.Types;
with Crypto.Types.Nonces;
with Crypto.Types.Nonces.Nonces_Ctr;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Ada.Containers.Vectors;
with Crypto.Symmetric.AE;
with Crypto.Symmetric.KDF_SHA512Crypt;

package body Test.OCB is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


   Zero_Bytes : Bytes(1..0) := (others => 0);
   Eight_Bytes_Container : Bytes(0..7):= (others=>0);
   Sixteen_Bytes_Container : Bytes(0..15):= (others => 0);
   Twentyfour_Bytes_Container : Bytes(0..23):= (others => 0);
   Thirtytwo_Bytes_Container : Bytes(0..31):= (others => 0);
   Fourty_Bytes_Container : Bytes(0..39):= (others => 0);

   Eight_Bytes_Full : Bytes(0..7):= (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#);
   Sixteen_Bytes_Full : Bytes(0..15):= (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#,
                                   16#08#, 16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#);
   Twentyfour_Bytes_Full : Bytes(0..23):= (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#,
                                   16#08#, 16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#,
                                   16#10#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#);

   Key: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#,
                       16#08#, 16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#);


   PT: B_Block128 := (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, others=>Byte(0));

   CT: Bytes(0..23) := (others=>0);

   Plain_Bytes : Bytes(0..7) := (others=>0);


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




------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register OCB3 Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out OCB3_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, OCB3_Test_Initialize'Access,"OCB3 Initialization");
		Register_Routine(T, OCB3_Test_Encryption'Access,"OCB3 Encryption / Decryption");
		Register_Routine(T, OCB3_Test_Exceptions'Access,"OCB3 Exceptions");
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


   procedure OCB3_Test_Initialize(T : in out Test_Cases.Test_Case'Class) is
   begin
      Counter.Initialize(This      => Nonce,
                         File_Path => "last_nonce.txt",
                         IV        => zero_iv);

      my_Scheme.Init_Encrypt(Key             => Key,
                             N_Init          => Nonce,
                             Bytes_Of_N_Read => 12,
                             Taglen          => 16);




      Assert(true, "OCB3 failed.");

   end OCB3_Test_Initialize;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB3_Test_Encryption(T : in out Test_Cases.Test_Case'Class) is

   begin

--        my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
--                          Ciphertext => Sixteen_Bytes_Container,
--                          AD         => Zero_Bytes);
--
--        Assert(Sixteen_Bytes_Container = (16#19#, 16#7B#, 16#9C#, 16#3C#, 16#44#, 16#1D#, 16#3C#, 16#83#,
--               16#EA#, 16#FB#, 16#2B#, 16#EF#, 16#63#, 16#3B#, 16#91#, 16#82#),"First Encryption failed!");
--
--        -------
--
--
--        my_Scheme.Encrypt(Plaintext  => Eight_Bytes_Full,
--                          Ciphertext => Twentyfour_Bytes_Container,
--                          AD         => Eight_Bytes_Full);
--
--        Assert(Twentyfour_Bytes_Container = (16#92#, 16#B6#, 16#57#, 16#13#, 16#0A#, 16#74#, 16#B8#, 16#5A#, 16#16#, 16#DC#, 16#76#, 16#A4#, 16#6D#, 16#47#, 16#E1#, 16#EA#, 16#D5#, 16#37#, 16#20#, 16#9E#, 16#8A#, 16#96#, 16#D1#, 16#4E#),"First Encryption failed!");
--
--        --------
--
--        my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
--                          Ciphertext => Sixteen_Bytes_Container,
--                          AD         => Eight_Bytes_Full);
--
--        Assert(Sixteen_Bytes_Container = (16#98#, 16#B9#, 16#15#, 16#52#, 16#C8#, 16#C0#, 16#09#, 16#18#, 16#50#, 16#44#, 16#E3#, 16#0A#, 16#6E#, 16#B2#, 16#FE#, 16#21#),"First Encryption failed!");
--
--
--        --------
--
--        my_Scheme.Encrypt(Plaintext  => Eight_Bytes_Full,
--                          Ciphertext => Twentyfour_Bytes_Container,
--                          AD         => Zero_Bytes);
--
--        Assert(Twentyfour_Bytes_Container = (16#92#, 16#B6#, 16#57#, 16#13#, 16#0A#, 16#74#, 16#B8#, 16#5A#,
--               				   16#97#, 16#1E#, 16#FF#, 16#CA#, 16#E1#, 16#9A#, 16#D4#, 16#71#,
--               				   16#6F#, 16#88#, 16#E8#, 16#7B#, 16#87#, 16#1F#, 16#BE#, 16#ED#),"First Encryption failed!");
--
--        --------

      my_Scheme.Encrypt(Plaintext  => Sixteen_Bytes_Full,
                        Ciphertext => Fourty_Bytes_Container,
                        AD         => Zero_Bytes);


      for I in Fourty_Bytes_Container'Range loop
         Ada.Text_IO.Put(To_Hex(Fourty_Bytes_Container(I)));
      end loop;


      Assert(Thirtytwo_Bytes_Container = (16#BE#, 16#A5#, 16#E8#, 16#79#, 16#8D#, 16#BE#, 16#71#, 16#10#,
             				  16#03#, 16#1C#, 16#14#, 16#4D#, 16#A0#, 16#B2#, 16#61#, 16#22#,
             				  16#77#, 16#6C#, 16#99#, 16#24#, 16#D6#, 16#72#, 16#3A#, 16#1F#,
             				  16#C4#, 16#52#, 16#45#, 16#32#, 16#AC#, 16#3E#, 16#5B#, 16#EB#),"First Encryption failed!");





      Assert(True, "Fail at SHA512Crypt Test");

   end OCB3_Test_Encryption;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure OCB3_Test_Exceptions(T : in out Test_Cases.Test_Case'Class) is
   begin

   	   Assert(True, "Aussage");

   end OCB3_Test_Exceptions;

------------------------------------------------------------------------------------

end Test.OCB;
