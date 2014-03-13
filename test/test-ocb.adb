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
with Crypto.Symmetric.AEAD_McOE;

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
                                           16#10#, 16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#);

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

      my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
                        Ciphertext => Sixteen_Bytes_Container,
                        AD         => Zero_Bytes);

      Assert(Sixteen_Bytes_Container = (16#19#, 16#7B#, 16#9C#, 16#3C#, 16#44#, 16#1D#, 16#3C#, 16#83#,
             16#EA#, 16#FB#, 16#2B#, 16#EF#, 16#63#, 16#3B#, 16#91#, 16#82#),"First Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Sixteen_Bytes_Container,
                                          Plaintext  => Eight_Bytes_Container,
                                          AD         => Zero_Bytes), "First Decryption Failed");
      -------

      my_Scheme.Encrypt(Plaintext  => Eight_Bytes_Full,
                        Ciphertext => Twentyfour_Bytes_Container,
                        AD         => Eight_Bytes_Full);

      Assert(Twentyfour_Bytes_Container = (16#92#, 16#B6#, 16#57#, 16#13#, 16#0A#, 16#74#, 16#B8#, 16#5A#,
             16#16#, 16#DC#, 16#76#, 16#A4#, 16#6D#, 16#47#, 16#E1#, 16#EA#,
             16#D5#, 16#37#, 16#20#, 16#9E#, 16#8A#, 16#96#, 16#D1#, 16#4E#),"Second Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Twentyfour_Bytes_Container,
                                          Plaintext  => Eight_Bytes_Container,
                                          AD         => Eight_Bytes_Full), "Second Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
                        Ciphertext => Sixteen_Bytes_Container,
                        AD         => Eight_Bytes_Full);

      Assert(Sixteen_Bytes_Container = (16#98#, 16#B9#, 16#15#, 16#52#, 16#C8#, 16#C0#, 16#09#, 16#18#,
             16#50#, 16#44#, 16#E3#, 16#0A#, 16#6E#, 16#B2#, 16#FE#, 16#21#),"Third Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Sixteen_Bytes_Container,
                                          Plaintext  => Zero_Bytes,
                                          AD         => Eight_Bytes_Full), "Third Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Eight_Bytes_Full,
                        Ciphertext => Twentyfour_Bytes_Container,
                        AD         => Zero_Bytes);

      Assert(Twentyfour_Bytes_Container = (16#92#, 16#B6#, 16#57#, 16#13#, 16#0A#, 16#74#, 16#B8#, 16#5A#,
             16#97#, 16#1E#, 16#FF#, 16#CA#, 16#E1#, 16#9A#, 16#D4#, 16#71#,
             16#6F#, 16#88#, 16#E8#, 16#7B#, 16#87#, 16#1F#, 16#BE#, 16#ED#),"Fourth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Twentyfour_Bytes_Container,
                                          Plaintext  => Eight_Bytes_Container,
                                          AD         => Zero_Bytes), "Fourth Decryption Failed");


      --------

      my_Scheme.Encrypt(Plaintext  => Sixteen_Bytes_Full,
                        Ciphertext => Thirtytwo_Bytes_Container,
                        AD         => Sixteen_Bytes_Full);

      Assert(Thirtytwo_Bytes_Container = (16#BE#, 16#A5#, 16#E8#, 16#79#, 16#8D#, 16#BE#, 16#71#, 16#10#,
             16#03#, 16#1C#, 16#14#, 16#4D#, 16#A0#, 16#B2#, 16#61#, 16#22#,
             16#77#, 16#6C#, 16#99#, 16#24#, 16#D6#, 16#72#, 16#3A#, 16#1F#,
             16#C4#, 16#52#, 16#45#, 16#32#, 16#AC#, 16#3E#, 16#5B#, 16#EB#),"Fifth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Thirtytwo_Bytes_Container,
                                          Plaintext  => Sixteen_Bytes_Container,
                                          AD         => Sixteen_Bytes_Full), "Fifth Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
                        Ciphertext => Sixteen_Bytes_Container,
                        AD         => Sixteen_Bytes_Full);

      Assert(Sixteen_Bytes_Container = (16#7D#, 16#DB#, 16#8E#, 16#6C#, 16#EA#, 16#68#, 16#14#, 16#86#,
             16#62#, 16#12#, 16#50#, 16#96#, 16#19#, 16#B1#, 16#9C#, 16#C6#),"Sixth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Sixteen_Bytes_Container,
                                          Plaintext  => Zero_Bytes,
                                          AD         => Sixteen_Bytes_Full), "Sixth Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Sixteen_Bytes_Full,
                        Ciphertext => Thirtytwo_Bytes_Container,
                        AD         => Zero_Bytes);

      Assert(Thirtytwo_Bytes_Container = (16#BE#, 16#A5#, 16#E8#, 16#79#, 16#8D#, 16#BE#, 16#71#, 16#10#,
             16#03#, 16#1C#, 16#14#, 16#4D#, 16#A0#, 16#B2#, 16#61#, 16#22#,
             16#13#, 16#CC#, 16#8B#, 16#74#, 16#78#, 16#07#, 16#12#, 16#1A#,
             16#4C#, 16#BB#, 16#3E#, 16#4B#, 16#D6#, 16#B4#, 16#56#, 16#AF#),"Seventh Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Thirtytwo_Bytes_Container,
                                          Plaintext  => Sixteen_Bytes_Container,
                                          AD         => Zero_Bytes), "Seventh Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Twentyfour_Bytes_Full,
                        Ciphertext => Fourty_Bytes_Container,
                        AD         => Twentyfour_Bytes_Full);

      Assert(Fourty_Bytes_Container = (16#BE#, 16#A5#, 16#E8#, 16#79#, 16#8D#, 16#BE#, 16#71#, 16#10#,
             16#03#, 16#1C#, 16#14#, 16#4D#, 16#A0#, 16#B2#, 16#61#, 16#22#,
             16#FC#, 16#FC#, 16#EE#, 16#7A#, 16#2A#, 16#8D#, 16#4D#, 16#48#,
             16#5F#, 16#A9#, 16#4F#, 16#C3#, 16#F3#, 16#88#, 16#20#, 16#F1#,
             16#DC#, 16#3F#, 16#3D#, 16#1F#, 16#D4#, 16#E5#, 16#5E#, 16#1C#),"Eigth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Fourty_Bytes_Container,
                                          Plaintext  => Twentyfour_Bytes_Container,
                                          AD         => Twentyfour_Bytes_Full), "Eigth Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Zero_Bytes,
                        Ciphertext => Sixteen_Bytes_Container,
                        AD         => Twentyfour_Bytes_Full);

      Assert(Sixteen_Bytes_Container = (16#28#, 16#20#, 16#26#, 16#DA#, 16#30#, 16#68#, 16#BC#, 16#9F#,
             16#A1#, 16#18#, 16#68#, 16#1D#, 16#55#, 16#9F#, 16#10#, 16#F6#),"Ninth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Sixteen_Bytes_Container,
                                          Plaintext  => Eight_Bytes_Container,
                                          AD         => Twentyfour_Bytes_Full), "Ninth Decryption Failed");

      --------

      my_Scheme.Encrypt(Plaintext  => Twentyfour_Bytes_Full,
                        Ciphertext => Fourty_Bytes_Container,
                        AD         => Zero_Bytes);

      Assert(Fourty_Bytes_Container = (16#BE#, 16#A5#, 16#E8#, 16#79#, 16#8D#, 16#BE#, 16#71#, 16#10#,
             16#03#, 16#1C#, 16#14#, 16#4D#, 16#A0#, 16#B2#, 16#61#, 16#22#,
             16#FC#, 16#FC#, 16#EE#, 16#7A#, 16#2A#, 16#8D#, 16#4D#, 16#48#,
             16#6E#, 16#F2#, 16#F5#, 16#25#, 16#87#, 16#FD#, 16#A0#, 16#ED#,
             16#97#, 16#DC#, 16#7E#, 16#ED#, 16#E2#, 16#41#, 16#DF#, 16#68#),"Tenth Encryption failed!");

      Assert(my_Scheme.Decrypt_And_Verify(Ciphertext => Fourty_Bytes_Container,
                                          Plaintext  => Twentyfour_Bytes_Container,
                                          AD         => Zero_Bytes), "Tenth Decryption Failed");

   end OCB3_Test_Encryption;


end Test.OCB;
