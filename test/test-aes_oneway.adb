with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.AES.Oneway;
with Crypto.Types;
with Crypto.Random;
with Text_IO;
use Crypto.Types;

package body Test.Aes_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register AES Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Aes_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Aes_Oneway_Test1921'Access,"AES192 Oneway Determinism Test");
      Register_Routine(T, Aes_Oneway_Test1922'Access,"AES192 Oneway Known Answer Test");
      Register_Routine(T, Aes_Oneway_Test2561'Access,"AES256 Oneway Determinism Test");
      Register_Routine(T, Aes_Oneway_Test2562'Access,"AES256 Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name AES Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Aes_Oneway_Test) return Test_String is
   begin
      return new String'("Aes Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Aes_Oneway_Test1921(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.AES.Oneway;
      One_Way_Key : Crypto.Types.B_Block192;
      One_Way_Plaintext : Crypto.Types.B_Block128;
      One_Way_Ciphertext : Crypto.Types.B_Block128;
      One_Way_Ciphertext2 : Crypto.Types.B_Block128;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.AES.Oneway.Cipherkey_Oneway_AES192;
      Is_distinct : Boolean := true;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
          Crypto.Random.Read(One_Way_Key(i));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
          Crypto.Random.Read(One_Way_Plaintext(i));
      	end loop;

      	Prepare_Oneway_Key192(One_Way_Key, One_Way_Cipherkey);
      	Encrypt_OneWay192(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt_OneWay192(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct = true, "AES192 Oneway Known Answer Test failed");
   end Aes_Oneway_Test1921;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Aes_Oneway_Test1922(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.AES.Oneway;
      use Crypto.Types;



      One_Way_Plaintext: B_Block128 := (16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#);

      One_Way_Test_Ciphertext: B_Block128 := (16#de#, 16#88#, 16#5d#, 16#c8#,
                                              16#7f#, 16#5a#, 16#92#, 16#59#,
                                              16#40#, 16#82#, 16#d0#, 16#2c#,
                                              16#c1#, 16#e1#, 16#b4#, 16#2c#);

      One_Way_Key: B_Block192 := (16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#);

      One_Way_Cipherkey: Cipherkey_Oneway_AES192;
      One_Way_Ciphertext: B_Block128;

   begin

      Prepare_Oneway_Key192(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt_OneWay192(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);


      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "Aes192 Oneway Known Answer Test failed");
   end Aes_Oneway_Test1922;


 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 ----------------------------------- Test 3 ----------------------------------
------------------------------------------------------------------------------

 procedure Aes_Oneway_Test2561(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.AES.Oneway;
      One_Way_Key : Crypto.Types.B_Block256;
      One_Way_Plaintext : Crypto.Types.B_Block128;
      One_Way_Ciphertext : Crypto.Types.B_Block128;
      One_Way_Ciphertext2 : Crypto.Types.B_Block128;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.AES.Oneway.Cipherkey_Oneway_AES256;
      Is_distinct : Boolean := true;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
          Crypto.Random.Read(One_Way_Key(i));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
          Crypto.Random.Read(One_Way_Plaintext(i));
      	end loop;

      	Prepare_Oneway_Key256(One_Way_Key, One_Way_Cipherkey);
      	Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct = true, "AES256 Oneway Determinism Test failed");
   end Aes_Oneway_Test2561;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 4 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Aes_Oneway_Test2562(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.AES.Oneway;
      use Crypto.Types;



      One_Way_Plaintext: B_Block128 := (16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                              16#00#);

      One_Way_Test_Ciphertext: B_Block128 := (16#e3#, 16#5a#, 16#6d#, 16#cb#, 16#19#, 16#b2#,
                      16#01#, 16#a0#, 16#1e#, 16#bc#, 16#fa#, 16#8a#,
                                  16#a2#, 16#2b#, 16#57#, 16#59#);

      One_Way_Key: B_Block256 := (16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                           16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                          16#00#, 16#00#);

      One_Way_Cipherkey: Cipherkey_Oneway_AES256;
      One_Way_Ciphertext: B_Block128;

   begin

      Prepare_Oneway_Key256(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);

      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "AES256 Oneway Known Answer Test failed");
   end Aes_Oneway_Test2562;

------------------------------------------------------------------------------------


end Test.Aes_Oneway;
