with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Serpent.Oneway;
with Crypto.Types;
with Crypto.Types.Random;
use Crypto.Types;
with Text_IO;

package body Test.Serpent_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register Serpent Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Serpent_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Serpent_Oneway_Test1'Access,"Serpent Oneway Determinism Test");
      Register_Routine(T, Serpent_Oneway_Test2'Access,"Serpent Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Serpent Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Serpent_Oneway_Test) return Test_String is
   begin
      return new String'("Serpent Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Serpent_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.Serpent.Oneway;
      One_Way_Key : Crypto.Types.B_Block256;
      One_Way_Plaintext : Crypto.Types.B_Block128;
      One_Way_Ciphertext : Crypto.Types.B_Block128;
      One_Way_Ciphertext2 : Crypto.Types.B_Block128;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.Serpent.Oneway.Cipherkey_Oneway_Serpent256;
      Is_distinct : Boolean := true;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
          Crypto.Types.Random.Read(One_Way_Key(i));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
          Crypto.Types.Random.Read(One_Way_Plaintext(i));
      	end loop;

      	Prepare_Oneway_Key256(One_Way_Key, One_Way_Cipherkey);
      	Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct = true, "Serpent Oneway Determinism Test failed");
   end Serpent_Oneway_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Serpent_Oneway_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.Serpent.Oneway;
      use Crypto.Types;



      One_Way_Plaintext: B_Block128 := (16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   				 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   				 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   				 16#00#);

      One_Way_Test_Ciphertext: B_Block128 := (16#a2#, 16#23#, 16#aa#, 16#12#, 16#88#,
                                              16#46#, 16#3c#, 16#0e#, 16#2b#, 16#e3#,
                                              16#8e#, 16#bd#, 16#82#, 16#56#, 16#16#,
                                              16#c0#);

      One_Way_Key: B_Block256 := (16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   		   16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   		   16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   		   16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   		   16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
   	   		   16#00#, 16#00#);

      One_Way_Cipherkey: Cipherkey_Oneway_Serpent256;
      One_Way_Ciphertext: B_Block128;

   begin

      Prepare_Oneway_Key256(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt_OneWay256(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);

      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "Serpent Oneway Known Answer Test failed");
   end Serpent_Oneway_Test2;

------------------------------------------------------------------------------------

end Test.Serpent_Oneway;
