with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Blowfish.Oneway;
with Crypto.Types;
with Crypto.Types.Random;
with Text_IO;
use Crypto.Types;

package body Test.Blowfish_Oneway is

   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   ------------------------ Register Blowfish Oneway Tests -------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Blowfish_Oneway_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Blowfish_Oneway_Test1'Access,"Blowfish128 Oneway Determinism Test");
      Register_Routine(T, Blowfish_Oneway_Test2'Access,"Blowfish128 Oneway Known Answer Test");
   end Register_Tests;

   -----------------------------------------------------------------------------
   --------------------------- Name Blowfish Oneway Test ---------------------------
   -----------------------------------------------------------------------------

   function Name(T : Blowfish_Oneway_Test) return Test_String is
   begin
      return new String'("Blowfish Oneway Test");
   end Name;

   -----------------------------------------------------------------------------
   --------------------------------- Start Tests -------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 1 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Blowfish_Oneway_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.Blowfish.Oneway;
      One_Way_Key : Crypto.Types.B_Block128;
      One_Way_Plaintext : Crypto.Types.B_Block64;
      One_Way_Ciphertext : Crypto.Types.B_Block64;
      One_Way_Ciphertext2 : Crypto.Types.B_Block64;
      One_Way_Cipherkey : Crypto.Symmetric.Algorithm.Blowfish.Oneway.Cipherkey_Oneway_Blowfish128;
      Is_distinct : Boolean := true;

   begin
      for k in 0..100 loop

      	for i in One_Way_Key'Range loop
          Crypto.Types.Random.Read(One_Way_Key(i));
      	end loop;

      	for i in One_Way_Plaintext'Range loop
          Crypto.Types.Random.Read(One_Way_Plaintext(i));
      	end loop;

      	Prepare_Oneway_Key128(One_Way_Key, One_Way_Cipherkey);
      	Encrypt_OneWay128(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);
      	Encrypt_OneWay128(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext2);
         if One_Way_Ciphertext /= One_Way_Ciphertext2 then
            Is_distinct := false;
         end if;

      end loop;

      Assert(Is_distinct = true, "Blowfish128 Oneway Determinism Test failed");
   end Blowfish_Oneway_Test1;

   ------------------------------------------------------------------------------------
   ------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------------- Test 2 ----------------------------------
   -----------------------------------------------------------------------------

   procedure Blowfish_Oneway_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      use Crypto.Symmetric.Algorithm.Blowfish.Oneway;
      use Crypto.Types;



      One_Way_Plaintext: B_Block64 := (16#00#, 16#00#, 16#00#, 16#00#,
                                        16#00#, 16#00#, 16#00#, 16#000#);

      One_Way_Test_Ciphertext: B_Block64 := (16#4e#, 16#f9#, 16#97#, 16#45#,
                                             16#61#, 16#98#, 16#dd#, 16#78#);

      One_Way_Key: B_Block128 := (16#00#, 16#00#, 16#00#, 16#00#,
                                  16#00#, 16#00#, 16#00#, 16#00#,
                                  16#00#, 16#00#, 16#00#, 16#00#,
                                  16#00#, 16#00#, 16#00#, 16#00#);

      One_Way_Cipherkey: Cipherkey_Oneway_Blowfish128;
      One_Way_Ciphertext: B_Block64;

   begin

      Prepare_Oneway_Key128(Key       => One_Way_Key,
                            Cipherkey => One_Way_Cipherkey);
      Encrypt_OneWay128(One_Way_Cipherkey, One_Way_Plaintext, One_Way_Ciphertext);


      Assert(One_Way_Ciphertext = One_Way_Test_Ciphertext, "Blowfish128 Oneway Known Answer Test failed");
   end Blowfish_Oneway_Test2;

------------------------------------------------------------------------------------

end Test.Blowfish_Oneway;
