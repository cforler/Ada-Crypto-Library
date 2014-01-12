with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Directories;
with Crypto.Symmetric.KDF_SHA512Crypt;
use Crypto.Symmetric.KDF_SHA512Crypt;
with Crypto.Symmetric.Algorithm;
with Crypto.Symmetric.Algorithm.SHA512;
with Crypto.Symmetric.KDF;

with Crypto.Symmetric.KDF_Scrypt;


package body Test.SHA512Crypt is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


   package S5C renames Crypto.Symmetric.KDF_SHA512Crypt;
   package SHA512 renames Crypto.Symmetric.Algorithm.SHA512;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register SHA512Crypt Test 1 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out SHA512Crypt_Test) is
		use Test_Cases.Registration;
	begin
		Register_Routine(T, SHA512Crypt_Test_Add_Bytes'Access,"SHA512Crypt Initialization");
		Register_Routine(T, SHA512Crypt_Test_Encryption'Access,"SHA512Crypt Encryption / Decryption");
		Register_Routine(T, SHA512Crypt_Test_Exceptions'Access,"SHA512Crypt Exceptions");
	end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name SHA512Crypt Test ------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : SHA512Crypt_Test) return Test_String is
	begin
		return new String'("SHA512Crypt Test");
	end Name;



------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------


   procedure SHA512Crypt_Test_Add_Bytes(T : in out Test_Cases.Test_Case'Class) is

      Hash : DW_Block512;
      Hash_Ideal : DW_Block512;
      Digest_Bytes : Bytes(0..127) := (others => 0);
      Digest_Bytes_Length : Natural := 0;
      Bytes_To_Add_A : Bytes(0..7) := (Others=>1);
      Bytes_To_Add_B : Bytes(0..63) := (Others=>2);
      Bytes_To_Add_C : Bytes(0..127) := (Others=>3);
      Digest_Bytes_Ideal : Bytes(0..127) := (others => 0);
   begin


      null;



--        SHA512.Init(Hash_Value => Hash);
--
--        ----------------
--
--        Hash_Ideal := Hash;
--        S5C.Add_Bytes(Bytes_To_Add        => Bytes_To_Add_A,
--                      Digest_Bytes        => Digest_Bytes,
--                      Digest_Bytes_Length => Digest_Bytes_Length,
--                      Digest_Hash         => Hash);
--
--        Assert(Hash = Hash_Ideal, "SHA512Crypt failed.");
--        Digest_Bytes_Ideal(0..7) := (others=>1);
--        Assert(Digest_Bytes=Digest_Bytes_Ideal, "First Ideal failed");
--        Assert(Digest_Bytes_Length = 8, "SHA512Crypt failed.");
--
--        -----------------
--
--        Hash_Ideal := Hash;
--        S5C.Add_Bytes(Bytes_To_Add        => Bytes_To_Add_B,
--                      Digest_Bytes        => Digest_Bytes,
--                      Digest_Bytes_Length => Digest_Bytes_Length,
--                      Digest_Hash         => Hash);
--
--        Assert(Hash = Hash_Ideal, "SHA512Crypt failed.");
--        Digest_Bytes_Ideal(8..71) := (others=>2);
--        Assert(Digest_Bytes=Digest_Bytes_Ideal, "First Ideal failed");
--        Assert(Digest_Bytes_Length = 72, "SHA512Crypt failed.");
--
--        ---------------
--
--        Hash_Ideal := Hash;
--        Digest_Bytes_Ideal(72..127) := (others=>2);
--        SHA512.Round(Message_Block => To_DW_Block1024(B => Digest_Bytes_Ideal),
--                     Hash_Value    => Hash_Ideal);
--        S5C.Add_Bytes(Bytes_To_Add        => Bytes_To_Add_B,
--                      Digest_Bytes        => Digest_Bytes,
--                      Digest_Bytes_Length => Digest_Bytes_Length,
--                      Digest_Hash         => Hash);
--
--        Assert(Hash = Hash_Ideal, "SHA512Crypt failed.");
--        Digest_Bytes_Ideal(0..127) := (others=>0);
--        Digest_Bytes_Ideal(0..7) := (others=>2);
--        Assert(Digest_Bytes=Digest_Bytes_Ideal, "First Ideal failed");
--        Assert(Digest_Bytes_Length = 8, "SHA512Crypt failed.");


   end SHA512Crypt_Test_Add_Bytes;

------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA512Crypt_Test_Encryption(T : in out Test_Cases.Test_Case'Class) is

      By: Bytes(0..3):=(16#55#, others=>0);

      WB512 : W_Block512;

      Scheme : S5C.SHA512Crypt_KDF;

   begin



      Scheme.Derive(Salt     => "saltstring",
                    Password => "Hello World!",
                    Key      => WB512);



      Assert(True, "Fail at SHA512Crypt Test");

   end SHA512Crypt_Test_Encryption;

------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure SHA512Crypt_Test_Exceptions(T : in out Test_Cases.Test_Case'Class) is
   begin

   	   Assert(True, "Aussage");

   end SHA512Crypt_Test_Exceptions;

------------------------------------------------------------------------------------

end Test.SHA512Crypt;
