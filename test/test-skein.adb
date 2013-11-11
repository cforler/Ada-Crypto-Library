with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Skein;
with Crypto.Symmetric.Algorithm.Threefish;
with Crypto.Types.Skein;
with Crypto.Types;
with Ada.text_IO;

package body Test.Skein is
   use Crypto.Types.Skein;
   use Crypto.Symmetric.Algorithm.Skein;
   use Crypto.Types;
   use Crypto.Symmetric.Algorithm.Threefish;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------Type - Declaration -----------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   Mode : Skein_Mode;
   Message_Bytes_Length : Natural:=256;
   Result_Bits_Length : Natural:= 1024;


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
----------------------------- Register Skein Test-----------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   procedure Register_Tests(T : in out Skein_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Skein_Test1'Access,"Skein_Test1.");

   end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Skein Test -------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

   function Name(T : Skein_Test) return Test_String is
	begin
		return new String'("Skein Test");
   end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
--------------------First simple test to check functionality------------------------
------------------------------------------------------------------------------------


   procedure Skein_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;


      Message : Bytes(0..Message_Bytes_Length-1) := (others=>Byte(0));
      Finalresult: Bytes(0..Result_Bits_Length/8-1);

   begin

      Mode := m1024;		--result length in BITS

      for i in Message'Range loop
                Message(i) := Byte(255-i);
            end loop;


      Crypto.Symmetric.Algorithm.Skein.Hash(Mode           => Mode,
                                            N_0            => Result_Bits_Length,
                                            Message        => Message,
                                            Message_Length => Message_Bytes_Length*8,
                                            Result         => Finalresult);

      Ada.text_IO.Put_Line(" ");
        Ada.text_IO.Put_Line("This is the result of the hashing test");
        for i in Finalresult'Range loop
         Ada.Text_IO.Put(To_Hex(Finalresult(i)));
            Ada.Text_IO.Put(" ");
            if (i+1) mod 16 = 0 then
                Ada.Text_IO.Put_LIne(" ");
            end if;
        end loop;


      Assert(false, "Skein Test a)");


   end Skein_Test1;













   end Test.Skein;
