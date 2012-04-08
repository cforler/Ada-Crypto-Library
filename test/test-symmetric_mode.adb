with AUnit.Assertions;
with Crypto.Symmetric.Mode;
with Crypto.Types;
with Crypto.Random;
with Ada.Text_IO;

package body Test.Symmetric_Mode is
use Crypto.Types;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------- Type - Declaration --------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
        Mode_Bytes : Bytes(1..32);
        OutLength : Positive;
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------------- Register Symmetric Mode Test --------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	procedure Register_Tests(T : in out Mode_Test) is
	   use Test_Cases.Registration;
        begin
           Register_Routine(T, Mode_Test1'Access,"Symmetric Mode To_Block.");
           Register_Routine(T, Mode_Test2'Access,"Symmetric Mode To_Block.");
           Register_Routine(T, Mode_Test3'Access,"Symmetric Mode To_Bytes.");
           Register_Routine(T, Mode_Test5'Access,"Symmetric Mode Set Zero.");
        end Register_Tests;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ Name Symmetric Mode Test ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

	function Name(T : Mode_Test) return Test_String is
	begin
		return new String'("Symmetric Mode Test");
	end Name;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------ Start Tests -----------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 1 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mode_Test1(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;

   begin
      OutLength := 32;
      declare
         Mode_Block : Bytes(0..OutLength-1);
      begin
      for i in Mode_Bytes'Range loop
         Crypto.Random.Read(Mode_Bytes(i));
      end loop;
      Mode_Block := Crypto.Symmetric.Mode.To_Block(Mode_Bytes,OutLength);

      Assert(Mode_Bytes = Mode_Block,"Symmetric Mode Test To_Block failed!");
      end;
   end Mode_Test1;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 2 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mode_Test2(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      -- Bytes'Length = OutLen /= Block'Length
   begin
      OutLength := 36;
      declare
         Mode_Block : Bytes(0..OutLength-1);
      begin
         for i in Mode_Bytes'Range loop
            Crypto.Random.Read(Mode_Bytes(i));
         end loop;
         Mode_Block := Crypto.Symmetric.Mode.To_Block(Mode_Bytes,OutLength);
         --for i in Mode_Bytes'Range loop
            --Ada.Text_IO.Put(Crypto.Types.Byte'Image(Mode_Bytes(i)));
            --Ada.Text_IO.Put(Crypto.Types.Byte'Image(Mode_Block(i)));
            --Ada.Text_IO.Put_Line("");
         --end loop;

         Assert(Mode_Block(Mode_Bytes'Range) = Mode_Bytes,"Symmetric Mode Test To_Block failed!");
      end;
   end Mode_Test2;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 3 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mode_Test3(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Mode_Bytes_2 : Bytes(1..32);

   begin
      OutLength := 32;
      declare
         Mode_Block : Bytes(1..OutLength);
      begin
         for i in Mode_Bytes'Range loop
         Crypto.Random.Read(Mode_Bytes(i));
         end loop;

         Mode_Block := Crypto.Symmetric.Mode.To_Block(Mode_Bytes,OutLength);
         Mode_Bytes_2 := Crypto.Symmetric.Mode.To_Bytes(Mode_Block);

         Assert(Mode_Bytes = Mode_Bytes_2,"Symmetric Mode Test To_Bytes failed!");
      end;
   end Mode_Test3;

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-------------------------------------- Test 5 --------------------------------------
------------------------------------------------------------------------------------

   procedure Mode_Test5(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;
      Is_Zero : Boolean := True;
   begin
      for i in Mode_Bytes'Range loop
         Crypto.Random.Read(Mode_Bytes(i));
      end loop;

      Crypto.Symmetric.Mode.Set_Zero(Mode_Bytes);

      for i in Mode_Bytes'Range loop
          if Mode_Bytes(i) /= 0 then
             Is_Zero := False;
          end if;
      end loop;

      Assert(Is_Zero = True,"Symmetric Mode Test setting Zero failed!");

   end Mode_Test5;

------------------------------------------------------------------------------------
end Test.Symmetric_Mode;
