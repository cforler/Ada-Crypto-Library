with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Skein;
with Crypto.Types.Skein;
with Ada.text_IO;

package body Test.Skein is
   use Crypto.Types.Skein;


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
      use Crypto.Symmetric.Algorithm.Skein;

      Mode    : Skein_Mode := m512;

        M_Length: Natural := 64*8;      --message length in BITS
        Message_Bytes_Length : Natural := 64*1;
        Message : Skein_Bytes(0..Message_Bytes_Length-1) := (others => Skein_Byte(0));


        N_0     : Natural := 512;      --result length in BITS

        --needed for full skein
        Key  : Skein_Bytes(0..-1) := (others => Skein_Byte(0));

--        Key   : Bytes(0..31) := (
--            0 => Byte(16#CB#),
--            1 => Byte(16#41#),
--            2 => Byte(16#F1#),
--            3 => Byte(16#70#),
--            4 => Byte(16#6C#),
--            5 => Byte(16#DE#),
--            6 => Byte(16#09#),
--            7 => Byte(16#65#),
--            8 => Byte(16#12#),
--            9 => Byte(16#03#),
--            10 => Byte(16#C2#),
--            11 => Byte(16#D0#),
--            12 => Byte(16#EF#),
--            13 => Byte(16#BA#),
--            14 => Byte(16#DD#),
--            15 => Byte(16#F8#),
--            16 => Byte(16#47#),
--            17 => Byte(16#A0#),
--            18 => Byte(16#D3#),
--            19 => Byte(16#15#),
--            20 => Byte(16#CB#),
--            21 => Byte(16#2E#),
--            22 => Byte(16#53#),
--            23 => Byte(16#FF#),
--            24  => Byte(16#8B#),
--            25 => Byte(16#AC#),
--            26 => Byte(16#41#),
--            27 => Byte(16#DA#),
--            28 => Byte(16#00#),
--            29 => Byte(16#02#),
--            30 => Byte(16#67#),
--            31 => Byte(16#2E#)
--            );

        Tweak : Skein_Bytes(0..15) :=(others => Skein_Byte(0));

        Type_Value_Msg  : Skein_Byte := Skein_Byte(48);

        Initresult: Skein_Bytes(0..Get_Number_Of_Skein_Bytes(Mode)-1) := (others => Skein_Byte(0));
        Updateresult: Skein_Bytes(0..Get_Number_Of_Skein_Bytes(Mode)-1) := (others => Skein_Byte(0));
        Finalresult: Skein_Bytes(0..N_0/8-1);

        simple : Boolean := true;
        modify_message :Boolean := true;

   begin

        if modify_Message then
--            Message(0) := Byte(16#D3#);

            for i in Message'Range loop
                Message(i) := Skein_Byte(255-i);
            end loop;

--            for i in Message'Range loop
--                if i mod 2 = 0 then
--                    Message(i) := Byte(i mod 256);
--                else
--                    if i < 256 then
--                        Message(i) := Byte(2);
--                    else
--                        Message(i) := Byte(3);
--                    end if;
--                end if;
--            end loop;
        end if;

        Ada.text_IO.Put_Line(" ");
        Ada.text_IO.Put_Line("The Original Message");
        for i in Message'Range loop
            Ada.Text_IO.Put(Show_Hex(Message(i)));
            Ada.Text_IO.Put(" ");
            if (i+1) mod 16 = 0 then
                Ada.Text_IO.Put_LIne(" ");
            end if;
        end loop;
        Ada.Text_IO.Put_LIne(" ");

        --a simple test
        if simple then
            Hash(
                Mode => Mode,
                N_0 =>N_0,
                Message => Message,
                Message_length => M_Length,
                Result => Finalresult);

        else

            --the "big" test
            Ada.Text_IO.Put_LIne(" ");
            Ada.Text_IO.Put_Line("The Input_Key");
            for i in Key'Range loop
                Ada.Text_IO.Put(Show_Hex(Key(i)));
                Ada.Text_IO.Put(" ");
                if (i+1) mod 16 = 0 then
                    Ada.Text_IO.Put_LIne(" ");
                end if;
            end loop;
            Ada.Text_IO.Put_LIne(" ");

            Hash(
                Mode => Mode,
                N_0 =>N_0,
                K => Key,
                Y_l => 2,
                Y_f => 2,
                Y_m => 2,
                Message => Message,
                Message_Length => M_Length,
                Type_Value => Type_Value_Msg,
                Result => Finalresult);

        end if;

        Ada.text_IO.Put_Line(" ");
        Ada.text_IO.Put_Line("This is the result of the hashing test");
        for i in Finalresult'Range loop
            Ada.Text_IO.Put(Show_Hex(Finalresult(i)));
            Ada.Text_IO.Put(" ");
            if (i+1) mod 16 = 0 then
                Ada.Text_IO.Put_LIne(" ");
            end if;
      end loop;

      Assert(True, "Skein Test ...");


   end Skein_Test1;

   end Test.Skein;
