with AUnit.Assertions;
with Crypto.Symmetric.Algorithm.Skein;
with Crypto.Symmetric.Algorithm.Threefish;
with Crypto.Types;


package body Test.Skein is
   use Crypto.Symmetric.Algorithm.Skein;
   use Crypto.Types;
   use Crypto.Symmetric.Algorithm.Threefish;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------------Type - Declaration ----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   Mode : Skein_Mode;


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ----------------------------- Register Skein Test----------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Skein_Test) is
      use Test_Cases.Registration;
   begin
      Register_Routine(T, Skein_Test_256'Access,"Skein_Test_256.");
      Register_Routine(T, Skein_Test_512'Access,"Skein_Test_512.");
      Register_Routine(T, Skein_Test_1024'Access,"Skein_Test_1024.");

   end Register_Tests;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------------ Name Skein Test ------------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   function Name(T : Skein_Test) return Test_String is
   begin
      return new String'("Skein Test");
   end Name;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   ------------------------------------ Start Tests ----------------------------
   -----------------------------------------------------------------------------
   -------------------------------------- Test 1 -------------------------------
   --------------------First simple test to check functionality-----------------
   -----------------------------------------------------------------------------


   procedure Skein_Test_256(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;


      Message_Short : constant Bytes(0..0) := (others=>Byte(0));
      Message : Bytes(0..31) := (others=>Byte(0));
      Message_Long : Bytes(0..63) := (others=>Byte(0));

      --Target for 1 Byte 16#00# Message data
      Target_result_Short: constant Bytes(0..31) :=
        (16#34#, 16#E2#, 16#B6#, 16#5B#, 16#F0#, 16#BE#, 16#66#, 16#7C#,
         16#A5#, 16#DE#, 16#BA#, 16#82#, 16#C3#, 16#7C#, 16#B2#, 16#53#,
         16#EB#, 16#9F#, 16#84#, 16#74#, 16#F3#, 16#42#, 16#6B#, 16#A6#,
         16#22#, 16#A2#, 16#52#, 16#19#, 16#FD#, 16#18#, 16#24#, 16#33#);

      --Target for 32 Byte incrementing from 16#FF data

      Target_result: constant Bytes(0..31) :=
        (16#8D#, 16#0F#, 16#A4#, 16#EF#, 16#77#, 16#7F#, 16#D7#, 16#59#,
         16#DF#, 16#D4#, 16#04#, 16#4E#, 16#6F#, 16#6A#, 16#5A#, 16#C3#,
         16#C7#, 16#74#, 16#AE#, 16#C9#, 16#43#, 16#DC#, 16#FC#, 16#07#,
         16#92#, 16#7B#, 16#72#, 16#3B#, 16#5D#, 16#BF#, 16#40#, 16#8B#);

      --Target for 64 Byte incrementing from 16#FF data

      Target_result_long: constant Bytes(0..31) :=
        (16#DF#, 16#28#, 16#E9#, 16#16#, 16#63#, 16#0D#, 16#0B#, 16#44#,
         16#C4#, 16#A8#, 16#49#, 16#DC#, 16#9A#, 16#02#, 16#F0#, 16#7A#,
         16#07#, 16#CB#, 16#30#, 16#F7#, 16#32#, 16#31#, 16#82#, 16#56#,
         16#B1#, 16#5D#, 16#86#, 16#5A#, 16#C4#, 16#AE#, 16#16#, 16#2F#);

      Finalresult_Short: Bytes(0..31);
      Finalresult: Bytes(0..31);
      Finalresult_Long: Bytes(0..31);


   begin

      Mode := m256;		--result length in BITS

      --        for i in Message_Short'Range loop
      --           Message_Short(i) := Byte(255-i);
      --        end loop;

      for i in Message'Range loop
         Message(i) := Byte(255-i);
      end loop;

      for i in Message_Long'Range loop

         Message_Long(i) := Byte(255-i);
      end loop;




      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 256,
         Message        => Message_Short,
         Message_Length_Bits => 8,
         Result         => Finalresult_Short);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 256,
         Message        => Message,
         Message_Length_Bits => 256,
         Result         => Finalresult);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 256,
         Message        => Message_Long,
         Message_Length_Bits => 512,
         Result         => Finalresult_Long);

      Error_Output.Put_Line(" ");
      Error_Output.Put_Line("This is the result of the hashing test");
      for i in Finalresult_Short'Range loop
         Error_Output.Put(To_Hex(Finalresult_Short(i)));
         Error_Output.Put(" ");
         if (i+1) mod 16 = 0 then
            Error_Output.Put_LIne(" ");
         end if;
      end loop;




      Assert(Finalresult_Short=Target_result_Short
             and then Finalresult=Target_result
             and then Finalresult_Long=Target_result_long, "Skein Test a)");


   end Skein_Test_256;


   procedure Skein_Test_512(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;


      Message_Short : constant Bytes(0..0) := (others=>Byte(0));
      Message : Bytes(0..63) := (others=>Byte(0));
      Message_Long : Bytes(0..127) := (others=>Byte(0));

      --Target for 1 Byte 16#00# Message data
      Target_result_Short: constant Bytes(0..63) :=
        (16#40#, 16#28#, 16#5F#, 16#43#, 16#36#, 16#99#, 16#A1#, 16#D8#,
         16#C7#, 16#99#, 16#B2#, 16#76#, 16#CC#, 16#F1#, 16#80#, 16#10#,
         16#C9#, 16#DC#, 16#9D#, 16#41#, 16#8B#, 16#0E#, 16#8A#, 16#4E#,
         16#D9#, 16#87#, 16#B4#, 16#4C#, 16#61#, 16#C0#, 16#1C#, 16#5C#,
         16#CB#, 16#CC#, 16#09#, 16#77#, 16#B1#, 16#D3#, 16#4A#, 16#4D#,
         16#36#, 16#65#, 16#D2#, 16#0E#, 16#12#, 16#71#, 16#6D#, 16#F9#,
         16#34#, 16#D2#, 16#08#, 16#FE#, 16#A6#, 16#60#, 16#7F#, 16#74#,
         16#96#, 16#8E#, 16#D8#, 16#6B#, 16#E3#, 16#C9#, 16#98#, 16#32#);

      --Target for 64 Byte incrementing from 16#FF data

      Target_result: constant Bytes(0..63) :=
        (16#45#, 16#86#, 16#3B#, 16#A3#, 16#BE#, 16#0C#, 16#4D#, 16#FC#,
         16#27#, 16#E7#, 16#5D#, 16#35#, 16#84#, 16#96#, 16#F4#, 16#AC#,
         16#9A#, 16#73#, 16#6A#, 16#50#, 16#5D#, 16#93#, 16#13#, 16#B4#,
         16#2B#, 16#2F#, 16#5E#, 16#AD#, 16#A7#, 16#9F#, 16#C1#, 16#7F#,
         16#63#, 16#86#, 16#1E#, 16#94#, 16#7A#, 16#FB#, 16#1D#, 16#05#,
         16#6A#, 16#A1#, 16#99#, 16#57#, 16#5A#, 16#D3#, 16#F8#, 16#C9#,
         16#A3#, 16#CC#, 16#17#, 16#80#, 16#B5#, 16#E5#, 16#FA#, 16#4C#,
         16#AE#, 16#05#, 16#0E#, 16#98#, 16#98#, 16#76#, 16#62#, 16#5B#);

      --Target for 128 Byte incrementing from 16#FF data

      Target_result_long: constant Bytes(0..63) :=
        (16#91#, 16#CC#, 16#A5#, 16#10#, 16#C2#, 16#63#, 16#C4#, 16#DD#,
         16#D0#, 16#10#, 16#53#, 16#0A#, 16#33#, 16#07#, 16#33#, 16#09#,
         16#62#, 16#86#, 16#31#, 16#F3#, 16#08#, 16#74#, 16#7E#, 16#1B#,
         16#CB#, 16#AA#, 16#90#, 16#E4#, 16#51#, 16#CA#, 16#B9#, 16#2E#,
         16#51#, 16#88#, 16#08#, 16#7A#, 16#F4#, 16#18#, 16#87#, 16#73#,
         16#A3#, 16#32#, 16#30#, 16#3E#, 16#66#, 16#67#, 16#A7#, 16#A2#,
         16#10#, 16#85#, 16#6F#, 16#74#, 16#21#, 16#39#, 16#00#, 16#00#,
         16#71#, 16#F4#, 16#8E#, 16#8B#, 16#A2#, 16#A5#, 16#AD#, 16#B7#);

      Finalresult_Short: Bytes(0..63);
      Finalresult: Bytes(0..63);
      Finalresult_Long: Bytes(0..63);


   begin

      Mode := m512;

      for i in Message'Range loop
         Message(i) := Byte(255-i);
      end loop;

      for i in Message_Long'Range loop

         Message_Long(i) := Byte(255-i);
      end loop;

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 512,
         Message        => Message_Short,
         Message_Length_Bits => 8,
         Result         => Finalresult_Short);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 512,
         Message        => Message,
         Message_Length_Bits => 512,
         Result         => Finalresult);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 512,
         Message        => Message_Long,
         Message_Length_Bits => 1024,
         Result         => Finalresult_Long);

      Error_Output.Put_Line(" ");
      Error_Output.Put_Line("This is the result of the hashing test");
      for i in Finalresult_Short'Range loop
         Error_Output.Put(To_Hex(Finalresult_Short(i)));
         Error_Output.Put(" ");
         if (i+1) mod 16 = 0 then
            Error_Output.Put_LIne(" ");
         end if;
      end loop;




      Assert(Finalresult_Short=Target_result_Short
             and then Finalresult=Target_result
             and then Finalresult_Long=Target_result_long, "Skein Test a)");


   end Skein_Test_512;

   procedure Skein_Test_1024(T : in out Test_Cases.Test_Case'Class) is
      use AUnit.Assertions;


      Message_Short : constant Bytes(0..0) := (others=>Byte(0));
      Message : Bytes(0..127) := (others=>Byte(0));
      Message_Long : Bytes(0..255) := (others=>Byte(0));

      --Target for 1 Byte 16#00# Message data
      Target_result_Short: constant Bytes(0..127) :=
        (16#CC#, 16#66#, 16#6D#, 16#D8#, 16#2A#, 16#8D#, 16#4D#, 16#A4#,
         16#88#, 16#00#, 16#26#, 16#5F#, 16#75#, 16#ED#, 16#5C#, 16#08#,
         16#94#, 16#E5#, 16#97#, 16#12#, 16#2F#, 16#6B#, 16#55#, 16#47#,
         16#A9#, 16#39#, 16#2F#, 16#2D#, 16#25#, 16#AD#, 16#55#, 16#62#,
         16#C1#, 16#F9#, 16#05#, 16#61#, 16#E7#, 16#02#, 16#84#, 16#E1#,
         16#9E#, 16#C0#, 16#D1#, 16#FD#, 16#20#, 16#B3#, 16#7F#, 16#D0#,
         16#97#, 16#82#, 16#3E#, 16#28#, 16#90#, 16#91#, 16#5B#, 16#D0#,
         16#9A#, 16#4C#, 16#E4#, 16#73#, 16#AB#, 16#9F#, 16#A3#, 16#80#,
         16#F3#, 16#2C#, 16#F8#, 16#64#, 16#F9#, 16#72#, 16#CA#, 16#12#,
         16#03#, 16#D5#, 16#23#, 16#75#, 16#AE#, 16#B0#, 16#71#, 16#F7#,
         16#15#, 16#9F#, 16#C9#, 16#EA#, 16#D7#, 16#54#, 16#8B#, 16#52#,
         16#B0#, 16#1F#, 16#4A#, 16#0B#, 16#37#, 16#70#, 16#46#, 16#BD#,
         16#6F#, 16#E9#, 16#DC#, 16#D6#, 16#92#, 16#31#, 16#2A#, 16#5B#,
         16#60#, 16#30#, 16#BD#, 16#DE#, 16#C5#, 16#A4#, 16#ED#, 16#B9#,
         16#3C#, 16#56#, 16#81#, 16#67#, 16#49#, 16#0A#, 16#C5#, 16#46#,
         16#B4#, 16#A6#, 16#AE#, 16#A3#, 16#F6#, 16#30#, 16#31#, 16#91#);

      --Target for 256 Byte incrementing from 16#FF data

      Target_result: constant Bytes(0..127) :=
        (16#1F#, 16#3E#, 16#02#, 16#C4#, 16#6F#, 16#B8#, 16#0A#, 16#3F#,
         16#CD#, 16#2D#, 16#FB#, 16#BC#, 16#7C#, 16#17#, 16#38#, 16#00#,
         16#B4#, 16#0C#, 16#60#, 16#C2#, 16#35#, 16#4A#, 16#F5#, 16#51#,
         16#18#, 16#9E#, 16#BF#, 16#43#, 16#3C#, 16#3D#, 16#85#, 16#F9#,
         16#FF#, 16#18#, 16#03#, 16#E6#, 16#D9#, 16#20#, 16#49#, 16#31#,
         16#79#, 16#ED#, 16#7A#, 16#E7#, 16#FC#, 16#E6#, 16#9C#, 16#35#,
         16#81#, 16#A5#, 16#A2#, 16#F8#, 16#2D#, 16#3E#, 16#0C#, 16#7A#,
         16#29#, 16#55#, 16#74#, 16#D0#, 16#CD#, 16#7D#, 16#21#, 16#7C#,
         16#48#, 16#4D#, 16#2F#, 16#63#, 16#13#, 16#D5#, 16#9A#, 16#77#,
         16#18#, 16#EA#, 16#D0#, 16#7D#, 16#07#, 16#29#, 16#C2#, 16#48#,
         16#51#, 16#D7#, 16#E7#, 16#D2#, 16#49#, 16#1B#, 16#90#, 16#2D#,
         16#48#, 16#91#, 16#94#, 16#E6#, 16#B7#, 16#D3#, 16#69#, 16#DB#,
         16#0A#, 16#B7#, 16#AA#, 16#10#, 16#6F#, 16#0E#, 16#E0#, 16#A3#,
         16#9A#, 16#42#, 16#EF#, 16#C5#, 16#4F#, 16#18#, 16#D9#, 16#37#,
         16#76#, 16#08#, 16#09#, 16#85#, 16#F9#, 16#07#, 16#57#, 16#4F#,
         16#99#, 16#5E#, 16#C6#, 16#A3#, 16#71#, 16#53#, 16#A5#, 16#78#);

      --Target for 512 Byte incrementing from 16#FF data

      Target_result_long: constant Bytes(0..127) :=
        (16#84#, 16#2A#, 16#53#, 16#C9#, 16#9C#, 16#12#, 16#B0#, 16#CF#,
         16#80#, 16#CF#, 16#69#, 16#49#, 16#1B#, 16#E5#, 16#E2#, 16#F7#,
         16#51#, 16#5D#, 16#E8#, 16#73#, 16#3B#, 16#6E#, 16#A9#, 16#42#,
         16#2D#, 16#FD#, 16#67#, 16#66#, 16#65#, 16#B5#, 16#FA#, 16#42#,
         16#FF#, 16#B3#, 16#A9#, 16#C4#, 16#8C#, 16#21#, 16#77#, 16#77#,
         16#95#, 16#08#, 16#48#, 16#CE#, 16#CD#, 16#B4#, 16#8F#, 16#64#,
         16#0F#, 16#81#, 16#FB#, 16#92#, 16#BE#, 16#F6#, 16#F8#, 16#8F#,
         16#7A#, 16#85#, 16#C1#, 16#F7#, 16#CD#, 16#14#, 16#46#, 16#C9#,
         16#16#, 16#1C#, 16#0A#, 16#FE#, 16#8F#, 16#25#, 16#AE#, 16#44#,
         16#4F#, 16#40#, 16#D3#, 16#68#, 16#00#, 16#81#, 16#C3#, 16#5A#,
         16#A4#, 16#3F#, 16#64#, 16#0F#, 16#D5#, 16#FA#, 16#3C#, 16#3C#,
         16#03#, 16#0B#, 16#CC#, 16#06#, 16#AB#, 16#AC#, 16#01#, 16#D0#,
         16#98#, 16#BC#, 16#C9#, 16#84#, 16#EB#, 16#D8#, 16#32#, 16#27#,
         16#12#, 16#92#, 16#1E#, 16#00#, 16#B1#, 16#BA#, 16#07#, 16#D6#,
         16#D0#, 16#1F#, 16#26#, 16#90#, 16#70#, 16#50#, 16#25#, 16#5E#,
         16#F2#, 16#C8#, 16#E2#, 16#4F#, 16#71#, 16#6C#, 16#52#, 16#A5#);

      Finalresult_Short: Bytes(0..127);
      Finalresult: Bytes(0..127);
      Finalresult_Long: Bytes(0..127);


   begin

      Mode := m1024;		--result length in BITS

      --        for i in Message_Short'Range loop
      --           Message_Short(i) := Byte(255-i);
      --        end loop;

      for i in Message'Range loop
         Message(i) := Byte(255-i);
      end loop;

      for i in Message_Long'Range loop

         Message_Long(i) := Byte(255-i);
      end loop;

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 1024,
         Message        => Message_Short,
         Message_Length_Bits => 8,
         Result         => Finalresult_Short);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 1024,
         Message        => Message,
         Message_Length_Bits => 1024,
         Result         => Finalresult);

      Crypto.Symmetric.Algorithm.Skein.Skein_Complete
        (Mode           => Mode,
         Output_Length_Bits            => 1024,
         Message        => Message_Long,
         Message_Length_Bits => 2048,
         Result         => Finalresult_Long);


      Error_Output.Put_Line(" ");
      Error_Output.Put_Line("This is the result of the hashing test");
      for i in Finalresult_Short'Range loop
         Error_Output.Put(To_Hex(Finalresult_Short(i)));
         Error_Output.Put(" ");
         if (i+1) mod 16 = 0 then
            Error_Output.Put_LIne(" ");
         end if;
      end loop;




      Assert(Finalresult_Short=Target_result_Short
             and then Finalresult=Target_result
             and then Finalresult_Long=Target_result_long, "Skein Test a)");


   end Skein_Test_1024;











end Test.Skein;
