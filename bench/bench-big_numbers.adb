-- Author: schmolli


with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar;        use Ada.Calendar;

package body Bench.Big_Numbers is

   function Bench_Big_Numbers return Boolean is
      Result : Boolean := True;
      R      : Big_Unsigned;
      Start  : Time ;
      Ende   : Time ;
      Spanne : Duration ;
      N:Natural;
      Initial : Big_Unsigned;
      A, B    : Big_Unsigned ;


   begin



      
      if CSV then
         null;
      else
         Put_Line("Multiplication-Test : ");
         Put_Line("===================== ");
      end if;
      
      for I in 1..1 loop
         Initial := Get_Random;
         N := Bit_Length(Initial)/2;
         A  := Shift_Right(Initial, N);
         B  := Initial - Shift_Left( A, N );

         if CSV then
            Put(Natural'Image(Bit_Length(Initial)));
         else
            Put("Bit_Length(Initial : ");  Put(Natural'Image(Bit_Length(Initial)));  New_Line;
            Put("N                  : ");  Put(N);  New_Line;
            Put("Bit_Length(A)      : ");  Put(Bit_Length(A));  New_Line;
            Put("Bit_Length(B)      : ");  Put(Bit_Length(B));  New_Line;
         end if;

         if not CSV then Put("School:"); end if;
         Start := Clock;
         R := A*B;
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer       : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         
         if not CSV then Put("Russ:"); end if;
         Start := Clock;
         R := Russ(A,B);
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer         : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         
         if not CSV then Put("Kara:"); end if;
         Start := Clock;
         R := Karatsuba(A, B);
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer         : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         
         if not CSV then Put("Kara_P:");  end if;
         Start := Clock;
         R := Karatsuba_P(A, B);
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer       : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         
         if not CSV then Put("T-C:"); end if;
         Start := Clock;
         R := Toom_Cook(A,B);
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer          : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         
         if not CSV then Put("T-C_P:"); end if;
         Start := Clock;
         R := Toom_Cook_P(A,B);
         Ende := Clock;
         Spanne := Ende - Start;
         if CSV then
            Put( "," & Duration'Image(Spanne) );
         else
            Put("Dauer        : ");  Put(Duration'Image(Spanne));  --New_Line;
            if R/A = B then Put_Line(" PASSED "); else Put_Line(" FAILED "); end if;
         end if;
         
         if CSV then
            New_Line;
         else
            Put_Line("-------------------------------------");
         end if;
         
      end loop;

      return Result;
   end Bench_Big_Numbers;
end Bench.Big_Numbers;



