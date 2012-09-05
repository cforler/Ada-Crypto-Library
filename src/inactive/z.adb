with Ada.Text_IO; use Ada.Text_IO;
with Crypto.Types; use Crypto.Types;

procedure Z is
   package BIO is new Ada.Text_IO.Modular_IO (Byte);
   package WIO is new Ada.Text_IO.Modular_IO (Word);
   
   B : Bytes(0..7);
   A : Bytes(0..7) := (34,67,124,232,214,others => 255);

   

begin
   for I in 0..80 loop
      B := Shift_Left(A,I);
      for J in B'Range loop
	 Bio.Put(B(J), Width=> 12, Base => 2);
      end loop;
	New_Line;
   end loop;
end Z;
