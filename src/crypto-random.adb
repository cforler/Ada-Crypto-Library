-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.
--with Ada.Numerics.Discrete_Random;
with Interfaces.C_Streams;
use  Interfaces.C_Streams;

use type Interfaces.C_Streams.Size_T;


package body Crypto.Random is

   Random_File : Files;

   ---------------------------------------------------------------------------

   procedure Read(B : out Byte) is
      I : Int;
   begin
      I := fgetc(Random_File);
      if  I < 0 then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
      B := Byte(I);
   end Read;
   
  ---------------------------------------------------------------------------

--   procedure Read(B : out Byte) is
--      R, F : Float;
--      I : Integer;
--      P : Positive;

--   begin
--      Rand.Reset(G);
--      P := Rand.Random(G);
--      for I in 1..P loop
--         R := Next;
--      end loop;
--		
--      F := (255.0)*Next;
--      I := Integer(F);
--      B := Byte(I);
--   end Read;

   ---------------------------------------------------------------------------


   procedure Read(Byte_Array : out Bytes) is
   begin
      if  Fread( Buffer => Byte_Array'address, Size => 1,
		 Count => Byte_Array'Length, Stream =>  Random_File) 
	/= Byte_Array'Length then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
   end read;

   ---------------------------------------------------------------------------
   procedure Read(B : out B_Block128) is
   begin
      Read(Bytes(B));
   end Read;
   
   ---------------------------------------------------------------------------

   procedure Read(W : out Word) is
   begin
      if Fread( Buffer => W'Address, Size => W'Size/8,
		Count => 1, Stream =>  Random_File) /= 1 then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
   end Read;

   ---------------------------------------------------------------------------

   procedure Read(Word_Array : out Words) is
   begin
      if Fread(Buffer => Word_Array'Address, Size => Word'Size/8 ,
	       Count => Word_Array'Length, Stream =>  Random_File)
	/=  Word_Array'Length  then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
   end Read;


   ---------------------------------------------------------------------------

   procedure Read(D : out DWord) is
   begin
      if  Fread( Buffer => D'Address, Size => D'Size/8,
		 Count => 1, Stream =>  Random_File) /= 1 then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
   end Read;

   ---------------------------------------------------------------------------


   procedure Read(DWord_Array : out DWords) is
   begin
      if  Fread(Buffer => DWord_Array'Address, Size => DWord'Size/8,
		Count => DWord_Array'Length, Stream =>  Random_File)
	/=  DWord_Array'Length  then
         raise  RANDOM_SOURCE_READ_ERROR;
      end if;
   end Read;

   ---------------------------------------------------------------------------

--   M3     : constant := 97;
--   Init_C : constant := 362436.0 / 16777216.0;
--   CD     : constant := 7654321.0 / 16777216.0;
--   CM     : constant := 16777213.0 / 16777216.0;

--   subtype Range_1 is Integer range 0..M1-1;
--   subtype Range_2 is Integer range 0..M2-1;
--   subtype Range_3 is Integer range 1..M3;

--   I,
--   J,
--   K  : Range_1;
--   NI,
--   NJ : Integer;
--   L  : Range_2;
--   C  : Float;
--   U  : array (Range_3) of Float;

   ---------------------------------------------------------------------------
--   procedure Start (
--         New_I : Seed_Range_1 := Default_I;
--         New_J : Seed_Range_1 := Default_J;
--         New_K : Seed_Range_1 := Default_K;
--         New_L : Seed_Range_2 := Default_L) is
--      S,
--      T : Float;
--      M : Range_1;
--   begin
--      I:=New_I;
--      J:=New_J;
--      K:= New_K;
--      L:= New_L;
--      Ni:=Range_3'Last;
--      Nj:=(Range_3'Last/3)+1;
--      C:=Init_C;

--      for Ii in Range_3 loop
--         S:=0.0;
--         T:=0.5;
--         for Jj in 1..24 loop
--            M:=(((J*I) mod M1)*K) mod M1;
--            I:=J;
--            J:=K;
--            K:=M;
--            L:=(53*L+1) mod M2;
--            if((L*M) mod 64) >= 32 then
--               S:= S+T;
--            end if;
--            T:=0.5*T;
--         end loop;
--         U(Ii):=S;
--      end loop;
--   end Start;

   ---------------------------------------------------------------------------

--   function Next return Float is
--      Temp : Float;
--   begin
--      Temp := U(Ni) -U(Nj);
--      if Temp < 0.0 then
--         Temp := Temp +1.0;
--      end if;
--      U(Ni) := Temp;
--      Ni := Ni-1;
--      if Ni = 0 then
--         Ni := Range_3'Last;
--      end if;
--      Nj := Nj-1;
--      if Nj = 0 then
--         Nj:= Range_3'Last;
--      end if;
--      C:=C-Cd;
--      if C<0.0 then
--         C:=C+Cm;
--      end if;
--      Temp:=Temp-C;
--      if Temp<0.0 then
--         Temp:=Temp+1.0;
--      end if;
--      return Temp;
--   end Next;

   ---------------------------------------------------------------------------

   Cpath : constant String := Path & ASCII.NUL;
   Mode :  constant String := "r";

begin
   Random_File := Fopen(Cpath'address, Mode'address);
   if Ferror( Random_File) /= 0 then
      raise RANDOM_SOURCE_DOES_NOT_EXISTS_ERROR;
   end if;
   ----------------------------------NEU--------------------------------------
   --   Start;
end Crypto.Random;
