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

with Crypto.Types; use Crypto.Types;
--with Ada.Numerics.Discrete_Random;


package Crypto.Random is

   procedure Read(B : out Byte);
   procedure Read(Byte_Array : out Bytes);
   procedure Read(B : out B_Block128);

   procedure Read(W : out Word);
   procedure Read(Word_Array : out Words);

   procedure Read(D : out DWord);
   procedure Read(DWord_Array : out DWords);

   ---------------------------------------------------------------------------
--   M1 : constant := 179;
--   M2 : constant := M1 - 10;

--   subtype Seed_Range_1 is Integer range 1..M1-1;
--   subtype Seed_Range_2 is Integer range 1..M2-1;

--   Default_I : constant Seed_Range_1 := 12;
--   Default_J : constant Seed_Range_1 := 34;
--   Default_K : constant Seed_Range_1 := 56;
--   Default_L : constant Seed_Range_1 := 78;

--   procedure Start(
--      New_I : Seed_Range_1 := Default_I;
--      New_J : Seed_Range_1 := Default_J;
--      New_K : Seed_Range_1 := Default_K;
--      New_L : Seed_Range_2 := Default_L);

--   function Next return Float;


--   subtype RR is Positive range 10000..50000;
--   package Rand is new Ada.Numerics.Discrete_Random(RR);
--   G : Rand.Generator;

   ---------------------------------------------------------------------------
   --------------------------EXCEPTIONS---------------------------------------
   ---------------------------------------------------------------------------


   Random_Source_Does_Not_Exists_Error : exception;
   Random_Source_Read_Error            : exception;


   ---------------------------------------------------------------------------
   --------------------------PRIVATE------------------------------------------
   ---------------------------------------------------------------------------

private

   -- Random Source
   Path :  constant String := "/dev/random";

   pragma Inline (Read);
   pragma Optimize (Time);

end Crypto.Random;
