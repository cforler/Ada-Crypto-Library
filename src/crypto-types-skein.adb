------------------------------------------------------------------------
--
-- Implementation of the Skein hash function.
-- Definition of types
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------

--with skein_debug;               use skein_debug;
with Ada.Text_IO;			    use Ada.Text_IO;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Interfaces;

package body Crypto.Types.Skein is

   ------------------------------------------------------------------
   -- we need some functions to Create Dwords from various inputs
   ------------------------------------------------------------------

   --we want to be able to multiply Integers and Bytes
   --we need this in Bytes_To_int for example
   --    function "*"(Left : Byte;
   --            Right : Integer) return Integer is
   --    begin
   --        return Integer(Left) * Right;
   --    end "*";

   --    function "+" (Left,Right : Dword) return Dword is
   --    begin
   --        return Left xor Right;
   --    end "+" ;
   --    function "xor" (Left,Right : Dword) return Dword is
   --    begin
   --        return Left + Right;
   --    end "xor" ;

   function "+" (Left : DWord; Right : Integer) return DWord is
   begin
      return Left + DWord (Right);
   end "+";

   function Natural_To_Bytes (N : Natural; number : Natural) return Bytes is
      result : Bytes (0 .. number - 1) := (others => Byte (0));
   begin
      for i in result'Range loop  --Natural can be at least 256**3
         --result(i) := Byte( ( N/(256**i) ) mod 256);
         if i < 4 then
            result (i) := Byte ((N / (256 ** i)) mod 256);
            --Ada.Text_IO.Put_Line(Show_Hex(result(i)));
         end if;
      end loop;
      return result;
   end Natural_To_Bytes;

   function Bytes_To_Dword (b : in Bytes) return DWord is
      My_SW : DWord := DWord (0);
   begin
      if not (b'Length = 8) then
         Put_Line
           ("The Length of Bytes must be 8 for converting to Dword");
         raise Program_Error;
      end if;
      --8 Bytes are one word,
      for j in b'Range loop
         --we need mod 8 here because in call from Bytes_To_Dwords
         --the indices are kept :/
         My_SW := My_SW + (DWord (b (j)) * 256 ** (j mod 8));
      end loop;
      return My_SW;
   end Bytes_To_Dword;

   --8 bytes are one Dword
   function Bytes_To_Dwords (b : in Bytes) return DWords is
      My_SW_Array : DWords (0 .. b'Length / 8 - 1);
   begin
      if not (b'Length mod 8 = 0) then
         Put_Line ("The Length of Bytes must be a multiple of 8");
         Put_Line (Integer'Image (b'Length));
         raise Program_Error;
      end if;
      for i in My_SW_Array'Range loop
         My_SW_Array (i) := Bytes_To_Dword (b (8 * i .. 8 * i + 7));
      end loop;
      return My_SW_Array;
   end Bytes_To_Dwords;

   --convert one single Dword to an array of 8 Bytes
   function Dword_To_Bytes (s : in DWord) return Bytes is
      My_Bytes_Array : Bytes (0 .. 7);
   begin
      for i in My_Bytes_Array'Range loop
         My_Bytes_Array (i) := Byte (s / 256 ** i mod 256);
      end loop;
      return My_Bytes_Array;
   end Dword_To_Bytes;

   --one Dword is 8 Bytes
   function Dwords_To_Bytes (s : in DWords) return Bytes is
      My_Bytes : Bytes (0 .. s'Length * 8 - 1);
   begin
      for i in s'Range loop
         My_Bytes (i * 8 .. i * 8 + 7) := Dword_To_Bytes (s (i));
      end loop;
      return My_Bytes;
   end Dwords_To_Bytes;

   function "+" (Left : Bytes; Right : Natural) return Bytes is
   begin
      if not (Left'Length = 8) then
         Put_Line ("maximum of 8 Byte is allowed for Addition");
         raise Program_Error;
      end if;
      return Dword_To_Bytes (Bytes_To_Dword (Left) + Right);
   end "+";



end Crypto.Types.Skein;
