------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     L A S T _ C H A N C E _ H A N D L E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2008-2010, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C;
with Ada.Unchecked_Conversion;

package body AUnit.Last_Chance_Handler is

   Exception_Name    : Message_String := null;
   Exception_Message : Message_String := null;

   type Jmp_Buff is array (1 .. 5) of System.Address;
   type Jmp_Buff_Address is access all Jmp_Buff;
   --  type expected by setjmp call

   function Builtin_Setjmp (Buff : Jmp_Buff_Address) return Integer;
   pragma Import (Intrinsic, Builtin_Setjmp, "__builtin_setjmp");

   procedure Builtin_Longjmp (Buff : Jmp_Buff_Address; Flag : Integer);
   pragma Import (Intrinsic, Builtin_Longjmp, "__builtin_longjmp");
   pragma No_Return (Builtin_Longjmp);

   --  handle at most 5 handlers at the same time
   Jmp_Buffer   : array (1 .. 5) of aliased Jmp_Buff;
   Jmp_Buff_Idx : Integer := Jmp_Buffer'First;

   ---------------------------
   --  C Strings management --
   ---------------------------

   type chars_ptr is access all Character;
   pragma No_Strict_Aliasing (chars_ptr);

   function To_chars_ptr is
      new Ada.Unchecked_Conversion (System.Address, chars_ptr);

   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, System.Address);

   function To_Ada
     (Item : chars_ptr; Line : Integer := 0) return Message_String;

   ----------------
   -- Gen_Setjmp --
   ----------------

   function Gen_Setjmp return Integer
   is
      Ret : Integer;
   begin
      Ret := Builtin_Setjmp (Jmp_Buffer (Jmp_Buff_Idx)'Access);

      if Ret = 0 then
         Jmp_Buff_Idx := Jmp_Buff_Idx + 1;
         Proc;
         Jmp_Buff_Idx := Jmp_Buff_Idx - 1;
      end if;

      return Ret;
   end Gen_Setjmp;

   ------------------------
   -- Get_Exception_Name --
   ------------------------

   function Get_Exception_Name return Message_String is
   begin
      if Exception_Message = null then
         return AUnit.Message_Alloc (0);
      else
         return Exception_Name;
      end if;
   end Get_Exception_Name;

   ---------------------------
   -- Get_Exception_Message --
   ---------------------------

   function Get_Exception_Message return Message_String is
   begin
      if Exception_Message = null then
         return AUnit.Message_Alloc (0);
      else
         return Exception_Message;
      end if;
   end Get_Exception_Message;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (Item : chars_ptr; Line : Integer := 0) return Message_String
   is
      use Interfaces.C;
      Result   : Message_String;
      Length   : size_t := 0;
      Line_Img : String (1 .. Integer'Width);
      First    : Natural := Line_Img'Last + 1;

      function "+" (Left : chars_ptr; Right : size_t) return chars_ptr;
      --  Return the address Right character right of address Left.

      function Peek (From : chars_ptr) return char;
      --  Return the character at address From

      function To_Ada (Item : char) return Character;
      --  Translate char to an Ada Character

      ---------
      -- "+" --
      ---------

      function "+" (Left : chars_ptr; Right : size_t) return chars_ptr is
      begin
         return To_chars_ptr (To_Address (Left) + Storage_Offset (Right));
      end "+";

      ----------
      -- Peek --
      ----------

      function Peek (From : chars_ptr) return char is
      begin
         return char (From.all);
      end Peek;

      ------------
      -- To_Ada --
      ------------

      function To_Ada (Item : char) return Character is
      begin
         return Character'Val (char'Pos (Item));
      end To_Ada;

   begin
      if Item = null then
         return null;
      end if;

      --  Compute the Length of "Item"
      loop
         if Peek (Item + Length) = nul then
            exit;
         end if;

         Length := Length + 1;
      end loop;

      --  Compute the image of Line
      if Line /= 0 then
         declare
            Int   : Integer;
            Val   : Natural;

         begin
            Int := Line;

            loop
               Val := Int mod 10;
               Int := (Int - Val) / 10;
               First := First - 1;
               Line_Img (First) := Character'Val (Val + Character'Pos ('0'));
               exit when Int = 0;
            end loop;
         end;
      end if;

      if Line /= 0 then
         Result := AUnit.Message_Alloc
           (Natural (Length) +  Line_Img'Last - First + 2);
      else
         Result := AUnit.Message_Alloc (Natural (Length));
      end if;

      for J in 1 .. Integer (Length) loop
         Result (J) := To_Ada (Peek (Item + size_t (J - 1)));
      end loop;

      if Line /= 0 then
         Result (Integer (Length + 1)) := ':';
         for J in First .. Line_Img'Last loop
            Result (Integer (Length + 2) + J - First) := Line_Img (J);
         end loop;
      end if;

      return Result;
   end To_Ada;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      procedure OS_Exit (Status : Integer);
      pragma Import (C, OS_Exit, "exit");
      pragma No_Return (OS_Exit);

   begin
      --  Save the exception message before performing the longjmp
      Exception_Name   := Format ("Unexpected exception in zfp profile");
      if Line = 0 then
         Exception_Message := To_Ada (To_chars_ptr (Msg));
      else
         Exception_Message := To_Ada (To_chars_ptr (Msg), Line);
      end if;

      Jmp_Buff_Idx := Jmp_Buff_Idx - 1;

      if Jmp_Buff_Idx >= Jmp_Buffer'First then
         --  No return procedure.
         Builtin_Longjmp (Jmp_Buffer (Jmp_Buff_Idx)'Access, 1);
      else
         OS_Exit (1);
      end if;
   end Last_Chance_Handler;

end AUnit.Last_Chance_Handler;
