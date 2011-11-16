------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2010, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with AUnit.Memory.Utils;

package body AUnit.Test_Suites is

   --------------
   -- Add_Test --
   --------------

   procedure Add_Test
     (S : access Test_Suite'Class;
      T : access Test_Suite'Class)
   is
   begin
      Append
        (S.Tests,
         (Kind => TS_Elt,
          TS   => Access_Test_Suite'(T.all'Unchecked_Access)));
   end Add_Test;

   --------------
   -- Add_Test --
   --------------

   procedure Add_Test
     (S : access Test_Suite'Class;
      T : access Test_Case'Class)
   is
   begin
      Append
        (S.Tests,
         (Kind => TC_Elt,
          TC   => Test_Case_Access'(T.all'Unchecked_Access)));
   end Add_Test;

   ---------
   -- Run --
   ---------

   procedure Run (Suite   : access Test_Suite;
                  Options :        AUnit_Options;
                  R       : in out Result'Class;
                  Outcome :    out Status)
   is
      C      : Cursor := First (Suite.Tests);
      Result : Status := Success;

   begin
      Outcome := Success;
      while Has_Element (C) loop
         case Element (C).Kind is
            when TC_Elt =>
               Run (Element (C).TC, Options, R, Result);
            when TS_Elt =>
               Run (Element (C).TS, Options, R, Result);
         end case;

         if Result = Failure then
            Outcome := Failure;
         end if;

         Next (C);
      end loop;
   end Run;

   ---------------
   -- New_Suite --
   ---------------

   function New_Suite return Access_Test_Suite is
      type Access_Type is access all Test_Suite;
      pragma No_Strict_Aliasing (Access_Type);
      function Alloc is new AUnit.Memory.Utils.Gen_Alloc
        (Test_Suite, Access_Type);
      function Convert is new Ada.Unchecked_Conversion
        (Access_Type, Access_Test_Suite);
      Ret : constant Access_Type := Alloc;
      Obj : Test_Suite;
      for Obj'Address use Ret.all'Address;
      pragma Warnings (Off, Obj);
   begin
      return Convert (Ret);
   end New_Suite;

end AUnit.Test_Suites;
