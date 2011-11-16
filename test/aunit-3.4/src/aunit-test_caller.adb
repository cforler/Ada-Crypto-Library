------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ C A L L E R                     --
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

with Ada.Unchecked_Conversion;
with AUnit.Assertions;
with AUnit.Memory.Utils;

package body AUnit.Test_Caller is

   function New_Fixture is new AUnit.Memory.Utils.Gen_Alloc
     (Test_Fixture, Fixture_Access);

   The_Fixture_Object : constant Fixture_Access := New_Fixture;

   ------------
   -- Create --
   ------------

   procedure Create
     (TC   : out Test_Case'Class;
      Name : String;
      Test : Test_Method)
   is
   begin
      TC.Name    := Format (Name);
      TC.Method  := Test;
      TC.Fixture := The_Fixture_Object;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name : String;
      Test : Test_Method) return Test_Case_Access
   is
      type Access_Type is access all Test_Case;
      function Alloc is new AUnit.Memory.Utils.Gen_Alloc
        (Test_Case, Access_Type);
      function Convert is new Ada.Unchecked_Conversion
        (Access_Type, Test_Case_Access);
      Ret : constant Test_Case_Access := Convert (Alloc);
   begin
      Create (Ret.all, Name, Test);
      return Ret;
   end Create;

   ----------
   -- Name --
   ----------

   function Name (Test : Test_Case) return Message_String is
   begin
      return Test.Name;
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Test_Case) is
   begin
      --  Before running the fixture's method, we need to make sure that
      --  the test Ids correspond so that a failure reported via Fixture is
      --  correctly understood as being part of Test.
      AUnit.Assertions.Copy_Id (Test, Test.Fixture.all);
      Test.Method (Test_Fixture (Test.Fixture.all));
   end Run_Test;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
   begin
      Set_Up (Test.Fixture.all);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
   begin
      Tear_Down (Test.Fixture.all);
   end Tear_Down;

end AUnit.Test_Caller;
