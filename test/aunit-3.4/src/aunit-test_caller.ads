------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ C A L L E R                     --
--                                                                          --
--                                 S p e c                                  --
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

--  <description>
--  A Test caller provides access to a test case type based on a test fixture.
--  Test callers are useful when you want to run individual test or add it to
--  a suite.
--  Test callers invoke only one Test (i.e. test method) on one Fixture of a
--  AUnit.Test_Fixtures.Test_Fixture.
--
--  Here is an example:
--
--  <code>
--  package Math_Test is
--     Type Test is new AUnit.Test_Fixtures.Test_Fixture with record
--        M_Value1 : Integer;
--        M_Value2 : Integer;
--     end record;
--
--     procedure Set_Up (T : in out Test);
--
--     procedure Test_Addition (T : in out Test);
--     procedure Test_Subtraction (T : in out Test);
--
--  end Math_Test;
--
--  function Suite return AUnit.Test_Suites.Test_Suite_Access is
--     package Caller is new AUnit.Test_Caller (Math_Test.Test);
--     The_Suite       : AUnit.Test_Suites.Test_Suite_Access :=
--                         new AUnit.Test_Suites.Test_Suite;
--  begin
--     The_Suite.Add_Test
--      (Caller.Create ("Test Addition on integers",
--                      Math_Test.Test_Addition'Access));
--     The_Suite.Add_Test
--      (Caller.Create ("Test Subtraction on integers",
--                      Math_Test.Test_Subtraction'Access));
--     return The_Suite;
--  end Suite;
--  </code>
--  </description>

with AUnit.Simple_Test_Cases;
with AUnit.Test_Fixtures;

generic

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with private;

package AUnit.Test_Caller is

   type Test_Case is new AUnit.Simple_Test_Cases.Test_Case with private;
   type Test_Case_Access is access all Test_Case'Class;

   type Test_Method is access procedure (Test : in out Test_Fixture);

   function Create
     (Name : String;
      Test : Test_Method) return Test_Case_Access;
   --  Return a test case from a test fixture method, reporting the result
   --  of the test using the Name parameter.

   procedure Create
     (TC   : out Test_Case'Class;
      Name : String;
      Test : Test_Method);
   --  Initialize a test case from a test fixture method, reporting the result
   --  of the test using the Name parameter.

   function Name (Test : Test_Case) return Message_String;
   --  Test case name

   procedure Run_Test (Test : in out Test_Case);
   --  Perform the test.

   procedure Set_Up (Test : in out Test_Case);
   --  Set up performed before each test case

   procedure Tear_Down (Test : in out Test_Case);
   --  Tear down performed after each test case

private

   type Fixture_Access is access all Test_Fixture;
   pragma No_Strict_Aliasing (Fixture_Access);

   type Test_Case is new AUnit.Simple_Test_Cases.Test_Case with record
      Fixture : Fixture_Access;
      Name    : Message_String;
      Method  : Test_Method;
   end record;

end AUnit.Test_Caller;
