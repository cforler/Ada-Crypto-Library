------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
--                                                                          --
--                                 S p e c                                  --
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

with Ada_Containers; use Ada_Containers;
with Ada_Containers.AUnit_Lists;
with AUnit.Options;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Results; use AUnit.Test_Results;

pragma Elaborate_All(Ada_Containers.AUnit_Lists);

--  Test case: a collection of test routines
package AUnit.Test_Cases is

   type Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with
     private;
   type Test_Case_Access is access all Test_Case'Class;

   type Test_Routine is access procedure (Test : in out Test_Case'Class);

   type Routine_Spec is record
      Routine      : Test_Routine;
      Routine_Name : Message_String;
   end record;

   procedure Add_Routine (T : in out Test_Case'Class; Val : Routine_Spec);

   procedure Register_Tests (Test : in out Test_Case) is abstract;
   --  Register test methods with test suite

   procedure Set_Up_Case (Test : in out Test_Case);
   --  Set up performed before each test case (set of test routines)

   procedure Tear_Down_Case (Test : in out Test_Case);
   --  Tear down performed after each test case

   package Registration is

      procedure Register_Routine
        (Test    : in out Test_Case'Class;
         Routine : Test_Routine;
         Name    : String);
      --  Add test routine to test case

      function Routine_Count (Test : Test_Case'Class) return Count_Type;
      --  Count of registered routines in test case

   end Registration;

   generic
      type Specific_Test_Case is abstract new Test_Case with private;
   package Specific_Test_Case_Registration is
      --  Specific Test Case registration

      type Specific_Test_Routine is access procedure
        (Test : in out Specific_Test_Case'Class);

      procedure Register_Wrapper
        (Test    : in out Specific_Test_Case'Class;
         Routine : Specific_Test_Routine;
         Name    : String);
      --  Add test routine for a specific test case
   end Specific_Test_Case_Registration;

   procedure Run
     (Test    : access Test_Case;
      Options :        AUnit.Options.AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status);
   --  Run test case. Do not override.

   procedure Run_Test (Test : in out Test_Case);
   --  Perform the current test procedure. Do not override.

   function Routine_Name (Test : Test_Case) return Message_String;
   --  Routine name. Returns the routine under test. Do not override.

private

   type Routine_Access is access all Routine_Spec;
   --  Test routine description

   package Routine_Lists is new Ada_Containers.AUnit_Lists (Routine_Spec);
   --  Container for test routines

   package Failure_Lists is
     new Ada_Containers.AUnit_Lists (Message_String);
   --  Container for failed assertion messages per routine

   type Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with record
      Routines : aliased Routine_Lists.List;
      Routine  : Routine_Spec;
   end record;

end AUnit.Test_Cases;
