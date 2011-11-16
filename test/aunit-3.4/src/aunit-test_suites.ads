------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
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
--  A collection of test cases.

with Ada_Containers;
with Ada_Containers.AUnit_Lists;
with AUnit.Options;           use AUnit.Options;
with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with AUnit.Tests;
with AUnit.Test_Results;      use AUnit.Test_Results;

pragma Elaborate_All(Ada_Containers.AUnit_Lists);
package AUnit.Test_Suites is

   type Test_Suite is new AUnit.Tests.Test with private;
   type Access_Test_Suite is access all Test_Suite'Class;

   procedure Add_Test (S : access Test_Suite'Class;
                       T : access Test_Suite'Class);
   procedure Add_Test (S : access Test_Suite'Class;
                       T : access Test_Case'Class);
   --  Add a test case or suite to this suite

   procedure Run (Suite   : access Test_Suite;
                  Options :        AUnit_Options;
                  R       : in out Result'Class;
                  Outcome :    out Status);
   --  Run all tests collected into this suite

   function New_Suite return Access_Test_Suite;
   --  Create a new test suite

private

   type Test_Suite_Elt_Kind is
     (TC_Elt,
      TS_Elt);

   type Test_Suite_Element (Kind : Test_Suite_Elt_Kind := TC_Elt) is record
      case Kind is
         when TC_Elt =>
            TC : Test_Case_Access;
         when TS_Elt =>
            TS : Access_Test_Suite;
      end case;
   end record;

   use Ada_Containers;

   package Test_Lists is new Ada_Containers.AUnit_Lists (Test_Suite_Element);
   use Test_Lists;
   --  Containers for test cases and sub-suites

   type Test_Suite is new AUnit.Tests.Test with record
      Tests : aliased Test_Lists.List;
   end record;

end AUnit.Test_Suites;
