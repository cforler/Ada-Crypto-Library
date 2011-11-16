------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             A U N I T . R U N                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2010, AdaCore                      --
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

with AUnit.Time_Measure;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package body AUnit.Run is

   procedure Run
     (Suite    : Access_Test_Suite;
      Results  : in out Test_Results.Result'Class;
      Options  : AUnit.Options.AUnit_Options;
      Reporter : AUnit.Reporter.Reporter'Class;
      Outcome  : out Status);
   --  Run a specific testsuite and return its status

   ---------
   -- Run --
   ---------

   procedure Run
     (Suite    : Access_Test_Suite;
      Results  : in out Test_Results.Result'Class;
      Options  : AUnit.Options.AUnit_Options;
      Reporter : AUnit.Reporter.Reporter'Class;
      Outcome  : out Status)
   is
      Time    : Time_Measure.Time;

   begin
      if Options.Global_Timer then
         Time_Measure.Start_Measure (Time);
      end if;

      pragma Warnings (Off);
      AUnit.Test_Suites.Run (Suite, Options, Results, Outcome);
      pragma Warnings (On);

      if Options.Global_Timer then
         Time_Measure.Stop_Measure (Time);
         Test_Results.Set_Elapsed (Results, Time);
      end if;

      AUnit.Reporter.Report (Reporter, Results);
   end Run;

   -----------------
   -- Test_Runner --
   -----------------

   procedure Test_Runner
     (Reporter : AUnit.Reporter.Reporter'Class;
      Options  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options)
   is
      Results : Test_Results.Result;
      Outcome : Status;
      pragma Unreferenced (Outcome);
   begin
      Test_Results.Clear (Results);
      Run (Suite, Results, Options, Reporter, Outcome);
   end Test_Runner;

   -----------------------------
   -- Test_Runner_With_Status --
   -----------------------------

   function Test_Runner_With_Status
     (Reporter : AUnit.Reporter.Reporter'Class;
      Options  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options)
      return Status
   is
      Results : Test_Results.Result;
      Outcome : Status;
   begin
      Test_Results.Clear (Results);
      Run (Suite, Results, Options, Reporter, Outcome);
      return Outcome;
   end Test_Runner_With_Status;

   ------------------------------
   -- Test_Runner_With_Results --
   ------------------------------

   procedure Test_Runner_With_Results
     (Reporter : AUnit.Reporter.Reporter'Class;
      Results  : in out AUnit.Test_Results.Result'Class;
      Options  : AUnit.Options.AUnit_Options := AUnit.Options.Default_Options)
   is
      Outcome : Status;
      pragma Unreferenced (Outcome);
   begin
      Run (Suite, Results, Options, Reporter, Outcome);
   end Test_Runner_With_Results;

end AUnit.Run;
