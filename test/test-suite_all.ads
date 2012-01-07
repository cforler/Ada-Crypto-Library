with AUnit; use AUnit;
with AUnit.Test_Suites;

package Test.Suite_All is
        function Suite return Test_Suites.Access_Test_Suite;
end Test.Suite_All;
