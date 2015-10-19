with Ada.Text_IO;
use Ada.Text_IO;

package body AUnit.Test_Results.ACL is
   
    procedure Add_Error
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Error        : Test_Error;
      Elapsed      : Time) is
    begin
       Put("U");
       Result(R).Add_Error(Test_Name, Routine_Name, Error, Elapsed);
    end Add_Error; 
    
    procedure Add_Failure
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Failure      : Test_Failure;
      Elapsed      : Time) is
    begin
       Put("-");
       Result(R).Add_Failure(Test_Name, Routine_Name, Failure, Elapsed);
    end Add_Failure;  
    
    
     procedure Add_Success
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Elapsed      : Time) is
     begin
	Put("+");
	Result(R).Add_Success(Test_Name, Routine_Name,Elapsed);
     end Add_Success;  
end AUnit.Test_Results.ACL;
