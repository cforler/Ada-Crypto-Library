package AUnit.Test_Results.ACL is
   use  AUnit.Test_Results;
   type ACL_Result is new Result with null record;
   
   Overriding 
    procedure Add_Error
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Error        : Test_Error;
      Elapsed      : Time);
   --  Record an unexpected exception
   
   Overriding 
   procedure Add_Failure
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Failure      : Test_Failure;
      Elapsed      : Time);
   --  Record a test routine failure
   
   Overriding 
   procedure Add_Success
     (R            : in out ACL_Result;
      Test_Name    : Message_String;
      Routine_Name : Message_String;
      Elapsed      : Time);
   --  Record a test routine success  
end AUnit.Test_Results.ACL;
