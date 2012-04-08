pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~test-big_number.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~test-big_number.adb");

with System.Restrictions;

package body ada_main is
   pragma Warnings (Off);

   procedure Do_Finalize;
   pragma Import (C, Do_Finalize, "system__standard_library__adafinal");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   procedure adainit is
      E208 : Boolean; pragma Import (Ada, E208, "system__stack_usage_E");
      E013 : Boolean; pragma Import (Ada, E013, "system__soft_links_E");
      E138 : Boolean; pragma Import (Ada, E138, "system__fat_flt_E");
      E023 : Boolean; pragma Import (Ada, E023, "system__exception_table_E");
      E082 : Boolean; pragma Import (Ada, E082, "ada__io_exceptions_E");
      E141 : Boolean; pragma Import (Ada, E141, "ada__strings_E");
      E147 : Boolean; pragma Import (Ada, E147, "ada__strings__maps_E");
      E005 : Boolean; pragma Import (Ada, E005, "ada__tags_E");
      E057 : Boolean; pragma Import (Ada, E057, "ada__streams_E");
      E120 : Boolean; pragma Import (Ada, E120, "interfaces__c_E");
      E122 : Boolean; pragma Import (Ada, E122, "interfaces__c__strings_E");
      E188 : Boolean; pragma Import (Ada, E188, "system__task_info_E");
      E068 : Boolean; pragma Import (Ada, E068, "ada__calendar_E");
      E017 : Boolean; pragma Import (Ada, E017, "system__secondary_stack_E");
      E072 : Boolean; pragma Import (Ada, E072, "system__finalization_root_E");
      E092 : Boolean; pragma Import (Ada, E092, "system__finalization_implementation_E");
      E090 : Boolean; pragma Import (Ada, E090, "ada__finalization_E");
      E143 : Boolean; pragma Import (Ada, E143, "ada__strings__unbounded_E");
      E088 : Boolean; pragma Import (Ada, E088, "ada__finalization__list_controller_E");
      E128 : Boolean; pragma Import (Ada, E128, "system__file_control_block_E");
      E159 : Boolean; pragma Import (Ada, E159, "ada__streams__stream_io_E");
      E118 : Boolean; pragma Import (Ada, E118, "system__file_io_E");
      E125 : Boolean; pragma Import (Ada, E125, "system__os_lib_E");
      E157 : Boolean; pragma Import (Ada, E157, "system__strings__stream_ops_E");
      E224 : Boolean; pragma Import (Ada, E224, "system__tasking__initialization_E");
      E230 : Boolean; pragma Import (Ada, E230, "system__tasking__protected_objects_E");
      E170 : Boolean; pragma Import (Ada, E170, "ada__real_time_E");
      E113 : Boolean; pragma Import (Ada, E113, "ada__text_io_E");
      E234 : Boolean; pragma Import (Ada, E234, "system__tasking__protected_objects__entries_E");
      E238 : Boolean; pragma Import (Ada, E238, "system__tasking__queuing_E");
      E244 : Boolean; pragma Import (Ada, E244, "system__tasking__stages_E");
      E062 : Boolean; pragma Import (Ada, E062, "ada_containers__aunit_lists_E");
      E053 : Boolean; pragma Import (Ada, E053, "aunit_E");
      E055 : Boolean; pragma Import (Ada, E055, "aunit__memory_E");
      E064 : Boolean; pragma Import (Ada, E064, "aunit__memory__utils_E");
      E102 : Boolean; pragma Import (Ada, E102, "aunit__tests_E");
      E097 : Boolean; pragma Import (Ada, E097, "aunit__test_filters_E");
      E066 : Boolean; pragma Import (Ada, E066, "aunit__time_measure_E");
      E059 : Boolean; pragma Import (Ada, E059, "aunit__test_results_E");
      E101 : Boolean; pragma Import (Ada, E101, "aunit__assertions_E");
      E056 : Boolean; pragma Import (Ada, E056, "aunit__reporter_E");
      E074 : Boolean; pragma Import (Ada, E074, "aunit__reporter__text_E");
      E099 : Boolean; pragma Import (Ada, E099, "aunit__simple_test_cases_E");
      E248 : Boolean; pragma Import (Ada, E248, "aunit__test_cases_E");
      E111 : Boolean; pragma Import (Ada, E111, "aunit__test_results__acl_E");
      E086 : Boolean; pragma Import (Ada, E086, "aunit__test_suites_E");
      E084 : Boolean; pragma Import (Ada, E084, "aunit__run_E");
      E212 : Boolean; pragma Import (Ada, E212, "big_number_add_results_E");
      E300 : Boolean; pragma Import (Ada, E300, "big_number_and_results_E");
      E214 : Boolean; pragma Import (Ada, E214, "big_number_constants_E");
      E254 : Boolean; pragma Import (Ada, E254, "big_number_div_results_E");
      E306 : Boolean; pragma Import (Ada, E306, "big_number_exponentiate_results_E");
      E314 : Boolean; pragma Import (Ada, E314, "big_number_mod_results_E");
      E262 : Boolean; pragma Import (Ada, E262, "big_number_mult2_results_E");
      E266 : Boolean; pragma Import (Ada, E266, "big_number_mult_results_E");
      E318 : Boolean; pragma Import (Ada, E318, "big_number_or_results_E");
      E270 : Boolean; pragma Import (Ada, E270, "big_number_sub_results_E");
      E322 : Boolean; pragma Import (Ada, E322, "big_number_xor_results_E");
      E276 : Boolean; pragma Import (Ada, E276, "big_numbers_mod_utils_E");
      E216 : Boolean; pragma Import (Ada, E216, "crypto__asymmetric_E");
      E135 : Boolean; pragma Import (Ada, E135, "crypto__types_E");
      E328 : Boolean; pragma Import (Ada, E328, "crypto__hashfunction_E");
      E133 : Boolean; pragma Import (Ada, E133, "crypto__random_source_E");
      E131 : Boolean; pragma Import (Ada, E131, "crypto__random_E");
      E140 : Boolean; pragma Import (Ada, E140, "crypto__random_source__file_E");
      E330 : Boolean; pragma Import (Ada, E330, "crypto__symmetric__algorithm_E");
      E332 : Boolean; pragma Import (Ada, E332, "crypto__symmetric__algorithm__sha1_E");
      E334 : Boolean; pragma Import (Ada, E334, "crypto__symmetric__algorithm__sha_utils_E");
      E218 : Boolean; pragma Import (Ada, E218, "crypto__types__big_numbers_E");
      E137 : Boolean; pragma Import (Ada, E137, "crypto__types_generic_mod_aux_E");
      E168 : Boolean; pragma Import (Ada, E168, "test__big_number_add_E");
      E274 : Boolean; pragma Import (Ada, E274, "test__big_number_add_mod_utils_E");
      E298 : Boolean; pragma Import (Ada, E298, "test__big_number_and_E");
      E339 : Boolean; pragma Import (Ada, E339, "test__big_number_b_add_E");
      E341 : Boolean; pragma Import (Ada, E341, "test__big_number_b_div_E");
      E343 : Boolean; pragma Import (Ada, E343, "test__big_number_b_mult_E");
      E345 : Boolean; pragma Import (Ada, E345, "test__big_number_b_sub_E");
      E250 : Boolean; pragma Import (Ada, E250, "test__big_number_comp_E");
      E302 : Boolean; pragma Import (Ada, E302, "test__big_number_dec_E");
      E252 : Boolean; pragma Import (Ada, E252, "test__big_number_div_E");
      E347 : Boolean; pragma Import (Ada, E347, "test__big_number_div_mod_utils_E");
      E304 : Boolean; pragma Import (Ada, E304, "test__big_number_exponentiate_E");
      E308 : Boolean; pragma Import (Ada, E308, "test__big_number_inc_E");
      E349 : Boolean; pragma Import (Ada, E349, "test__big_number_inv_mod_utils_E");
      E278 : Boolean; pragma Import (Ada, E278, "test__big_number_iseven_E");
      E280 : Boolean; pragma Import (Ada, E280, "test__big_number_isodd_E");
      E351 : Boolean; pragma Import (Ada, E351, "test__big_number_lprime_mod_utils_E");
      E282 : Boolean; pragma Import (Ada, E282, "test__big_number_lsb_E");
      E310 : Boolean; pragma Import (Ada, E310, "test__big_number_min_max_E");
      E312 : Boolean; pragma Import (Ada, E312, "test__big_number_mod_E");
      E256 : Boolean; pragma Import (Ada, E256, "test__big_number_mod_type_comp_and_add_E");
      E258 : Boolean; pragma Import (Ada, E258, "test__big_number_mod_types_E");
      E353 : Boolean; pragma Import (Ada, E353, "test__big_number_mr_mod_utils_E");
      E284 : Boolean; pragma Import (Ada, E284, "test__big_number_msb_E");
      E260 : Boolean; pragma Import (Ada, E260, "test__big_number_mult2_E");
      E355 : Boolean; pragma Import (Ada, E355, "test__big_number_mult_mod_utils_E");
      E264 : Boolean; pragma Import (Ada, E264, "test__big_number_multiplication_E");
      E316 : Boolean; pragma Import (Ada, E316, "test__big_number_or_E");
      E357 : Boolean; pragma Import (Ada, E357, "test__big_number_pow_mod_utils_E");
      E359 : Boolean; pragma Import (Ada, E359, "test__big_number_prime_mod_utils_E");
      E361 : Boolean; pragma Import (Ada, E361, "test__big_number_rand_mod_utils_E");
      E286 : Boolean; pragma Import (Ada, E286, "test__big_number_rl_E");
      E288 : Boolean; pragma Import (Ada, E288, "test__big_number_sl_E");
      E290 : Boolean; pragma Import (Ada, E290, "test__big_number_sr_E");
      E268 : Boolean; pragma Import (Ada, E268, "test__big_number_sub_E");
      E292 : Boolean; pragma Import (Ada, E292, "test__big_number_sub_mod_utils_E");
      E294 : Boolean; pragma Import (Ada, E294, "test__big_number_swap_E");
      E320 : Boolean; pragma Import (Ada, E320, "test__big_number_xor_E");
      E363 : Boolean; pragma Import (Ada, E363, "test__big_numbers_division_E");
      E324 : Boolean; pragma Import (Ada, E324, "test__big_numbers_utils_E");
      E166 : Boolean; pragma Import (Ada, E166, "test__suite_big_num1_E");
      E272 : Boolean; pragma Import (Ada, E272, "test__suite_big_num2_E");
      E296 : Boolean; pragma Import (Ada, E296, "test__suite_big_num3_E");
      E337 : Boolean; pragma Import (Ada, E337, "test__suite_big_num4_E");
      E164 : Boolean; pragma Import (Ada, E164, "test__suite_big_num_all_E");

      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");
   begin
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, True, 
           False, False, False, False, False, False, False, False, 
           False),
         Value => (0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, True, True, False, False, False, True, 
           True, True, True, True, True, False, False, True, 
           False, False, True, True, False, True, True, True, 
           True, True, True, True, True, True, False, True, 
           False, False, True, False, False, True, False, False, 
           False, True, False, False, True, False, True, False, 
           False, True, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, True, 
           True, False, False, False, True, True, False, False, 
           False),
         Count => (0, 0, 2, 40, 0, 0, 0),
         Unknown => (False, False, False, True, False, False, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      System.Stack_Usage'Elab_Spec;
      System.Fat_Flt'Elab_Spec;
      E138 := True;
      System.Exception_Table'Elab_Body;
      E023 := True;
      Ada.Io_Exceptions'Elab_Spec;
      E082 := True;
      Ada.Strings'Elab_Spec;
      E141 := True;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E057 := True;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E188 := True;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E068 := True;
      E122 := True;
      E120 := True;
      Ada.Tags'Elab_Body;
      E005 := True;
      E147 := True;
      System.Soft_Links'Elab_Body;
      E013 := True;
      E208 := True;
      System.Secondary_Stack'Elab_Body;
      E017 := True;
      System.Finalization_Root'Elab_Spec;
      E072 := True;
      System.Finalization_Implementation'Elab_Spec;
      System.Finalization_Implementation'Elab_Body;
      E092 := True;
      Ada.Finalization'Elab_Spec;
      E090 := True;
      Ada.Strings.Unbounded'Elab_Spec;
      E143 := True;
      Ada.Finalization.List_Controller'Elab_Spec;
      E088 := True;
      System.File_Control_Block'Elab_Spec;
      E128 := True;
      Ada.Streams.Stream_Io'Elab_Spec;
      E159 := True;
      System.Os_Lib'Elab_Body;
      E125 := True;
      System.File_Io'Elab_Body;
      E118 := True;
      System.Strings.Stream_Ops'Elab_Body;
      E157 := True;
      System.Tasking.Initialization'Elab_Body;
      E224 := True;
      System.Tasking.Protected_Objects'Elab_Body;
      E230 := True;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E170 := True;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E113 := True;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E234 := True;
      System.Tasking.Queuing'Elab_Body;
      E238 := True;
      System.Tasking.Stages'Elab_Body;
      E244 := True;
      E055 := True;
      E053 := True;
      E064 := True;
      E062 := True;
      Aunit.Tests'Elab_Spec;
      E102 := True;
      Aunit.Test_Filters'Elab_Spec;
      Aunit.Time_Measure'Elab_Spec;
      E066 := True;
      Aunit.Test_Results'Elab_Spec;
      Aunit.Test_Results'Elab_Body;
      E059 := True;
      Aunit.Assertions'Elab_Spec;
      Aunit.Assertions'Elab_Body;
      E101 := True;
      Aunit.Reporter'Elab_Spec;
      E056 := True;
      Aunit.Reporter.Text'Elab_Spec;
      Aunit.Reporter.Text'Elab_Body;
      E074 := True;
      Aunit.Simple_Test_Cases'Elab_Spec;
      E099 := True;
      E097 := True;
      Aunit.Test_Cases'Elab_Spec;
      E248 := True;
      Aunit.Test_Results.Acl'Elab_Spec;
      E111 := True;
      Aunit.Test_Suites'Elab_Spec;
      E086 := True;
      E084 := True;
      E212 := True;
      E300 := True;
      E214 := True;
      E254 := True;
      E306 := True;
      E314 := True;
      E262 := True;
      E266 := True;
      E318 := True;
      E270 := True;
      E322 := True;
      E276 := True;
      Crypto.Asymmetric'Elab_Spec;
      E216 := True;
      Crypto.Types'Elab_Spec;
      E328 := True;
      Crypto.Random_Source'Elab_Spec;
      E133 := True;
      Crypto.Random_Source.File'Elab_Spec;
      E140 := True;
      Crypto.Symmetric.Algorithm'Elab_Spec;
      E330 := True;
      Crypto.Symmetric.Algorithm.Sha1'Elab_Spec;
      Crypto.Symmetric.Algorithm.Sha_Utils'Elab_Spec;
      E334 := True;
      E332 := True;
      E218 := True;
      E137 := True;
      Crypto.Types'Elab_Body;
      E135 := True;
      Crypto.Random'Elab_Body;
      E131 := True;
      Test.Big_Number_Add'Elab_Spec;
      Test.Big_Number_Add'Elab_Body;
      E168 := True;
      Test.Big_Number_Add_Mod_Utils'Elab_Spec;
      Test.Big_Number_Add_Mod_Utils'Elab_Body;
      E274 := True;
      Test.Big_Number_And'Elab_Spec;
      Test.Big_Number_And'Elab_Body;
      E298 := True;
      Test.Big_Number_B_Add'Elab_Spec;
      Test.Big_Number_B_Add'Elab_Body;
      E339 := True;
      Test.Big_Number_B_Div'Elab_Spec;
      Test.Big_Number_B_Div'Elab_Body;
      E341 := True;
      Test.Big_Number_B_Mult'Elab_Spec;
      Test.Big_Number_B_Mult'Elab_Body;
      E343 := True;
      Test.Big_Number_B_Sub'Elab_Spec;
      Test.Big_Number_B_Sub'Elab_Body;
      E345 := True;
      Test.Big_Number_Comp'Elab_Spec;
      Test.Big_Number_Comp'Elab_Body;
      E250 := True;
      Test.Big_Number_Dec'Elab_Spec;
      Test.Big_Number_Dec'Elab_Body;
      E302 := True;
      Test.Big_Number_Div'Elab_Spec;
      Test.Big_Number_Div'Elab_Body;
      E252 := True;
      Test.Big_Number_Div_Mod_Utils'Elab_Spec;
      Test.Big_Number_Div_Mod_Utils'Elab_Body;
      E347 := True;
      Test.Big_Number_Exponentiate'Elab_Spec;
      Test.Big_Number_Exponentiate'Elab_Body;
      E304 := True;
      Test.Big_Number_Inc'Elab_Spec;
      Test.Big_Number_Inc'Elab_Body;
      E308 := True;
      Test.Big_Number_Inv_Mod_Utils'Elab_Spec;
      Test.Big_Number_Inv_Mod_Utils'Elab_Body;
      E349 := True;
      Test.Big_Number_Iseven'Elab_Spec;
      Test.Big_Number_Iseven'Elab_Body;
      E278 := True;
      Test.Big_Number_Isodd'Elab_Spec;
      Test.Big_Number_Isodd'Elab_Body;
      E280 := True;
      Test.Big_Number_Lprime_Mod_Utils'Elab_Spec;
      Test.Big_Number_Lprime_Mod_Utils'Elab_Body;
      E351 := True;
      Test.Big_Number_Lsb'Elab_Spec;
      Test.Big_Number_Lsb'Elab_Body;
      E282 := True;
      Test.Big_Number_Min_Max'Elab_Spec;
      Test.Big_Number_Min_Max'Elab_Body;
      E310 := True;
      Test.Big_Number_Mod'Elab_Spec;
      Test.Big_Number_Mod'Elab_Body;
      E312 := True;
      Test.Big_Number_Mod_Type_Comp_And_Add'Elab_Spec;
      Test.Big_Number_Mod_Type_Comp_And_Add'Elab_Body;
      E256 := True;
      Test.Big_Number_Mod_Types'Elab_Spec;
      Test.Big_Number_Mod_Types'Elab_Body;
      E258 := True;
      Test.Big_Number_Mr_Mod_Utils'Elab_Spec;
      Test.Big_Number_Mr_Mod_Utils'Elab_Body;
      E353 := True;
      Test.Big_Number_Msb'Elab_Spec;
      Test.Big_Number_Msb'Elab_Body;
      E284 := True;
      Test.Big_Number_Mult2'Elab_Spec;
      Test.Big_Number_Mult2'Elab_Body;
      E260 := True;
      Test.Big_Number_Mult_Mod_Utils'Elab_Spec;
      Test.Big_Number_Mult_Mod_Utils'Elab_Body;
      E355 := True;
      Test.Big_Number_Multiplication'Elab_Spec;
      Test.Big_Number_Multiplication'Elab_Body;
      E264 := True;
      Test.Big_Number_Or'Elab_Spec;
      Test.Big_Number_Or'Elab_Body;
      E316 := True;
      Test.Big_Number_Pow_Mod_Utils'Elab_Spec;
      Test.Big_Number_Pow_Mod_Utils'Elab_Body;
      E357 := True;
      Test.Big_Number_Prime_Mod_Utils'Elab_Spec;
      Test.Big_Number_Prime_Mod_Utils'Elab_Body;
      E359 := True;
      Test.Big_Number_Rand_Mod_Utils'Elab_Spec;
      Test.Big_Number_Rand_Mod_Utils'Elab_Body;
      E361 := True;
      Test.Big_Number_Rl'Elab_Spec;
      Test.Big_Number_Rl'Elab_Body;
      E286 := True;
      Test.Big_Number_Sl'Elab_Spec;
      Test.Big_Number_Sl'Elab_Body;
      E288 := True;
      Test.Big_Number_Sr'Elab_Spec;
      Test.Big_Number_Sr'Elab_Body;
      E290 := True;
      Test.Big_Number_Sub'Elab_Spec;
      Test.Big_Number_Sub'Elab_Body;
      E268 := True;
      Test.Big_Number_Sub_Mod_Utils'Elab_Spec;
      Test.Big_Number_Sub_Mod_Utils'Elab_Body;
      E292 := True;
      Test.Big_Number_Swap'Elab_Spec;
      Test.Big_Number_Swap'Elab_Body;
      E294 := True;
      Test.Big_Number_Xor'Elab_Spec;
      Test.Big_Number_Xor'Elab_Body;
      E320 := True;
      Test.Big_Numbers_Division'Elab_Spec;
      Test.Big_Numbers_Division'Elab_Body;
      E363 := True;
      Test.Big_Numbers_Utils'Elab_Spec;
      Test.Big_Numbers_Utils'Elab_Body;
      E324 := True;
      Test.Suite_Big_Num1'Elab_Body;
      E166 := True;
      Test.Suite_Big_Num2'Elab_Body;
      E272 := True;
      Test.Suite_Big_Num3'Elab_Body;
      E296 := True;
      Test.Suite_Big_Num4'Elab_Body;
      E337 := True;
      E164 := True;
   end adainit;

   procedure adafinal is
   begin
      Do_Finalize;
   end adafinal;

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure initialize (Addr : System.Address);
      pragma Import (C, initialize, "__gnat_initialize");

      procedure finalize;
      pragma Import (C, finalize, "__gnat_finalize");

      procedure Ada_Main_Program;
      pragma Import (Ada, Ada_Main_Program, "_ada_test__big_number");

      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Break_Start;
      Ada_Main_Program;
      Do_Finalize;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   aunit-3.4/src/ada_containers.o
   --   aunit-3.4/src/aunit-memory.o
   --   aunit-3.4/src/aunit.o
   --   aunit-3.4/src/aunit-memory-utils.o
   --   aunit-3.4/src/ada_containers-aunit_lists.o
   --   aunit-3.4/src/aunit-tests.o
   --   aunit-3.4/src/aunit-options.o
   --   aunit-3.4/src/aunit-time_measure.o
   --   aunit-3.4/src/aunit-test_results.o
   --   aunit-3.4/src/aunit-assertions.o
   --   aunit-3.4/src/aunit-reporter.o
   --   aunit-3.4/src/aunit-reporter-text.o
   --   aunit-3.4/src/aunit-simple_test_cases.o
   --   aunit-3.4/src/aunit-test_filters.o
   --   aunit-3.4/src/aunit-test_cases.o
   --   ./aunit-test_results-acl.o
   --   aunit-3.4/src/aunit-test_suites.o
   --   aunit-3.4/src/aunit-run.o
   --   ./big_number_add_results.o
   --   ./big_number_and_results.o
   --   ./big_number_constants.o
   --   ./big_number_div_results.o
   --   ./big_number_exponentiate_results.o
   --   ./big_number_mod_results.o
   --   ./big_number_mult2_results.o
   --   ./big_number_mult_results.o
   --   ./big_number_or_results.o
   --   ./big_number_sub_results.o
   --   ./big_number_xor_results.o
   --   ./big_numbers_mod_utils.o
   --   ../src/crypto.o
   --   ../src/crypto-asymmetric.o
   --   ../src/crypto-symmetric.o
   --   ../src/crypto-asymmetric-prime_tables.o
   --   ../src/crypto-hashfunction.o
   --   ../src/crypto-random_source.o
   --   ../src/crypto-random_source-file.o
   --   ../src/crypto-symmetric-algorithm.o
   --   ../src/crypto-symmetric-algorithm-sha_utils.o
   --   ../src/crypto-symmetric-algorithm-sha1.o
   --   ../src/crypto-types-big_numbers.o
   --   ../src/crypto-types_generic_mod_aux.o
   --   ../src/crypto-types.o
   --   ../src/crypto-random.o
   --   ../src/crypto-hashfunction_sha1.o
   --   ./test.o
   --   ./test-big_number_add.o
   --   ./test-big_number_add_mod_utils.o
   --   ./test-big_number_and.o
   --   ./test-big_number_b_add.o
   --   ./test-big_number_b_div.o
   --   ./test-big_number_b_mult.o
   --   ./test-big_number_b_sub.o
   --   ./test-big_number_comp.o
   --   ./test-big_number_dec.o
   --   ./test-big_number_div.o
   --   ./test-big_number_div_mod_utils.o
   --   ./test-big_number_exponentiate.o
   --   ./test-big_number_inc.o
   --   ./test-big_number_inv_mod_utils.o
   --   ./test-big_number_iseven.o
   --   ./test-big_number_isodd.o
   --   ./test-big_number_lprime_mod_utils.o
   --   ./test-big_number_lsb.o
   --   ./test-big_number_min_max.o
   --   ./test-big_number_mod.o
   --   ./test-big_number_mod_type_comp_and_add.o
   --   ./test-big_number_mod_types.o
   --   ./test-big_number_mr_mod_utils.o
   --   ./test-big_number_msb.o
   --   ./test-big_number_mult2.o
   --   ./test-big_number_mult_mod_utils.o
   --   ./test-big_number_multiplication.o
   --   ./test-big_number_or.o
   --   ./test-big_number_pow_mod_utils.o
   --   ./test-big_number_prime_mod_utils.o
   --   ./test-big_number_rand_mod_utils.o
   --   ./test-big_number_rl.o
   --   ./test-big_number_sl.o
   --   ./test-big_number_sr.o
   --   ./test-big_number_sub.o
   --   ./test-big_number_sub_mod_utils.o
   --   ./test-big_number_swap.o
   --   ./test-big_number_xor.o
   --   ./test-big_numbers_division.o
   --   ./test-big_numbers_utils.o
   --   ./test-suite_big_num1.o
   --   ./test-suite_big_num2.o
   --   ./test-suite_big_num3.o
   --   ./test-suite_big_num4.o
   --   ./test-suite_big_num_all.o
   --   ./test-big_number.o
   --   -L./
   --   -L/lib/
   --   -Launit-3.4/src/
   --   -L../src/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.6/adalib/
   --   -shared
   --   -lgnarl-4.6
   --   -lgnat-4.6
   --   -lpthread
--  END Object file/option list   

end ada_main;
