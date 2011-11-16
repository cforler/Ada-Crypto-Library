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
      E159 : Boolean; pragma Import (Ada, E159, "system__stack_usage_E");
      E018 : Boolean; pragma Import (Ada, E018, "system__soft_links_E");
      E198 : Boolean; pragma Import (Ada, E198, "system__fat_flt_E");
      E026 : Boolean; pragma Import (Ada, E026, "system__exception_table_E");
      E082 : Boolean; pragma Import (Ada, E082, "ada__io_exceptions_E");
      E162 : Boolean; pragma Import (Ada, E162, "ada__strings_E");
      E168 : Boolean; pragma Import (Ada, E168, "ada__strings__maps_E");
      E051 : Boolean; pragma Import (Ada, E051, "ada__tags_E");
      E049 : Boolean; pragma Import (Ada, E049, "ada__streams_E");
      E125 : Boolean; pragma Import (Ada, E125, "interfaces__c_E");
      E182 : Boolean; pragma Import (Ada, E182, "interfaces__c__strings_E");
      E139 : Boolean; pragma Import (Ada, E139, "system__task_info_E");
      E068 : Boolean; pragma Import (Ada, E068, "ada__calendar_E");
      E022 : Boolean; pragma Import (Ada, E022, "system__secondary_stack_E");
      E072 : Boolean; pragma Import (Ada, E072, "system__finalization_root_E");
      E092 : Boolean; pragma Import (Ada, E092, "system__finalization_implementation_E");
      E090 : Boolean; pragma Import (Ada, E090, "ada__finalization_E");
      E164 : Boolean; pragma Import (Ada, E164, "ada__strings__unbounded_E");
      E088 : Boolean; pragma Import (Ada, E088, "ada__finalization__list_controller_E");
      E188 : Boolean; pragma Import (Ada, E188, "system__file_control_block_E");
      E180 : Boolean; pragma Import (Ada, E180, "system__file_io_E");
      E185 : Boolean; pragma Import (Ada, E185, "system__os_lib_E");
      E208 : Boolean; pragma Import (Ada, E208, "system__tasking__initialization_E");
      E214 : Boolean; pragma Import (Ada, E214, "system__tasking__protected_objects_E");
      E116 : Boolean; pragma Import (Ada, E116, "ada__real_time_E");
      E176 : Boolean; pragma Import (Ada, E176, "ada__text_io_E");
      E218 : Boolean; pragma Import (Ada, E218, "system__tasking__protected_objects__entries_E");
      E222 : Boolean; pragma Import (Ada, E222, "system__tasking__queuing_E");
      E228 : Boolean; pragma Import (Ada, E228, "system__tasking__stages_E");
      E062 : Boolean; pragma Import (Ada, E062, "ada_containers__aunit_lists_E");
      E005 : Boolean; pragma Import (Ada, E005, "aunit_E");
      E008 : Boolean; pragma Import (Ada, E008, "aunit__memory_E");
      E064 : Boolean; pragma Import (Ada, E064, "aunit__memory__utils_E");
      E102 : Boolean; pragma Import (Ada, E102, "aunit__tests_E");
      E097 : Boolean; pragma Import (Ada, E097, "aunit__test_filters_E");
      E066 : Boolean; pragma Import (Ada, E066, "aunit__time_measure_E");
      E059 : Boolean; pragma Import (Ada, E059, "aunit__test_results_E");
      E101 : Boolean; pragma Import (Ada, E101, "aunit__assertions_E");
      E012 : Boolean; pragma Import (Ada, E012, "aunit__reporter_E");
      E074 : Boolean; pragma Import (Ada, E074, "aunit__reporter__text_E");
      E099 : Boolean; pragma Import (Ada, E099, "aunit__simple_test_cases_E");
      E232 : Boolean; pragma Import (Ada, E232, "aunit__test_cases_E");
      E086 : Boolean; pragma Import (Ada, E086, "aunit__test_suites_E");
      E084 : Boolean; pragma Import (Ada, E084, "aunit__run_E");
      E190 : Boolean; pragma Import (Ada, E190, "big_number_add_results_E");
      E240 : Boolean; pragma Import (Ada, E240, "big_number_and_results_E");
      E192 : Boolean; pragma Import (Ada, E192, "big_number_constants_E");
      E256 : Boolean; pragma Import (Ada, E256, "big_number_div_results_E");
      E262 : Boolean; pragma Import (Ada, E262, "big_number_exponentiate_results_E");
      E280 : Boolean; pragma Import (Ada, E280, "big_number_mod_results_E");
      E292 : Boolean; pragma Import (Ada, E292, "big_number_mult2_results_E");
      E298 : Boolean; pragma Import (Ada, E298, "big_number_mult_results_E");
      E302 : Boolean; pragma Import (Ada, E302, "big_number_or_results_E");
      E318 : Boolean; pragma Import (Ada, E318, "big_number_sub_results_E");
      E326 : Boolean; pragma Import (Ada, E326, "big_number_xor_results_E");
      E236 : Boolean; pragma Import (Ada, E236, "big_numbers_mod_utils_E");
      E195 : Boolean; pragma Import (Ada, E195, "crypto__asymmetric_E");
      E197 : Boolean; pragma Import (Ada, E197, "crypto__types_E");
      E200 : Boolean; pragma Import (Ada, E200, "crypto__random_E");
      E202 : Boolean; pragma Import (Ada, E202, "crypto__types__big_numbers_E");
      E114 : Boolean; pragma Import (Ada, E114, "test__big_number_add_E");
      E234 : Boolean; pragma Import (Ada, E234, "test__big_number_add_mod_utils_E");
      E238 : Boolean; pragma Import (Ada, E238, "test__big_number_and_E");
      E242 : Boolean; pragma Import (Ada, E242, "test__big_number_b_add_E");
      E244 : Boolean; pragma Import (Ada, E244, "test__big_number_b_div_E");
      E246 : Boolean; pragma Import (Ada, E246, "test__big_number_b_mult_E");
      E248 : Boolean; pragma Import (Ada, E248, "test__big_number_b_sub_E");
      E250 : Boolean; pragma Import (Ada, E250, "test__big_number_comp_E");
      E252 : Boolean; pragma Import (Ada, E252, "test__big_number_dec_E");
      E254 : Boolean; pragma Import (Ada, E254, "test__big_number_div_E");
      E258 : Boolean; pragma Import (Ada, E258, "test__big_number_div_mod_utils_E");
      E260 : Boolean; pragma Import (Ada, E260, "test__big_number_exponentiate_E");
      E264 : Boolean; pragma Import (Ada, E264, "test__big_number_inc_E");
      E266 : Boolean; pragma Import (Ada, E266, "test__big_number_inv_mod_utils_E");
      E268 : Boolean; pragma Import (Ada, E268, "test__big_number_iseven_E");
      E270 : Boolean; pragma Import (Ada, E270, "test__big_number_isodd_E");
      E272 : Boolean; pragma Import (Ada, E272, "test__big_number_lprime_mod_utils_E");
      E274 : Boolean; pragma Import (Ada, E274, "test__big_number_lsb_E");
      E276 : Boolean; pragma Import (Ada, E276, "test__big_number_min_max_E");
      E278 : Boolean; pragma Import (Ada, E278, "test__big_number_mod_E");
      E282 : Boolean; pragma Import (Ada, E282, "test__big_number_mod_type_comp_and_add_E");
      E284 : Boolean; pragma Import (Ada, E284, "test__big_number_mod_types_E");
      E286 : Boolean; pragma Import (Ada, E286, "test__big_number_mr_mod_utils_E");
      E288 : Boolean; pragma Import (Ada, E288, "test__big_number_msb_E");
      E290 : Boolean; pragma Import (Ada, E290, "test__big_number_mult2_E");
      E294 : Boolean; pragma Import (Ada, E294, "test__big_number_mult_mod_utils_E");
      E296 : Boolean; pragma Import (Ada, E296, "test__big_number_multiplication_E");
      E300 : Boolean; pragma Import (Ada, E300, "test__big_number_or_E");
      E304 : Boolean; pragma Import (Ada, E304, "test__big_number_pow_mod_utils_E");
      E306 : Boolean; pragma Import (Ada, E306, "test__big_number_prime_mod_utils_E");
      E308 : Boolean; pragma Import (Ada, E308, "test__big_number_rand_mod_utils_E");
      E310 : Boolean; pragma Import (Ada, E310, "test__big_number_rl_E");
      E312 : Boolean; pragma Import (Ada, E312, "test__big_number_sl_E");
      E314 : Boolean; pragma Import (Ada, E314, "test__big_number_sr_E");
      E316 : Boolean; pragma Import (Ada, E316, "test__big_number_sub_E");
      E320 : Boolean; pragma Import (Ada, E320, "test__big_number_sub_mod_utils_E");
      E322 : Boolean; pragma Import (Ada, E322, "test__big_number_swap_E");
      E324 : Boolean; pragma Import (Ada, E324, "test__big_number_xor_E");
      E328 : Boolean; pragma Import (Ada, E328, "test__big_numbers_utils_E");
      E112 : Boolean; pragma Import (Ada, E112, "test__suite_big_num_E");

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
           False, True, False, False, True, False, False, False, 
           False, True, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, True, 
           True, False, False, False, True, True, False, False, 
           False),
         Count => (0, 0, 2, 39, 0, 0, 0),
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
      E198 := True;
      System.Exception_Table'Elab_Body;
      E026 := True;
      Ada.Io_Exceptions'Elab_Spec;
      E082 := True;
      Ada.Strings'Elab_Spec;
      E162 := True;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E049 := True;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E139 := True;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E068 := True;
      E182 := True;
      E125 := True;
      Ada.Tags'Elab_Body;
      E051 := True;
      E168 := True;
      System.Soft_Links'Elab_Body;
      E018 := True;
      E159 := True;
      System.Secondary_Stack'Elab_Body;
      E022 := True;
      System.Finalization_Root'Elab_Spec;
      E072 := True;
      System.Finalization_Implementation'Elab_Spec;
      System.Finalization_Implementation'Elab_Body;
      E092 := True;
      Ada.Finalization'Elab_Spec;
      E090 := True;
      Ada.Strings.Unbounded'Elab_Spec;
      E164 := True;
      Ada.Finalization.List_Controller'Elab_Spec;
      E088 := True;
      System.File_Control_Block'Elab_Spec;
      E188 := True;
      System.Os_Lib'Elab_Body;
      E185 := True;
      System.File_Io'Elab_Body;
      E180 := True;
      System.Tasking.Initialization'Elab_Body;
      E208 := True;
      System.Tasking.Protected_Objects'Elab_Body;
      E214 := True;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E116 := True;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E176 := True;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E218 := True;
      System.Tasking.Queuing'Elab_Body;
      E222 := True;
      System.Tasking.Stages'Elab_Body;
      E228 := True;
      E008 := True;
      E005 := True;
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
      E012 := True;
      Aunit.Reporter.Text'Elab_Spec;
      Aunit.Reporter.Text'Elab_Body;
      E074 := True;
      Aunit.Simple_Test_Cases'Elab_Spec;
      E099 := True;
      E097 := True;
      Aunit.Test_Cases'Elab_Spec;
      E232 := True;
      Aunit.Test_Suites'Elab_Spec;
      E086 := True;
      E084 := True;
      E190 := True;
      E240 := True;
      E192 := True;
      E256 := True;
      E262 := True;
      E280 := True;
      E292 := True;
      E298 := True;
      E302 := True;
      E318 := True;
      E326 := True;
      E236 := True;
      Crypto.Asymmetric'Elab_Spec;
      E195 := True;
      Crypto.Types'Elab_Spec;
      E197 := True;
      Crypto.Random'Elab_Spec;
      Crypto.Random'Elab_Body;
      E200 := True;
      E202 := True;
      Test.Big_Number_Add'Elab_Spec;
      Test.Big_Number_Add'Elab_Body;
      E114 := True;
      Test.Big_Number_Add_Mod_Utils'Elab_Spec;
      Test.Big_Number_Add_Mod_Utils'Elab_Body;
      E234 := True;
      Test.Big_Number_And'Elab_Spec;
      Test.Big_Number_And'Elab_Body;
      E238 := True;
      Test.Big_Number_B_Add'Elab_Spec;
      Test.Big_Number_B_Add'Elab_Body;
      E242 := True;
      Test.Big_Number_B_Div'Elab_Spec;
      Test.Big_Number_B_Div'Elab_Body;
      E244 := True;
      Test.Big_Number_B_Mult'Elab_Spec;
      Test.Big_Number_B_Mult'Elab_Body;
      E246 := True;
      Test.Big_Number_B_Sub'Elab_Spec;
      Test.Big_Number_B_Sub'Elab_Body;
      E248 := True;
      Test.Big_Number_Comp'Elab_Spec;
      Test.Big_Number_Comp'Elab_Body;
      E250 := True;
      Test.Big_Number_Dec'Elab_Spec;
      Test.Big_Number_Dec'Elab_Body;
      E252 := True;
      Test.Big_Number_Div'Elab_Spec;
      Test.Big_Number_Div'Elab_Body;
      E254 := True;
      Test.Big_Number_Div_Mod_Utils'Elab_Spec;
      Test.Big_Number_Div_Mod_Utils'Elab_Body;
      E258 := True;
      Test.Big_Number_Exponentiate'Elab_Spec;
      Test.Big_Number_Exponentiate'Elab_Body;
      E260 := True;
      Test.Big_Number_Inc'Elab_Spec;
      Test.Big_Number_Inc'Elab_Body;
      E264 := True;
      Test.Big_Number_Inv_Mod_Utils'Elab_Spec;
      Test.Big_Number_Inv_Mod_Utils'Elab_Body;
      E266 := True;
      Test.Big_Number_Iseven'Elab_Spec;
      Test.Big_Number_Iseven'Elab_Body;
      E268 := True;
      Test.Big_Number_Isodd'Elab_Spec;
      Test.Big_Number_Isodd'Elab_Body;
      E270 := True;
      Test.Big_Number_Lprime_Mod_Utils'Elab_Spec;
      Test.Big_Number_Lprime_Mod_Utils'Elab_Body;
      E272 := True;
      Test.Big_Number_Lsb'Elab_Spec;
      Test.Big_Number_Lsb'Elab_Body;
      E274 := True;
      Test.Big_Number_Min_Max'Elab_Spec;
      Test.Big_Number_Min_Max'Elab_Body;
      E276 := True;
      Test.Big_Number_Mod'Elab_Spec;
      Test.Big_Number_Mod'Elab_Body;
      E278 := True;
      Test.Big_Number_Mod_Type_Comp_And_Add'Elab_Spec;
      Test.Big_Number_Mod_Type_Comp_And_Add'Elab_Body;
      E282 := True;
      Test.Big_Number_Mod_Types'Elab_Spec;
      Test.Big_Number_Mod_Types'Elab_Body;
      E284 := True;
      Test.Big_Number_Mr_Mod_Utils'Elab_Spec;
      Test.Big_Number_Mr_Mod_Utils'Elab_Body;
      E286 := True;
      Test.Big_Number_Msb'Elab_Spec;
      Test.Big_Number_Msb'Elab_Body;
      E288 := True;
      Test.Big_Number_Mult2'Elab_Spec;
      Test.Big_Number_Mult2'Elab_Body;
      E290 := True;
      Test.Big_Number_Mult_Mod_Utils'Elab_Spec;
      Test.Big_Number_Mult_Mod_Utils'Elab_Body;
      E294 := True;
      Test.Big_Number_Multiplication'Elab_Spec;
      Test.Big_Number_Multiplication'Elab_Body;
      E296 := True;
      Test.Big_Number_Or'Elab_Spec;
      Test.Big_Number_Or'Elab_Body;
      E300 := True;
      Test.Big_Number_Pow_Mod_Utils'Elab_Spec;
      Test.Big_Number_Pow_Mod_Utils'Elab_Body;
      E304 := True;
      Test.Big_Number_Prime_Mod_Utils'Elab_Spec;
      Test.Big_Number_Prime_Mod_Utils'Elab_Body;
      E306 := True;
      Test.Big_Number_Rand_Mod_Utils'Elab_Spec;
      Test.Big_Number_Rand_Mod_Utils'Elab_Body;
      E308 := True;
      Test.Big_Number_Rl'Elab_Spec;
      Test.Big_Number_Rl'Elab_Body;
      E310 := True;
      Test.Big_Number_Sl'Elab_Spec;
      Test.Big_Number_Sl'Elab_Body;
      E312 := True;
      Test.Big_Number_Sr'Elab_Spec;
      Test.Big_Number_Sr'Elab_Body;
      E314 := True;
      Test.Big_Number_Sub'Elab_Spec;
      Test.Big_Number_Sub'Elab_Body;
      E316 := True;
      Test.Big_Number_Sub_Mod_Utils'Elab_Spec;
      Test.Big_Number_Sub_Mod_Utils'Elab_Body;
      E320 := True;
      Test.Big_Number_Swap'Elab_Spec;
      Test.Big_Number_Swap'Elab_Body;
      E322 := True;
      Test.Big_Number_Xor'Elab_Spec;
      Test.Big_Number_Xor'Elab_Body;
      E324 := True;
      Test.Big_Numbers_Utils'Elab_Spec;
      Test.Big_Numbers_Utils'Elab_Body;
      E328 := True;
      Test.Suite_Big_Num'Elab_Body;
      E112 := True;
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
   --   ../src/crypto-types.o
   --   ../src/crypto-asymmetric-prime_tables.o
   --   ../src/crypto-random.o
   --   ../src/crypto-types-big_numbers.o
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
   --   ./test-big_numbers_utils.o
   --   ./test-suite_big_num.o
   --   ./test-big_number.o
   --   -L./
   --   -L/lib/
   --   -Launit-3.4/src/
   --   -L../src/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.6.1/adalib/
   --   -shared
   --   -lgnarl-4.6
   --   -lgnat-4.6
   --   -lpthread
--  END Object file/option list   

end ada_main;
