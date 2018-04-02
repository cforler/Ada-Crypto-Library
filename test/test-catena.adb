with AUnit.Assertions; use AUnit.Assertions;
with Crypto.Types;
with Crypto.Symmetric.KDF_Catena.Testing; use Crypto.Symmetric.KDF_Catena.Testing; 
with Crypto.Symmetric.KDF_Catena; use Crypto.Symmetric.KDF_Catena; 

with Crypto.Symmetric.Hashfunction_SHA512;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Crypto.Symmetric.KDF;
with Crypto.Types.XORShiftSTAR;
with Crypto.Symmetric.Algorithm.PCompressBla;
with Crypto.Symmetric.Algorithm.Blake2b; use Crypto.Symmetric.Algorithm.Blake2b;

with Crypto.Symmetric.Hashfunction_Blake2b;
with Crypto.Types.Output; use Crypto.types.Output;
with Crypto.Symmetric.Algorithm.Blake2b_Utils; use Crypto.Symmetric.Algorithm.Blake2b_Utils;
with Ada.Calendar; use Ada.Calendar;

with Crypto.Symmetric.Compress_CFAES; use Crypto.Symmetric.Compress_CFAES;
with Crypto.Symmetric.Compress_SHA512; use Crypto.Symmetric.Compress_SHA512;
with Crypto.Symmetric.Compress_Blake2b1; use Crypto.Symmetric.Compress_Blake2b1;
with Crypto.Symmetric.Compress_Blake2b1Bla; use Crypto.Symmetric.Compress_Blake2b1Bla;
with Crypto.Symmetric.Compress_ArgonCF; use Crypto.Symmetric.Compress_ArgonCF;
with Crypto.Symmetric.Compress_ArgonCFBla; use Crypto.Symmetric.Compress_ArgonCFBla;
with Crypto.Symmetric.Compress_PCompressG; use Crypto.Symmetric.Compress_PCompressG;
with Crypto.Symmetric.Compress_PCompressBla; use Crypto.Symmetric.Compress_PCompressBla;
with Crypto.Symmetric.Compress_Blake2b; use Crypto.Symmetric.Compress_Blake2b;
  
with System; use System;

package body Test.Catena is
   use Crypto.Types;


   -- use Catena.Testing;

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   -------------------------------- Type - Declaration -------------------------
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Register_Tests(T : in out Catena_Test) is
      use Test_Cases.Registration;
   begin

      Register_Routine(T, Catena_Test_CFAES'Access,"CFAES Test");
      Register_Routine(T, Catena_Test_Interface'Access,"Interface Test");
      Register_Routine(T, Catena_Test_SHA512'Access,"SHA512 Test");
      Register_Routine(T, Catena_Test_Pcompress'Access,"P Compress Test");
      Register_Routine(T, Catena_Test_ArgonCF'Access,"Argon CF Test");
      Register_Routine(T, Catena_Test_B2b1'Access,"B2b1 Test");
      Register_Routine(T, Catena_Test_B2b1Bla'Access,"B2b1Bla Test");
      Register_Routine(T, Catena_Test_GR'Access,"Gray Reverse Test");
      Register_Routine(T, Catena_Test_Bitreverse'Access,"Bitreverse for Catena");
      Register_Routine(T, Catena_Test_XORShift'Access,"XS Function for Catena");
      Register_Routine(T, Catena_Test_Keyed'Access,"Keyed Hashing Test");

   end Register_Tests;
   

   function Name(T : Catena_Test) return Test_String is
   begin
      return new String'("Catena Test");
   end Name;


   procedure Catena_Test_XORShift(T: in out Test_Cases.Test_Case'Class) is
      -- package XS renames Crypto.Types.XORShiftSTAR;
      package XS is new Crypto.Types.XORShiftSTAR(Crypto.Symmetric.Hashfunction_SHA512);
      -- test for more values!!
      input : Bytes(0..63) := (others => 0);
      g : Integer :=5;
      function Cast is new Ada.Unchecked_Conversion (DWord, Integer);
      type IntArray is array(0..9) of Integer;
      target : IntArray := (21, 28, 15, 31, 3, 8, 24, 2, 16, 22);
      result : IntArray;
   begin
      -- Put_Line("XS-Test");
      XS.init(input);
      for i in Natural range 0..9 loop
         result(i) := XS.getValue(g);
      end loop;
      -- Put_Line(" ");
      Assert(result = target, "Range Exception failed");
   end Catena_Test_XORShift;


   procedure Catena_Test_Bitreverse(T: in out Test_Cases.Test_Case'Class) is
      n  : Natural := 16;

      s : Natural := 31;
      function toInteger is new Ada.Unchecked_Conversion (Dword, Integer);
      function toByte is new Ada.Unchecked_Conversion (Integer, Dword);
   begin
      n := bitreverse_testing(n, 5);
      Assert(n = 1, "Bitreverse failed");

      s := bitreverse_testing(s, 5);
      Assert( s = 31, "Bitreverse failed");

   end Catena_Test_Bitreverse;


   procedure Catena_Test_Keyed(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;
      password : String := "Password1";
      data    : Bytes(0..12) := To_Bytes("I'm a header1");
      salt     : Bytes(0..15) := (16#00#, 16#11#,16#22#, 16#33#, 16#44#, 16#55#,16#66#, 16#77#, 16#88#, 16#99#,16#AA#, 16#BB#, 16#CC#, 16#DD#,16#EE#, 16#FF#);
      glow     : Natural := 2;
      ghigh    : Natural := 21;
      hashlen  : Natural := 64;
      hash     : Bytes(0..hashlen-1);
      tweak_id : Natural := 0;

      target_1   : Bytes(1..16):=(16#99#,16#7c#,16#71#,16#9b#,16#a1#,16#c8#,16#60#,16#6e#,16#d8#,16#e5#,16#e4#,16#94#,16#03#,16#16#,16#87#, 16#fd#);
      
      Start_Time : Time;
      Finis_Time : Time;
      
      number : Integer := 7;

      reverseTest : Dword := 16#0011223344556677#;

      Scheme : Catena_KDF;
      
      Key : Bytes(0..15) := (others=>1);
      uuid : Dword := 0;

   begin
      Scheme.Initialize(hashlen, 
                        tweak_id, 
                        glow, 
                        ghigh, 
                        "rgggg", 
                        Dragonfly, 
                        Dummy, 
                        Gamma, 
                        new Blake2b1_Scheme, 
                        Blake2b);
      
      Start_Time := Clock;
      -- Scheme.Derive(password, data, salt, hash);
      Scheme.Derive_Keyed(password, data, salt, key, to_Bytes(uuid), hash);
      Finis_Time := Clock;
      Put_Line(Duration'Image(Finis_Time-Start_Time));

      Assert(Hash(0..15) = target_1, "Keyed Hashing failed");

   end Catena_Test_Keyed;


   procedure Catena_Test_CFAES(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;
      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#dc#,16#d7#,16#79#,16#ee#,16#91#,16#5e#,16#ba#,16#c3#,16#9f#,16#67#,16#61#,16#be#,16#ee#,16#0f#,16#9f#,16#23#);
      target_butterfly   : Bytes(1..16):=(16#17#,16#72#,16#8a#,16#d9#,16#13#,16#76#,16#c2#,16#87#,16#ce#,16#d5#,16#db#,16#09#,16#26#,16#51#,16#f2#,16#d8#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#d6#,16#d9#,16#06#,16#92#,16#c8#,16#57#,16#50#);


      Scheme : Catena_KDF;

   begin
      
      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new CFAES_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "CFAES-BRG failed");

      ----------------------------------------------------

      Scheme.Initialize(hashlen, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            Butterfly, 
            LSB, 
            Gamma, 
            new CFAES_Scheme, 
            Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "CFAES-DBG failed");
    
      ----------------------------------------------------
       
      Scheme.Initialize(hashlen_low, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            Butterfly, 
            Dummy, 
            Gamma, 
            new CFAES_Scheme, 
            Blake2b);

      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "CFAES-DBG (low HL) failed");

   end Catena_Test_CFAES;


   procedure Catena_Test_Interface(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;
      
      Key    : Bytes(0..2) := To_Bytes("key");
      UID    : Bytes(0..4) := To_Bytes("12345");

      hash     : Bytes(0..hashlen-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#6F#,16#63#,16#8F#,16#F9#,16#0B#,16#DE#,16#B8#,16#2F#,16#CB#,16#99#,16#26#,16#DA#,16#A9#,16#51#,16#E2#,16#60#);
      target_butterfly   : Bytes(1..16):=(16#61#,16#40#,16#CD#,16#05#,16#10#,16#D7#,16#8B#,16#50#,16#25#,16#BC#,16#11#,16#8D#,16#15#,16#3F#,16#5B#,16#55#);


      Scheme : Catena_KDF;

   begin
      
      Scheme.Initialize(hashlen);

      Scheme.Derive(salt, To_Bytes(password), hash);

      Assert(hash=target_dragonfly, "Interface Test 1 failed");
      ----------------------------------------------------

      Scheme.Derive_Keyed(password, data, salt, key, uid, hash);
      Assert(hash=target_butterfly, "Interface Test 2 failed");
    
   end Catena_Test_Interface;

   procedure Catena_Test_SHA512(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#72#,16#1f#,16#6b#,16#48#,16#ce#,16#38#,16#53#,16#1b#,16#be#,16#a8#,16#4f#,16#de#,16#53#,16#69#,16#85#,16#3b#);
      target_butterfly   : Bytes(1..16):=(16#d2#,16#5c#,16#fa#,16#c8#,16#26#,16#fb#,16#83#,16#5d#,16#5d#,16#22#,16#96#,16#29#,16#75#,16#e9#,16#26#,16#f7#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#41#,16#88#,16#7f#,16#40#,16#3a#,16#26#,16#d3#);


      Scheme : Catena_KDF;

   begin
      
      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new SHA512_Scheme, 
                  SHA512);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "SHA512-BRG failed");

      ----------------------------------------------------

      Scheme.Initialize(hashlen, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            Butterfly, 
            LSB, 
            Gamma, 
            new SHA512_Scheme, 
            SHA512);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "SHA512-DBG failed");
    
      ----------------------------------------------------
       
      Scheme.Initialize(hashlen_low, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            Butterfly, 
            Dummy, 
            Gamma, 
            new SHA512_Scheme, 
            SHA512);

      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "SHA512-DBG (low HL) failed");

   end Catena_Test_SHA512;

   procedure Catena_Test_B2b1(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#a5#,16#a7#,16#8a#,16#26#,16#3f#,16#20#,16#23#,16#f0#,16#83#,16#2a#,16#2b#,16#bf#,16#91#,16#3d#,16#b5#,16#ca#);
      target_butterfly   : Bytes(1..16):=(16#bd#,16#8e#,16#80#,16#c9#,16#01#,16#22#,16#a4#,16#1a#,16#e2#,16#87#,16#cc#,16#c1#,16#57#,16#26#,16#a7#,16#c4#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#27#,16#b4#,16#ed#,16#59#,16#af#,16#55#,16#d3#);
      
      Scheme : Catena_KDF;
   begin

       Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new Blake2b1_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);


      Assert(hash=target_dragonfly, "B1b2-BRG failed");

      ----------------------------------------------------

     Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new Blake2b1_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "B1b2-DBG failed");
    
      ----------------------------------------------------

     Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new Blake2b1_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash_low);


      Assert(hash_low=target_butterfly_low_hl, "B1b2-DBG (low HL) failed");

   end Catena_Test_B2b1;

   procedure Catena_Test_B2b1Bla(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#48#,16#91#,16#b1#,16#b1#,16#0f#,16#f1#,16#ce#,16#d7#,16#50#,16#c3#,16#b1#,16#9e#,16#24#,16#99#,16#72#,16#a6#);
      target_butterfly   : Bytes(1..16):=(16#fc#,16#36#,16#88#,16#c0#,16#1a#,16#2f#,16#76#,16#5c#,16#67#,16#ad#,16#0e#,16#36#,16#fb#,16#c1#,16#16#,16#98#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#d2#,16#ed#,16#dc#,16#c0#,16#62#,16#34#,16#f0#);

      Scheme : Catena_KDF;
   begin

     Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new Blake2b1Bla_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "B1b2Bla-BRG failed");

     Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new Blake2b1Bla_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "B1b2Bla-DBG failed");
    
      ----------------------------------------------------

     Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new Blake2b1Bla_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "B1b2Bla-DBG (low HL) failed");

   end Catena_Test_B2b1Bla;

   procedure Catena_Test_GR(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 6;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#6c#,16#c9#,16#91#,16#46#,16#27#,16#57#,16#57#,16#63#,16#46#,16#ad#,16#fb#,16#fc#,16#7f#,16#cf#,16#38#,16#23#);
      target_butterfly   : Bytes(1..16):=(16#e3#,16#17#,16#8d#,16#2e#,16#d8#,16#92#,16#93#,16#7b#,16#f6#,16#df#,16#6a#,16#78#,16#3d#,16#a2#,16#c8#,16#17#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#de#,16#cc#,16#3d#,16#9d#,16#25#,16#3a#,16#14#);

      Scheme : Catena_KDF;
   begin

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  LanternflyTwo, 
                  Dummy, 
                  Gamma, 
                  new Blake2b_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash);
      
      Assert(hash=target_dragonfly, "LanternflyTwo failed");

      --------------------------------------------------

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  LanternflyTwo, 
                  LSB, 
                  Gamma, 
                  new Blake2b_Scheme, 
                  Blake2b);
      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "LanternflyTwo failed");
    
      ----------------------------------------------------
      
      Scheme.Initialize(hashlen_low, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            LanternflyTwo, 
            XS, 
            Gamma, 
            new Blake2b_Scheme, 
            Blake2b);
      Scheme.Derive(password, data, salt, hash_low);
     
      Assert(hash_low=target_butterfly_low_hl, "LanternflyTwo (low HL) failed");
       ---------------------------------------------------------
      ----------------------------------------------------------
      
      target_dragonfly(1..16):=(16#5a#,16#a3#,16#6c#,16#19#,16#ce#,16#8b#,16#5f#,16#ee#,16#17#,16#c8#,16#26#,16#32#,16#8c#,16#12#,16#99#,16#15#);
      target_butterfly(1..16):=(16#0e#,16#9e#,16#1f#,16#e9#,16#90#,16#d1#,16#aa#,16#8c#,16#6b#,16#87#,16#a3#,16#28#,16#ab#,16#11#,16#c0#,16#00#);
      target_butterfly_low_hl(1..7):=(16#08#,16#58#,16#02#,16#b9#,16#25#,16#96#,16#a8#);

      Scheme.Initialize(hashlen, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            LanternflyThree, 
            Dummy, 
            Gamma, 
            new Blake2b_Scheme, 
            Blake2b);
      Scheme.Derive(password, data, salt, hash);
      
      Assert(hash=target_dragonfly, "LanternflyThree failed");

      --------------------------------------------------

      Scheme.Initialize(hashlen, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            LanternflyThree, 
            LSB, 
            Gamma, 
            new Blake2b_Scheme, 
            Blake2b);
      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "LanternflyThree failed");
    
      ----------------------------------------------------

      Scheme.Initialize(hashlen_low, 
            tweak_id, 
            glow, 
            ghigh, 
            "rgg", 
            LanternflyThree, 
            XS, 
            Gamma, 
            new Blake2b_Scheme, 
            Blake2b);
      Scheme.Derive(password, data, salt, hash_low);
        
      Assert(hash_low=target_butterfly_low_hl, "LanternflyThree (low HL) failed");

   end Catena_Test_GR;

   procedure Catena_Test_Pcompress(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#ca#,16#23#,16#cb#,16#15#,16#3e#,16#bc#,16#71#,16#fd#,16#ab#,16#b2#,16#fb#,16#0e#,16#33#,16#99#,16#05#,16#f9#);
      target_butterfly   : Bytes(1..16):=(16#21#,16#84#,16#54#,16#1d#,16#d5#,16#7e#,16#cf#,16#20#,16#11#,16#ee#,16#c2#,16#fc#,16#fb#,16#43#,16#72#,16#3f#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#8e#,16#85#,16#96#,16#f5#,16#36#,16#60#,16#f9#);

      Scheme: Catena_KDF;
   begin
      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new PCompressG_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);


      Assert(hash=target_dragonfly, "PcompressG-BRG failed");

      ----------------------------------------------------
      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new PCompressG_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);


      Assert(hash=target_butterfly, "PcompressG-DBG failed");
    
      --------------------------------------------------


      Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new PCompressG_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash_low);
      Assert(hash_low=target_butterfly_low_hl, "PcompressG-DBG (low HL) failed");
      
      -------------------------------------------------------------
      -------------------------------------------------------------
      
      target_dragonfly        :=(16#03#,16#81#,16#c1#,16#91#,16#01#,16#fe#,16#07#,16#c9#,16#86#,16#ce#,16#a7#,16#09#,16#96#,16#a4#,16#4d#,16#19#);
      target_butterfly        :=(16#0a#,16#3a#,16#d8#,16#6d#,16#10#,16#99#,16#2e#,16#e3#,16#1c#,16#29#,16#fe#,16#cf#,16#59#,16#06#,16#f8#,16#10#);
      target_butterfly_low_hl :=(16#c8#,16#93#,16#b7#,16#4f#,16#b6#,16#8e#,16#1b#);

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new PCompressBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "PcompressBla-BRG failed");

      ----------------------------------------------------

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new PCompressBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "PcompressBla-DBG failed");
    
      --------------------------------------------------
        
      Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new PCompressBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "PcompressBla-DBG (low HL) failed");

   end Catena_Test_Pcompress;

   procedure Catena_Test_ArgonCF(T: in out Test_Cases.Test_Case'Class) is
      input : Bytes(0..63) := (others=>0);
      counter : Byte := 0;

      password : String := "password";
      data    : Bytes(0..3) := To_Bytes("data");
      salt     : Bytes(0..0) := (others=>0);
      glow     : Natural := 4;
      ghigh    : Natural := 6;
      hashlen  : Natural := 16;
      hashlen_low  : Natural := 7;

      hash     : Bytes(0..hashlen-1);
      hash_low: Bytes(0..hashlen_low-1);
      tweak_id : Natural := 0;

      target_dragonfly   : Bytes(1..16):=(16#25#,16#72#,16#13#,16#22#,16#30#,16#98#,16#81#,16#cf#,16#4d#,16#d2#,16#55#,16#07#,16#92#,16#b7#,16#a8#,16#50#);
      target_butterfly   : Bytes(1..16):=(16#bf#,16#d8#,16#0d#,16#80#,16#a9#,16#10#,16#3a#,16#3d#,16#55#,16#84#,16#50#,16#42#,16#e0#,16#1e#,16#e6#,16#30#);
      target_butterfly_low_hl   : Bytes(1..7):=(16#42#,16#c8#,16#73#,16#79#,16#78#,16#9e#,16#0b#);

      Scheme : Catena_KDF;

   begin
      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new ArgonCF_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "ArgonCF-BRG failed");

      ----------------------------------------------------

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new ArgonCF_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "ArgonCF-DBG failed");
    
      --------------------------------------------------
        
      Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new ArgonCF_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "ArgonCF-DBG (low HL) failed");
      
      -------------------------------------------------------------
      -------------------------------------------------------------
      
      target_dragonfly        :=(16#e0#,16#eb#,16#9f#,16#63#,16#4d#,16#f9#,16#b3#,16#5e#,16#61#,16#5f#,16#dd#,16#34#,16#59#,16#da#,16#90#,16#22#);
      target_butterfly        :=(16#4a#,16#3a#,16#27#,16#1f#,16#86#,16#af#,16#c1#,16#10#,16#ea#,16#75#,16#d3#,16#6e#,16#4e#,16#ed#,16#c8#,16#c2#);
      target_butterfly_low_hl :=(16#f8#,16#16#,16#fc#,16#9e#,16#0d#,16#78#,16#77#);

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Dragonfly, 
                  XS, 
                  Gamma, 
                  new ArgonCFBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_dragonfly, "ArgonCFBla-BRG failed");

      ----------------------------------------------------

      Scheme.Initialize(hashlen, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  LSB, 
                  Gamma, 
                  new ArgonCFBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash);

      Assert(hash=target_butterfly, "ArgonCFBla-DBG failed");
    
      --------------------------------------------------
        
      Scheme.Initialize(hashlen_low, 
                  tweak_id, 
                  glow, 
                  ghigh, 
                  "rgg", 
                  Butterfly, 
                  Dummy, 
                  Gamma, 
                  new ArgonCFBla_Scheme, 
                  Blake2b);

      Scheme.Derive(password, data, salt, hash_low);

      Assert(hash_low=target_butterfly_low_hl, "ArgonCFBla-DBG (low HL) failed");

   end Catena_Test_ArgonCF;


end Test.Catena;
