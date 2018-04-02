-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with This program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- This unit, or you link This unit with other files to produce an
-- eXecutable, This unit does not by itself cause the resulting
-- eXecutable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- eXecutable file might be covered by the GNU Public License.


with Ada.Text_IO;                       use Ada.Text_IO;
with Crypto.Types.XORShiftSTAR;
with Ada.Unchecked_Conversion;
with Crypto.Symmetric.Hashfunction_SHA512;
with Crypto.Symmetric.Hashfunction_Blake2b;
with Crypto.Symmetric.Compress_Blake2b1; use Crypto.Symmetric.Compress_Blake2b1;
with System; use System;

package body Crypto.Symmetric.KDF_Catena is

   --Interface function for static 64 Bytes Output, assuming p=8,
   -- r=8 and N=Security_Parameter
   procedure Derive(This	: in out Catena_KDF;
                    Salt	: in 	Bytes;
                    Password	: in	Bytes;
                    Key		: out	Bytes) is
   Data : constant Bytes(1..0) := (others=>0);
   begin
      Derive(This,To_String(Password), Data, Salt, Key);
   end Derive;

  procedure Derive_Keyed (This  : in out Catena_KDF;
                    Password  : in String;
                    data      : in Bytes;
                    Salt      : in Bytes;
                    Key      : in Bytes;
                    UID      : in Bytes;
                    Hash      : out Bytes) is
  Tmp_Hash : Bytes(0..63);
  function Cast is new Ada.Unchecked_Conversion (Integer, Byte);
  G_High_Byte : Bytes(0..0);
  begin
    Derive(This, Password, Data, Salt, Hash);

    G_High_Byte(0) := Cast(This.G_High);
    Tmp_Hash := To_Bytes(Hash_4(This, Key, UID, G_High_Byte, Key));

    Hash := Hash xor Tmp_Hash(0..This.Hash_Length-1);

  end Derive_Keyed;


   --function for setting security parameter, used here for setting round count
   procedure Initialize(This	: out Catena_KDF;
                        Key_Length: in Natural) is
   begin
    This.Hash_Length := Key_Length;
    This.D := 1;
    This.G_Low  := 17;
    This.G_High := 17;
    -- Structure
    This.Structure := ('r', 'g', 'g', others=>' ');
    This.Structure_Length := 3;
    This.Used_Compression := Blake2b1;
    This.Used_Hashfunction := Blake2b;
    This.Used_Mode := Dragonfly;
    This.Used_Phi := Dummy;
    This.Used_Random := Gamma;

    This.Hash_Fast_Handle := new Blake2b1_Scheme;
   end Initialize;

   procedure Initialize(This      : out Catena_KDF;
                        Key_Length: in Natural;
                        D  : in Natural;
                        G_Low      : in Natural;
                        G_High     : in Natural;
                        Structure : in String;
                        Mode      : in Mode_ID;
                        Phi       : in Phi_ID;
                        Random    : in Random_ID;
                        HF_Access  : in Hash_Fast_Access;
                        Hashfunction  : in Hashfunction_ID) is
   begin

    This.Hash_Fast_Handle := HF_Access;

    This.Hash_Length := Key_Length;
    This.D := D;
    This.G_Low := G_Low;
    This.G_High := G_High;
    This.Structure(This.Structure'First .. This.Structure'First + Structure'Length - 1) := Structure;
    This.Structure_Length := Structure'Length;
    This.Used_Hashfunction := Hashfunction;
    This.Used_Mode := Mode;
    This.Used_Phi := Phi;
    This.Used_Random := Random;
   end Initialize;



  --core catena function
  procedure Derive (This  : in out Catena_KDF;
                    Password  : in String;
                    Data      : in Bytes;
                    Salt      : in Bytes;
                    Hash      : out Bytes) is
    X       : DW_Block512;
    X_Long  : Bytes(0..This.Hash_Fast_Handle.get_Length -1);
    HV       : DW_Block512;
    X_Bytes : Bytes(0..63);
    G_Bytes : Bytes(0..0);
    T       : Bytes(0..3);
    Lambda  : constant Integer := Count_Characters(This.Structure(This.Structure'First .. This.Structure'First + This.Structure_Length-1), 'g');
    function Cast is new Ada.Unchecked_Conversion (Integer, Byte);
  begin
    
    HV := This.Hash_1(To_Bytes(This.Get_V_String));
    X := This.Hash_1(data);

    T(0):=Cast(This.D);
    T(1):=Cast(Lambda); 
    T(2):=Cast(This.Hash_Length);
    T(3):=Cast(Salt'Length);       

    X := This.Hash_5(To_Bytes(HV), T, To_Bytes(X), To_Bytes(Password), Salt);

      Flap( This => This,
                X=>To_Bytes(X),
                g=>(This.G_Low+1)/2,
                Salt=>Salt,
                Hash=>X_Long);

    for g in Natural range This.G_Low..This.G_High loop
      if g = This.G_Low then
        Flap( This => This,
                  X=>X_Long,
                  g=>g,
                  Salt=>Salt,
                  Hash=>X_Long);
      else
        Flap( This => This,
                  X=>To_Bytes(X),
                  g=>g,
                  Salt=>Salt,
                  Hash=>X_Long);
      end if; 

      G_Bytes(0):= Cast(g);
      X := This.Hash_2(G_Bytes, X_Long);
      X_Bytes := To_Bytes(X);
      X_Bytes(This.Hash_Length..63):=(others=>0);
      X := To_DW_Block512(X_Bytes);
    end loop;
  
    X_Bytes := To_Bytes(X);
    Hash := X_Bytes(0..This.Hash_Length-1);

  end Derive;

function Get_V_String(This  : in out Catena_KDF) return String is
begin
  case This.Used_Mode is
    when Dragonfly => return "Dragonfly";
    when Butterfly => return "Butterfly";
    when LanternflyTwo => return "LanternflyTwo";
    when LanternflyThree => return "LanternflyThree";
  end case;
end;

  procedure H_Init(This  : in out Catena_KDF; 
                    X     : in Bytes;
                    VM_1   : out Bytes;
                    VM_2   : out Bytes) is
    l   : constant Natural := (This.Hash_Fast_Handle.get_Length/64) * 2; 
    HashBytes : Bytes(0..l*64-1);
    One_Byte:  Bytes(0..0);
    function Cast is new Ada.Unchecked_Conversion (Integer, Byte);
  begin
    for i in Natural range 0..l-1 loop
      One_Byte(0):=Cast(i);
      HashBytes(i*64..(i+1)*64 -1):= To_Bytes(This.Hash_2(One_Byte, X));
    end loop;
    VM_1 := HashBytes(0..(l/2)*64 - 1);
    VM_2 := HashBytes((l/2)*64..l*64 - 1);
  end H_Init;


    procedure H_First(This  : in out Catena_KDF;
                    Index_1    : in Integer;
                    Index_2      : in Integer;
                    Hash         : in Integer) is
    X:  DW_Block512;
    function Cast is new Ada.Unchecked_Conversion (Integer, Byte);
    One_Byte:  Bytes(0..0);
    l: constant Natural := (This.Hash_Fast_Handle.get_Length/64);
    HashBytes : Bytes(0..l*64-1);
  begin
    X := This.Hash_2(This.State_Handle(Index_1)(0..l*64-1), This.State_Handle(Index_2)(0..l*64-1));
    for i in Natural range 0..(l - 1) loop
      One_Byte(0):=Cast(i);
      HashBytes(i*64..(i+1)*64 -1) := To_Bytes(This.Hash_2(One_Byte, To_Bytes(X)));
    end loop;

    This.State_Handle(Hash)(0..(l*64)-1) := HashBytes;

  end H_First;

    procedure H_First(This  : in out Catena_KDF;
                    Input_1    : in Bytes;
                    Input_2      : in Bytes;
                    Hash         : out Bytes) is
    X:  DW_Block512;
    function Cast is new Ada.Unchecked_Conversion (Integer, Byte);
    One_Byte:  Bytes(0..0);
    l: constant Natural := (This.Hash_Fast_Handle.get_Length/64);
    HashBytes : Bytes(0..l*64-1);
  begin
    X := This.Hash_2((Input_1), (Input_2));
    for i in Natural range 0..(l - 1) loop
      One_Byte(0):=Cast(i);
      HashBytes(i*64..(i+1)*64 -1) := To_Bytes(This.Hash_2(One_Byte, To_Bytes(X)));
    end loop;
    Hash := HashBytes;
  end H_First;

  --X function
   procedure Flap(This  : in out Catena_KDF;
                   X     : in Bytes;
                  g     : in Integer;
                  Salt  : in Bytes;
                  Hash  : out Bytes) is

    l : constant Natural := This.Hash_Fast_Handle.get_Length;

    VM_1   : Bytes(0..l -1);
    VM_2   : Bytes(0..l -1);

    the_graph : graph_access;
    the_Random: Random_access;
    the_Phi : Phi_access;
    ----------
    Hash_Tmp : Bytes(0..This.Hash_Fast_Handle.get_Length -1);
    ----------
   begin
    
    case This.Used_Phi is
      when LSB => the_Phi   := new PhiLayer_LSB.LSB_PhiLayer;
      when XS => the_Phi   := new PhiLayer_XS.XS_PhiLayer;
      when Dummy => the_Phi := new PhiLayer_Dummy.Dummy_PhiLayer;
    end case;

    case This.Used_Mode is
      when Dragonfly => the_graph       := new Graph_BRG.BRG_Graph;
      when Butterfly => the_graph       := new Graph_DBG.DBG_Graph;
      when LanternflyTwo => the_graph   := new Graph_GR2.GR2_Graph;
      when LanternflyThree => the_graph := new Graph_GR3.GR3_Graph; 
    end case;

    This.Init_State(the_graph.Memory(g));
    
    the_Random := new RandomLayer_Gamma.Gamma_RandomLayer;
    the_Random.internal_graph := the_graph;
    the_Phi.internal_graph := the_graph;
    
    This.H_Init(X, VM_1, VM_2);
    
    This.Hash_Fast_Handle.Reset;
    This.State_Handle(0)(0..l-1) := This.Hash_Fast_Handle.Process(VM_1(0..l-1), 
                                                                  VM_2(0..l-1),
                                                                  0);
      This.State_Handle(1)(0..l-1) := This.Hash_Fast_Handle.Process(This.State_Handle(0)(0..l-1), 
                                                                  VM_1(0..l-1),
                                                                  1);
    
    for i in Integer range 2..(2**g)-1 loop
        This.State_Handle(i)(0..l-1) := This.Hash_Fast_Handle.Process(This.State_Handle(i-1)(0..l-1), 
                                                              This.State_Handle(i-2)(0..l-1),
                                                              i);
    end loop;

    for i in Natural range This.Structure'First .. This.Structure'First + This.Structure_Length-1 loop
    case This.Structure(i) is

      when 'r'=>
        the_Random.Process(This, g, Salt);
        null;         
      when 'g'=>
        the_graph.Process(This, g ,Hash_Tmp);
      when others =>
        Put_Line("Wrong use of Structure!");
    end case;
    end loop;

    the_Phi.Process(This, g, Hash_Tmp);
    
    Hash := Hash_Tmp;

  end Flap;

    --bit reversal for BRG
   function Bit_Reverse( X  : in Integer;
                        n  : in Integer) return Integer is

    function Cast is new Ada.Unchecked_Conversion (Integer, Word);
    function CastBack is new Ada.Unchecked_Conversion (Word, Integer);
    XWord : Word := Cast(X);
   begin
    XWord := Shift_Right(XWord and 16#aaaaaaaa#, 1) or Shift_Left(XWord and 16#55555555#, 1);
    XWord := Shift_Right(XWord and 16#cccccccc#, 2) or Shift_Left(XWord and 16#33333333#, 2);
    XWord := Shift_Right(XWord and 16#f0f0f0f0#, 4) or Shift_Left(XWord and 16#0f0f0f0f#, 4);
    XWord := Shift_Right(XWord and 16#ff00ff00#, 8) or Shift_Left(XWord and 16#00ff00ff#, 8);
    
    Xword := Shift_Right(XWord, 16) or Shift_Left(XWord, 16);

    return CastBack(Shift_Right(Xword, 32-n));
   end Bit_Reverse;


  function Idx( i   : in Integer;
                j   : in Integer;
                co  : in Integer;
                c   : in Integer;
                m   : in Integer) return Integer is
  i_working : Integer := i;
  begin
    i_working := i_working + co;
    if (i_working mod 3) = 0 then
      return j;
    elsif (i_working mod 3) = 1 then
      if j<m then
        return j+c;
      else
        return j-m;
      end if;
    else
      return j+m;
    end if;
  end Idx;

  function Sigma( g : in Integer;
                  i : in Integer;
                  j : in Integer) return Integer is
  function toInteger is new Ada.Unchecked_Conversion (Dword, Integer);
  function toDword is new Ada.Unchecked_Conversion (Integer, Dword);
  one : constant Dword := 1;
  begin
    if i < g then
      return toInteger(toDword(j) Xor Shift_Left(one, g-1-i));
    else
      return toInteger(toDword(j) Xor Shift_Left(one, i-(g-1)));
    end if;
  end Sigma;

    procedure Init_State(This : in out Catena_KDF;
                       size : in Integer) is
    my_bytes : constant Bytes(0..This.Hash_Fast_Handle.get_Length -1) := (others=>0);
  begin
    This.State_Handle := new State_Array_X(0..size-1);
    
    for I in 0..size-1 loop
      This.State_Handle(i) := new Bytes(0..This.Hash_Fast_Handle.get_Length -1);
    end loop;
    
    This.State_Handle(0)(0..This.Hash_Fast_Handle.get_Length -1) := My_Bytes;
  end Init_State;

  function Hash_1 (This  : in out Catena_KDF;
                    Left : Bytes) return DW_Block512 is
  begin
    if This.Used_Hashfunction = SHA512  then
    return Crypto.Symmetric.Hashfunction_SHA512.Hash(Left);
    else 
    return Crypto.Symmetric.Hashfunction_Blake2b.Hash(Left);
    end if;
  end Hash_1;

function Hash_2 (This  : in out Catena_KDF;
                    Left : Bytes;
                 Right: Bytes) return DW_Block512 is

  begin
    if This.Used_Hashfunction = SHA512  then
    return Crypto.Symmetric.Hashfunction_SHA512.Hash(Left&Right);
    else 
    return Crypto.Symmetric.Hashfunction_Blake2b.Hash(Left&Right);
    end if;
end Hash_2;  

 function Hash_3  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three: Bytes) return DW_Block512 is

  begin
    if This.Used_Hashfunction = SHA512  then
    return Crypto.Symmetric.Hashfunction_SHA512.Hash(one&two&three);
    else 
    return Crypto.Symmetric.Hashfunction_Blake2b.Hash(one&two&three);
    end if;
end Hash_3;  

 function Hash_4  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three: Bytes;
                   four: Bytes) return DW_Block512 is

  begin
    if This.Used_Hashfunction = SHA512  then
    return Crypto.Symmetric.Hashfunction_SHA512.Hash(one&two&three&four);
    else 
    return Crypto.Symmetric.Hashfunction_Blake2b.Hash(one&two&three&four);
    end if;
end Hash_4; 

  function Hash_5  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three : Bytes;
                   four: Bytes;
                   five: Bytes) return DW_Block512 is
  begin
      if This.Used_Hashfunction = SHA512  then
    return Crypto.Symmetric.Hashfunction_SHA512.Hash(one&two&three&four&five);
    else 
    return Crypto.Symmetric.Hashfunction_Blake2b.Hash(one&two&three&four&five);
    end if;
  end Hash_5;

     function Count_Characters(Input : String; Target : Character) return Integer is
      counter : Natural := 0;
     begin
      for i in Input'range loop
        if input(i)=Target then
        counter := counter +1;
        end if;
      end loop;
      return counter;
     end Count_Characters;


    package body Graph_BRG is

    procedure Process(This     : in out BRG_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes) is
    Previous_Index  : Natural;
    Current_Index   : Natural;
    l               : constant Natural := Context.Hash_Fast_Handle.get_Length;
    c               : constant Natural := 2**Garlic;
    begin

    if This.currentL mod 2 = 0 then
      Context.H_First(c-1, 0, 0);
       Context.Hash_Fast_Handle.Reset;
      Previous_Index := 0;
      Current_Index := 0;
      for i in Natural range 1..c-1 loop
        Current_Index := Bit_Reverse(i, Garlic);
        Context.State_Handle(Current_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                      Context.State_Handle(Previous_Index)(0..l-1), 
                                                                      Context.State_Handle(Current_Index)(0..l-1),
                                                                      i);
        Previous_Index := Current_Index;
      end loop;
      Hash := Context.State_Handle(c-1)(0..l-1);
      This.currentL := This.currentL +1;

    else 
      Context.H_First(c-1, 0, 0);
       Context.Hash_Fast_Handle.Reset;
      for i in Natural range 1..c-1 loop
        Context.State_Handle(i)(0..l-1) := Context.Hash_Fast_Handle.Process(  Context.State_Handle(i-1)(0..l-1), 
                                                                      Context.State_Handle(i)(0..l-1),
                                                                      i);
      end loop;
      This.currentL := This.currentL + 1;
      end if;
    
    Hash := Context.State_Handle(c-1)(0..l-1);
    end Process;

   function Index(This       : in out BRG_Graph;
                   Index      : in Natural;
                    Garlic     : in Natural) return Integer is
   begin
      if This.currentL mod 2 = 0 then
      return Index;
      else 
      return Bit_Reverse(Index, Garlic);
      end if;
   end Index;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function Memory( This       : in out BRG_Graph;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   begin
   return 2**Garlic;
   end Memory;


   end Graph_Brg;


    package body Graph_GR2 is

   procedure Process(This     : in out GR2_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes) is
    Current_Index   : Natural := 0;
    c              : constant Natural := 2**Garlic;
    l : constant Natural := Context.Hash_Fast_Handle.get_Length;
    begin
      Context.H_First(c-1, 0, c);
      Context.Hash_Fast_Handle.Reset;
      for i in Natural range 1..c-1 loop
        Current_Index := Phi(This, i, Garlic);
        Context.State_Handle(c+i)(0..l-1) := Context.Hash_Fast_Handle.Process(  Context.State_Handle(c+i-1)(0..l-1), 
                                                                      Context.State_Handle(Current_Index)(0..l-1),
                                                                      i);
      end loop;
      
      for i in Natural Range 0..c-1 loop
        Context.State_Handle(i)(0..l-1) := Context.State_Handle(i+c)(0..l-1);
      end loop;
  
      Hash := Context.State_Handle(c-1)(0..l-1);
    end Process;


   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   pragma Warnings (Off, "formal parameter ""Garlic"" is not referenced");
   function Index(This       : in out GR2_Graph;
                   Index      : in Natural;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   pragma Warnings (On, "formal parameter ""Garlic"" is not referenced");
   begin
      return Index;
   end Index;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function Memory( This       : in out GR2_Graph;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   begin
   return 2**(Garlic+1);
   end Memory;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
     function Phi(This       : in out GR2_Graph;
                  Index      : in Natural;
                  Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
     function Cast is new Ada.Unchecked_Conversion (Integer, Dword);
    function Cast2 is new Ada.Unchecked_Conversion (Dword, Integer);
     revInt : constant Integer := Bit_Reverse(Index, Garlic);
     rev : constant Dword := Cast(revInt);
     invRev : Dword := rev Xor 16#ffffffffffffffff#;
     begin
      invRev := Shift_Left(invRev, 64-Garlic);
      invRev := Shift_Right(invRev, (64-Garlic) + (Garlic)/2);
      return Cast2(rev Xor invRev);
     end Phi;
   end Graph_GR2;


    package body Graph_GR3 is

   procedure Process(This     : in out GR3_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes) is
    Current_Index   : Natural := 0;
    c              : constant Natural := 2**Garlic;
    l : constant Natural := Context.Hash_Fast_Handle.get_Length;
    begin
      Context.H_First(c-1, 0, c);
      Context.Hash_Fast_Handle.Reset;
      for i in Natural range 1..c-1 loop
        Current_Index := Phi(This, i, Garlic);
        Context.State_Handle(c+i)(0..l-1) := Context.Hash_Fast_Handle.Process(  Context.State_Handle(c+i-1)(0..l-1), 
                                                                      Context.State_Handle(Current_Index)(0..l-1),
                                                                      i);
      end loop;
      
      for i in Natural Range 0..c-1 loop
        Context.State_Handle(i)(0..l-1) := Context.State_Handle(i+c)(0..l-1);
      end loop;

      Hash := Context.State_Handle(c-1)(0..l-1);
    end Process;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   pragma Warnings (Off, "formal parameter ""Garlic"" is not referenced");
   function Index(This       : in out GR3_Graph;
                   Index      : in Natural;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   pragma Warnings (On, "formal parameter ""Garlic"" is not referenced");
   begin
      return Index;
   end Index;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function Memory( This       : in out GR3_Graph;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   begin
   return 2**(Garlic+1);
   end Memory;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
     function Phi(This       : in out GR3_Graph;
                  Index      : in Natural;
                  Garlic     : in Natural) return Integer is
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
     function Cast is new Ada.Unchecked_Conversion (Integer, Dword);
    function Cast2 is new Ada.Unchecked_Conversion (Dword, Integer);
     revInt : constant Integer := Bit_Reverse(Index, Garlic);
     rev : constant Dword := Cast(revInt);
     invRev : Dword := rev Xor 16#ffffffffffffffff#;
     begin
      invRev := Shift_Left(invRev, 64-Garlic);
      invRev := Shift_Right(invRev, (64-Garlic) + (Garlic)/3);
      return Cast2(rev Xor invRev);
     end Phi;
   end Graph_GR3;

   package body Graph_DBG is


    procedure Process(This     : in out DBG_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes) is
     c : constant Integer := (2**Garlic);
     m : constant Integer := 2**(Garlic-1);
    l : constant Integer := Context.Hash_Fast_Handle.get_Length;
    tmp            : Bytes(0..l -1);
    i_out          : Integer;

    begin
      null;
      for i in Natural range 1..2*Garlic-1 loop
        tmp := Context.State_Handle(Idx(i-1,c-1,This.co,c,m))(0..l-1) Xor Context.State_Handle(Idx(i-1,0,This.co,c,m))(0..l-1);
        Context.H_First(tmp, Context.State_Handle(Idx(i-1,Sigma(Garlic,i-1,0),This.co,c,m))(0..l-1), tmp);
        Context.State_Handle(Idx(i,0,This.co,c,m))(0..l-1) := tmp;
        Context.Hash_Fast_Handle.Reset;
        for j in Natural range 1..c-1 loop
          tmp := Context.State_Handle(Idx(i,j-1,This.co,c,m))(0..l-1) Xor Context.State_Handle(Idx(i-1,j,This.co,c,m))(0..l-1);
          tmp := Context.Hash_Fast_Handle.Process(tmp, Context.State_Handle(Idx(i-1,Sigma(Garlic,i-1,j),This.co,c,m))(0..l-1), j);
          Context.State_Handle(Idx(i,j,This.co,c,m))(0..l-1) := tmp;
        end loop;
        i_out := i+1;
      end loop;
    This.co := ((This.co + (i_out-1)) mod 3);
    Hash := Context.State_Handle(c-1)(0..l-1);
  
    end Process;

   function Index(This       : in out DBG_Graph;
                   Index      : in Natural;
                    Garlic     : in Natural) return Integer is
     c : constant Integer := (2**Garlic);
     m : constant Integer := 2**(Garlic-1);
     begin
    return Idx(0, Index,This.co, c, m);
   end Index;
  
   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
     function Memory( This       : in out DBG_Graph;
                    Garlic     : in Natural) return Integer is
   pragma Warnings (ON, "formal parameter ""This"" is not referenced");
     begin
    return (2**Garlic + 2**(Garlic-1)); 
    end Memory;

   end Graph_DBG;


  package body RandomLayer_Gamma is
  
    procedure Process(This     : in out Gamma_RandomLayer;
                    Context  : in out Catena_KDF; 
                    Garlic   : in Integer;
                     Input     : in Bytes) is
    flo       : constant Float := (3.0/4.0)*Float(Garlic);
    q         : constant Natural := 2**Integer((Float'Ceiling(flo)));
    j1, j2  : Natural;
    package XS_Blake2b is new Crypto.Types.XORShiftSTAR(Hashfunction_Blake2b);
    package XS_SHA512 is new Crypto.Types.XORShiftSTAR(Hashfunction_SHA512);
    
    l : constant Natural := Context.Hash_Fast_Handle.get_Length;

  begin

    case Context.Used_Hashfunction is
      when SHA512 =>
      XS_SHA512.init(Input);
    Context.Hash_Fast_Handle.Reset;
    for i in Natural range 0..q-1 loop
      j1 := XS_SHA512.getValue(Garlic);
      j2 := XS_SHA512.getValue(Garlic);
      Context.State_Handle(This.internal_graph.Index(j1, Garlic))(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(This.internal_graph.Index(j1, Garlic))(0..l-1), 
                                                                            Context.State_Handle(This.internal_graph.Index(j2, Garlic))(0..l-1),
                                                                            i);
    end loop;
      when Blake2b =>
      XS_Blake2b.init(Input);
    Context.Hash_Fast_Handle.Reset;
    for i in Natural range 0..q-1 loop
      j1 := XS_Blake2b.getValue(Garlic);
      j2 := XS_Blake2b.getValue(Garlic);
      Context.State_Handle(This.internal_graph.Index(j1, Garlic))(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(This.internal_graph.Index(j1, Garlic))(0..l-1), 
                                                                            Context.State_Handle(This.internal_graph.Index(j2, Garlic))(0..l-1),
                                                                            i);      
    end loop;  
    end case;
    
    
    end;

   end RandomLayer_Gamma;

   -------------------------------------------
  package body PhiLayer_Dummy is

    procedure Process(This     : in out Dummy_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes) is
    -- new
    l : constant Natural := Context.Hash_Fast_Handle.get_Length;
    begin
      Hash := Context.State_Handle(This.internal_graph.Index( 2**Garlic -1, Garlic))(0..l-1);

    end Process;

   end PhiLayer_Dummy;
   --------------------------------------------

   package body PhiLayer_LSB is

    procedure Process(This     : in out LSB_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes) is
    l :constant  Natural := Context.Hash_Fast_Handle.get_Length;
    c : constant Integer := 2**Garlic;
    lastIndex : constant Integer := This.internal_graph.Index(c-1, Garlic);
    Current_Index : Integer := This.internal_graph.Index(Random(Context,Context.State_Handle(lastIndex)(0..l-1), Garlic),Garlic);
    Previous_Index : Integer := This.internal_graph.Index(0, Garlic);
    currentNumber : Integer := 0;
    begin
    Context.State_Handle(Previous_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(Current_Index)(0..l-1),
                                                                            0);   
    Current_Index := Previous_Index;
    Previous_Index := Current_Index;

    for i in Natural range 1..c-1 loop
      Current_Index := This.internal_graph.Index(i, Garlic);
      currentNumber := Random(Context, Context.State_Handle(Previous_Index)(0..l-1), Garlic);
      Context.State_Handle(Current_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(This.internal_graph.Index(currentNumber, Garlic))(0..l-1),
                                                                            i);   
      Previous_Index := Current_Index;
    end loop;
    Hash := Context.State_Handle(This.internal_graph.Index( 2**Garlic -1, Garlic))(0..l-1);
    end Process;
    


    function reverseBytes(input : Bytes) return Bytes is
    returnValue : Bytes(input'Range);
    begin
      if System.Default_Bit_Order = System.Low_Order_First then 
        for I in input'Range loop
          returnValue(input'last - i) := input(i);
        end loop;
       return returnValue;
      else
      return input;
      end if;
    end;

    function Random(Context  : in out Catena_KDF;
                        Input    : in Bytes;
                        Garlic   : in Integer) return Integer is

    tmpArray : constant Dwords(0..Context.Hash_Fast_Handle.get_Length/8 - 1) := To_Dwords((input));

    tmpDword : Dword := tmpArray(tmpArray'First);
    function Cast is new Ada.Unchecked_Conversion (Dword, Natural);
    begin
      tmpDword := To_Dword(reverseBytes(To_Bytes(tmpDword)));
      tmpDword := Shift_Right(tmpDword, 64-Garlic);
    return Cast(tmpDword);
    end Random;

   end PhiLayer_LSB;
   --------------------------------------------

     package body PhiLayer_XS is

     procedure Process(This     : in out XS_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes) is
    l : constant Natural := Context.Hash_Fast_Handle.get_Length;
    package XS_Blake2b is new Crypto.Types.XORShiftSTAR(Hashfunction_Blake2b);
    package XS_SHA512 is new Crypto.Types.XORShiftSTAR(Hashfunction_SHA512);

    c : constant Integer := 2**Garlic;
    Current_Index : Integer;
    Previous_Index : Integer := This.internal_graph.Index(0, Garlic);
    XS_Number : Integer := 0;

    begin
    case Context.Used_Hashfunction is
      when Blake2b =>
      XS_Blake2b.init((Context.State_Handle(This.internal_graph.Index(c - 1, Garlic))(0..l-1)));
      Current_Index := This.internal_graph.Index(XS_Blake2b.getValue(Garlic),Garlic);
      Context.Hash_Fast_Handle.Reset;
      Context.State_Handle(Previous_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(Current_Index)(0..l-1),
                                                                            0);   
      for i in Natural range 1..c-1 loop
        Current_Index := This.internal_graph.Index(i, Garlic);
        XS_Number := XS_Blake2b.getValue(Garlic);
        Context.State_Handle(Current_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(This.internal_graph.Index(XS_Number, Garlic))(0..l-1),
                                                                            i);   
        Previous_Index := Current_Index;
        end loop;
        Hash := Context.State_Handle(This.internal_graph.Index( 2**Garlic -1, Garlic))(0..l-1);
      when SHA512 =>
      XS_SHA512.init((Context.State_Handle(This.internal_graph.Index(c - 1, Garlic))(0..l-1)));
      Current_Index := This.internal_graph.Index(XS_SHA512.getValue(Garlic),Garlic);
      Context.Hash_Fast_Handle.Reset;
      Context.State_Handle(Previous_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(Current_Index)(0..l-1),
                                                                            0);   
      for i in Natural range 1..c-1 loop
        Current_Index := This.internal_graph.Index(i, Garlic);
        XS_Number := XS_SHA512.getValue(Garlic);
        Context.State_Handle(Current_Index)(0..l-1) := Context.Hash_Fast_Handle.Process(
                                                                            Context.State_Handle(Previous_Index)(0..l-1), 
                                                                            Context.State_Handle(This.internal_graph.Index(XS_Number, Garlic))(0..l-1),
                                                                            i);   
        Previous_Index := Current_Index;
        end loop;
        Hash := Context.State_Handle(This.internal_graph.Index( 2**Garlic -1, Garlic))(0..l-1);
    end case;
    end Process;

   end PhiLayer_XS;

pragma Optimize (Time);
end Crypto.Symmetric.KDF_Catena;
