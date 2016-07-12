-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- eXecutable, this unit does not by itself cause the resulting
-- eXecutable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- eXecutable file might be covered by the GNU Public License.

with Crypto.Symmetric.KDF;
with Crypto.Types; use Crypto.Types;

with Crypto.Symmetric.Compress_CFAES; use Crypto.Symmetric.Compress_CFAES;
with Crypto.Symmetric.Compress; use Crypto.Symmetric.Compress;

pragma Elaborate_All (Crypto.Symmetric.KDF);

package Crypto.Symmetric.KDF_Catena is

  type Hash_Fast_Access is access Compress.Compress_Scheme'Class;

   package KDF is new
     Crypto.Symmetric.KDF(Return_Type => Bytes);
   use KDF;

   type Catena_KDF is new KDF.KDF_Scheme with private;

  -- Enumerations
  type Compression_ID is (ArgonCF, ArgonCFBla, Blake2b, Blake2b1, Blake2b1Bla, CFAES, PCompressG, PCompressBla, SHA512);
  type Hashfunction_ID is (Blake2b, SHA512);
  type Mode_ID is (Dragonfly, Butterfly, LanternflyTwo, LanternflyThree);
  type Phi_ID is (Dummy, XS, LSB);
  type Random_ID is (Gamma);

   --Interface functions

   overriding
   procedure Initialize(This	: out Catena_KDF;
                        Key_Length: in Natural);

   procedure Initialize(This      : out Catena_KDF;
                        Key_Length: in Natural;
                        D         : in Natural;
                        G_Low      : in Natural;
                        G_High     : in Natural;
                        Structure : in String;
                        Mode      : in Mode_ID;
                        Phi       : in Phi_ID;
                        Random    : in Random_ID;
                        HF_Access  : in Hash_Fast_Access;
                        Hashfunction  : in Hashfunction_ID);

   overriding
   procedure Derive(This  : in out Catena_KDF;
                    Salt  : in  Bytes;
                    Password  : in  Bytes;
                    Key   : out Bytes);

    procedure Derive(This  : in out Catena_KDF;
                    Password  : in String;
                    Data      : in Bytes;
                    Salt      : in Bytes;
                    Hash      : out Bytes);

    procedure Derive_Keyed (This  : in out Catena_KDF;
                    Password  : in String;
                    Data      : in Bytes;
                    Salt      : in Bytes;
                    Key      : in Bytes;
                    UID      : in Bytes;
                    Hash      : out Bytes);

private

  -- States for different HashFast Sizes
  type Bytes_Access is access Bytes;
  type State_Array_X is array (Integer range <>) of Bytes_Access;
  type State_Array_Access_X is access State_Array_X;

  -- type State_Array_8k is array (Integer range <>) of DW_Block8192;
  -- type State_Array_512 is array (Integer range <>) of DW_Block512;

  -- type State_Array_Access_8k is access State_Array_8k;
  -- type State_Array_Access_512 is access State_Array_512;

function Get_V_String(This  : in out Catena_KDF) return String;

  procedure H_Init(This  : in out Catena_KDF;
                    X     : in Bytes;
                    VM_1   : out Bytes;
                    VM_2   : out Bytes);

  procedure Init_State(This : in out Catena_KDF;
                       size : in Integer);


  procedure H_First(This  : in out Catena_KDF;
                    Index_1    : in Integer;
                    Index_2      : in Integer;
                    Hash         : in Integer);

  procedure H_First(This  : in out Catena_KDF;
                    Input_1    : in Bytes;
                    Input_2      : in Bytes;
                    Hash         : out Bytes);

  function Bit_Reverse(X  : in Integer;
                      n  : in Integer) return Integer;

  procedure Flap(This  : in out Catena_KDF;
                   X     : in Bytes;
                  g     : in Integer;
                  Salt  : in Bytes;
                  hash  : out Bytes);

  function Idx( i   : in Integer;
                j   : in Integer;
                co  : in Integer;
                c   : in Integer;
                m   : in Integer) return Integer;

  function sigma( g : in Integer;
                  i : in Integer;
                  j : in Integer) return Integer;

  -- pragma Inline(sigma, Idx, Bit_Reverse, gamma);

  function Hash_1 (This  : in out Catena_KDF;
                    Left : Bytes) return DW_Block512;

  function Hash_2  (This : in out Catena_KDF;
                    Left : Bytes;
                   Right: Bytes) return DW_Block512;
  function Hash_3  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three: Bytes) return DW_Block512;
  function Hash_4  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three: Bytes;
                   four: Bytes) return DW_Block512;

  function Hash_5  (This : in out Catena_KDF;
                    one : Bytes;
                   two: Bytes;
                   three : Bytes;
                   four: Bytes;
                   five: Bytes) return DW_Block512;


  function Count_Characters(Input : String; Target : Character) return Integer;

   type Catena_KDF is new KDF.KDF_Scheme with
      record
        -- always 1 for Key Derivation
        D : Integer := 1;
        -- g range, 17 default for BRG
        G_Low : Integer := 17;
        G_High : Integer := 17;
        -- Structure
        Structure : String(1..100) := ('r', 'g', 'g', others=>' ');
        Structure_Length : Integer := 3;
        -- derived key Length in byte, 64 by default
        Hash_Length : Integer := 64;
        -- used compression function
        Used_Compression : Compression_ID := Blake2b1;
        -- used hash function
        Used_Hashfunction : Hashfunction_ID := Blake2b;
        -- used Mode
        Used_Mode : Mode_ID := Dragonfly;
        -- used Phi Layer
        Used_Phi : Phi_ID := Dummy;
        -- used Random Layer
        Used_Random : Random_ID := Gamma;

        -- -- Working variables
        -- State_Array_Handle_8k : State_Array_Access_8k;
        -- State_Array_Handle_512 : State_Array_Access_512;

        
        State_Handle : State_Array_Access_X;
        Hash_Fast_Handle : Hash_Fast_Access;

      end record;

-------------------------------------------------------

    package Graph is
   type Graph_Scheme is abstract tagged null record;
    procedure Process(This     : in out Graph_Scheme;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes) is abstract;
   function Index(  This       : in out Graph_Scheme;
                    Index      : in Natural;
                    Garlic     : in Natural) return Integer is abstract;

   function Memory( This       : in out Graph_Scheme;
                    Garlic     : in Natural) return Integer is abstract;

  end Graph;
-----------------------------------------

  package Graph_BRG is

   type BRG_Graph is new Graph.Graph_Scheme with
      record
        currentL        : Natural := 0;
      end record;

    overriding
    procedure Process(This     : in out BRG_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes);

    overriding
   function Index(This       : in out BRG_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;

    overriding
   function Memory( This       : in out BRG_Graph;
                    Garlic     : in Natural) return Integer;

    pragma Inline(Index, Memory);
   end Graph_Brg;

------------------------------------------

   package Graph_DBG is

   type DBG_Graph is new Graph.Graph_Scheme with
      record
        co        : Natural := 0;
      end record;

    overriding
    procedure Process(This     : in out DBG_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes);

   overriding
   function Index(This       : in out DBG_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;

   overriding
   function Memory( This       : in out DBG_Graph;
                    Garlic     : in Natural) return Integer;

   end Graph_DBG;

  type graph_access is access Graph.Graph_Scheme'Class;


-----------------------------------------

  package Graph_GR2 is

   type GR2_Graph is new Graph.Graph_Scheme with null record;

    overriding
    procedure Process(This     : in out GR2_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes);

    overriding
   function Index(This       : in out GR2_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;

   function Phi(This       : in out GR2_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;



    overriding
   function Memory( This       : in out GR2_Graph;
                    Garlic     : in Natural) return Integer;

   end Graph_GR2;


-----------------------------------------

  package Graph_GR3 is

   type GR3_Graph is new Graph.Graph_Scheme with null record;

    overriding
    procedure Process(This     : in out GR3_Graph;
                     Context :      in out Catena_KDF;
                     Garlic   : in  Integer;
                     Hash     : out Bytes);

    overriding
   function Index(This       : in out GR3_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;

   function Phi(This       : in out GR3_Graph;
                   Index      : in Natural;
                   Garlic     : in Natural) return Integer;



    overriding
   function Memory( This       : in out GR3_Graph;
                    Garlic     : in Natural) return Integer;

   end Graph_GR3;

--------------------------------------


   package RandomLayer is
   type Random_Scheme is abstract tagged
      record
        Internal_Graph : graph_access;
      end record;

  procedure Process(This     : in out Random_Scheme;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Input     : in Bytes) is abstract;

  end RandomLayer;

-------------------------------------------
  package RandomLayer_Gamma is

   type Gamma_RandomLayer is new RandomLayer.Random_Scheme with null record;

    overriding
    procedure Process(This     : in out Gamma_RandomLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Input     : in Bytes);

   end RandomLayer_Gamma;

  type Random_access is access RandomLayer.Random_Scheme'Class;


--------------------------------------
   package PhiLayer is
   type Phi_Scheme is abstract tagged
      record
        Internal_Graph : graph_access;
      end record;

  procedure Process(This     : in out Phi_Scheme;
                    Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes) is abstract;
  end PhiLayer;

-------------------------------------------
  package PhiLayer_Dummy is

   type Dummy_PhiLayer is new PhiLayer.Phi_Scheme with null record;

    overriding
    procedure Process(This     : in out Dummy_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes);


   end PhiLayer_Dummy;

-------------------------------------------
  package PhiLayer_LSB is

   type LSB_PhiLayer is new PhiLayer.Phi_Scheme with null record;

    overriding
    procedure Process(This     : in out LSB_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes);


    function Random(Context  : in out Catena_KDF;
                    Input    : in Bytes;
                    Garlic   : in Integer) return Integer;

   end PhiLayer_LSB;

   -------------------------------------------
  package PhiLayer_XS is

   type XS_PhiLayer is new PhiLayer.Phi_Scheme with null record;


    overriding
    procedure Process(This     : in out XS_PhiLayer;
                     Context  : in out Catena_KDF;
                     Garlic   : in Integer;
                     Hash     : out Bytes);

   end PhiLayer_XS;

---------------------------------------------

  type Phi_access is access PhiLayer.Phi_Scheme'Class;


pragma Optimize(Time);

end Crypto.Symmetric.KDF_Catena;

