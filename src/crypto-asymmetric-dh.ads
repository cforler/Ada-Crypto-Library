with Crypto.Types;
with Crypto.Types.Big_Numbers;

use Crypto.Types;

generic
   Size : Positive;

package Crypto.Asymmetric.DH is

   package Big is new Crypto.Types.Big_Numbers(Size);
   use Big;

   subtype DH_Number is Bytes(0 .. Size/8 - 1);
   type DH_Parameters is private;

   procedure Gen_DH_Parameters(Params : out DH_Parameters);

   procedure Get_Private_Key(Private_Key : out DH_Number;
                             Params      : in  DH_Parameters);

   function Check_Private_Key(Private_Key : in DH_Number;
                              Params      : in DH_Parameters) return Boolean;

   procedure Get_Public_Key(Private_Key : in  DH_Number;
                            Params      : in  DH_Parameters;
                            Public_Key  : out DH_Number);

   function Check_Public_Key(Private_Key : in DH_Number;
                             Params      : in DH_Parameters) return Boolean;

   procedure Get_Shared_Secret(Private_Key : in  DH_Number;
                               Public_Key  : in  DH_Number;
                               Params      : in  DH_Parameters;
                               Secret      : out DH_Number);

   procedure Set_DH_Parameters(G      : in  DH_Number;
                               P      : in  DH_Number;
                               Params : out DH_Parameters);

   procedure Get_DH_Parameters(Params : in  DH_Parameters;
                               G      : out DH_Number;
                               P      : out DH_Number);

   procedure Set_DH_Parameters(G      : in  Big_Unsigned;
                               P      : in  Big_Unsigned;
                               Params : out DH_Parameters);

   procedure Get_DH_Parameters(Params : in  DH_Parameters;
                               G      : out Big_Unsigned;
                               P      : out Big_Unsigned);

private
   type DH_Parameters is
      record
         G : Big_Unsigned;
         P : Big_Unsigned;
      end record;

end Crypto.Asymmetric.DH;