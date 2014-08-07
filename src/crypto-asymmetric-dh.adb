package body Crypto.Asymmetric.DH is

   use Big.Utils;
   use Big.Mod_Utils;

   function To_DH_Number(X : Big_Unsigned) return DH_Number is
      R : DH_Number := (others=> 0);
      N : constant Integer := Length_In_Bytes(X)-1;
   begin
      if N > 0 then
         R(R'Last-N..R'Last) := To_Bytes(X);
      end if;

      return R;
   end To_DH_Number;

   ------------------------------------------------------------

   procedure Gen_DH_Parameters(Params : out DH_Parameters) is
   begin
      null;
   end Gen_DH_Parameters;

   ------------------------------------------------------------

   procedure Get_Private_Key(Private_Key : out DH_Number;
                             Params      : in  DH_Parameters) is
      -- Generate x with 1 <= x <= p-1
      Key : Big_Unsigned;
   begin
      loop
         Key := Get_Random(Params.P - Big_Unsigned_One);
         exit when Key > Big_Unsigned_Zero;
      end loop;

      Private_Key := To_Bytes(Key);
   end Get_Private_Key;

   ------------------------------------------------------------

   function Check_Private_Key(Private_Key : in DH_Number;
                              Params      : in DH_Parameters) return Boolean is
      -- 1 <= x <= p-1
      X : constant Big_Unsigned := To_Big_Unsigned(Private_Key);
   begin
      if X < 1 or X > Params.P - 1 then
         return False;
      else
         return True;
      end if;

   end Check_Private_Key;

   ------------------------------------------------------------

   function Check_Public_Key(Private_Key : in DH_Number;
                             Params      : in DH_Parameters) return Boolean is
      -- 2 <= x <= p-1
      -- See Section 2.5.1 of [RFC2631]
      X : constant Big_Unsigned := To_Big_Unsigned(Private_Key);
   begin
      if X < 2 or X > Params.P-1 then
         return False;
      end if;

      if Pow(X, Params.G, Params.P) /= 1 then
         return False;
      end if;

      return True;
   end Check_Public_Key;

   ------------------------------------------------------------

   procedure Get_Public_Key(Private_Key : in  DH_Number;
                            Params      : in  DH_Parameters;
                            Public_Key  : out DH_Number) is
      K : Big_Unsigned;
   begin
      K := Pow(Params.G, To_Big_Unsigned(Private_Key), Params.P);
      Public_Key := To_DH_Number(K);
   end Get_Public_Key;

   ------------------------------------------------------------

   procedure Get_Shared_Secret(Private_Key : in  DH_Number;
                               Public_Key  : in  DH_Number;
                               Params      : in  DH_Parameters;
                               Secret      : out DH_Number) is
      K : Big_Unsigned;
   begin
      if not Check_Public_Key(Public_Key, Params) then
         raise Invalid_Public_Key_Error;
      end if;

      K := Pow(To_Big_Unsigned(Public_Key), To_Big_Unsigned(Private_Key), Params.P);
      Secret := To_DH_Number(K);
   end Get_Shared_Secret;

   ------------------------------------------------------------

   procedure Set_DH_Parameters(G      : in  DH_Number;
                               P      : in  DH_Number;
                               Params : out DH_Parameters) is
   begin
      Params.G := To_Big_Unsigned(G);
      Params.P := To_Big_Unsigned(P);
   end Set_DH_Parameters;

   ------------------------------------------------------------

   procedure Get_DH_Parameters(Params : in  DH_Parameters;
                               G      : out DH_Number;
                               P      : out DH_Number) is
   begin
      G := To_DH_Number(Params.G);
      P := To_DH_Number(Params.P);
   end Get_DH_Parameters;

   ------------------------------------------------------------

   procedure Set_DH_Parameters(G      : in  Big_Unsigned;
                               P      : in  Big_Unsigned;
                               Params : out DH_Parameters) is
   begin
      Params.G := G;
      Params.P := P;
   end Set_DH_Parameters;

   ------------------------------------------------------------

   procedure Get_DH_Parameters(Params : in  DH_Parameters;
                               G      : out Big_Unsigned;
                               P      : out Big_Unsigned) is
   begin
      G := Params.G;
      P := Params.P;
   end Get_DH_Parameters;

end Crypto.Asymmetric.DH;