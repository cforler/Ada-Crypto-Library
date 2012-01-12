generic
   type T is mod <>;
   type T_A is array (Integer range <>) of T;
   
package Crypto.Types_Generic_Mod_Aux is
   function "xor"(Left, Right : T_A)      return T_A;
   function "xor"(Left : T_A; Right  : T) return T_A;
   function "xor"(Left : T; Right : T_A)  return T_A;
   
   function "+"(Left : T_A; Right : T) return T_A;
   function "+"(Left : T; Right : T_A) return T_A;
   
   function Is_Zero(Item : T_A) return Boolean;
   
   function Left_Part (Block : in T_A) return T_A;
   function Right_Part(Block : in T_A) return T_A;
end Crypto.Types_Generic_Mod_Aux;
   
