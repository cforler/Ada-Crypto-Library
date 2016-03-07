with Crypto.Types;

generic
   with function To_Block_Type(B: in Crypto.Types.Bytes) return Crypto.Types.Nonces.Block;
   Rand_Source : String := "";

package Crypto.Types.Nonces.Nonces_Random is
   package N renames Crypto.Types.Nonces;

   type Nonce_Rand is new N.Nonce with private;

   overriding
   function Update(This: in out Nonce_Rand) return N.Block;

private
   type Nonce_Rand is new N.Nonce with null record;

end Crypto.Types.Nonces.Nonces_Random;
