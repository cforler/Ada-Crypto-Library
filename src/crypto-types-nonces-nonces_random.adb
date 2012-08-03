with Crypto.Types.Random;

package body Crypto.Types.Nonces.Nonces_Random is
   
   function Update(This: in out Nonce_Rand) return N.Block is
      Byte_Array: Crypto.Types.Bytes(0..(Block'Size / 8)-1);
      pragma Unreferenced (This);   
   begin     
      Crypto.Types.Random.Read(Byte_Array);
      return To_Block_Type(Byte_Array);
   end Update;

end Crypto.Types.Nonces.Nonces_Random;
