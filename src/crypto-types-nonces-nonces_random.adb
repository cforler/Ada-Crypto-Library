with Crypto.Random;

package body Crypto.Types.Nonces.Nonces_Random is

   function Update(This: in out Nonce_Rand) return N.Block is
      Byte_Array: Crypto.Types.Bytes(0..(Block'Size / 8)-1);
   begin
      This.Mutex.Seize;
      Crypto.Random.Read(Byte_Array);
      This.Value := To_Block_Type(Byte_Array);
      This.Mutex.Release;
      return This.Value;
   end Update;

end Crypto.Types.Nonces.Nonces_Random;