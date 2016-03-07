with Crypto.Types.Random;
with Crypto.Types.Random_Source.File;

package body Crypto.Types.Nonces.Nonces_Random is
   
   function Update(This: in out Nonce_Rand) return N.Block is
      Byte_Array: Crypto.Types.Bytes(0..(Block'Size / 8)-1);
      pragma Unreferenced (This);   
      Rand_Source_File : Crypto.Types.Random_Source.File.Random_Source_File;
   begin     
      if Rand_Source /= "" then
         Crypto.Types.Random_Source.File.Initialize(
            Rand_Source_File,
            Rand_Source
         );
         Crypto.Types.Random.Set(Rand_Source_File);
      end if;
      Crypto.Types.Random.Read(Byte_Array);
      return To_Block_Type(Byte_Array);
   end Update;

end Crypto.Types.Nonces.Nonces_Random;
