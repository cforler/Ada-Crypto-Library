package body Crypto.Symmetric.Tweakable_Blockcipher_CMT is
   
   -- Supress the following warning, since for CMT "This" is an empty record.
   -- warning: formal parameter "This" is not Referenced
   pragma Warnings("F");
   

	 
   procedure Key_Setup(This : in out CMT;
                       Key  : in     BC.Key_Type) is
   begin
      BC.Prepare_Key(Key);
   end Key_Setup;

   function Encrypt(This       : in out CMT;
                    Tweak      : in     BC.Block;
                    Plaintext  : in     BC.Block) return BC.Block is
      Ciphertext, C: BC.Block;
   begin
      BC.Encrypt(Plaintext, C);
      C := C xor Tweak;
      BC.Encrypt(C, Ciphertext);

      return Ciphertext;
   end Encrypt;

   function Decrypt(This       : in out CMT;
                    Tweak      : in     BC.Block;
                    Ciphertext : in     BC.Block) return BC.Block is
      Plaintext, P: BC.Block;
   begin
      BC.Decrypt(Ciphertext, P);
      P := P xor Tweak;
      BC.Decrypt(P, Plaintext);

      return Plaintext;
   end Decrypt;  
   pragma Warnings("f");
   
end Crypto.Symmetric.Tweakable_Blockcipher_CMT;
