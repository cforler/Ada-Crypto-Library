package body Crypto.Symmetric.Tweakable_Blockcipher_TX is

   use Crypto.Types;

   procedure Key_Setup(This : in out TX;
                       Key  : in     BC.Key_Type) is
   begin
      This.Key := Key;
   end Key_Setup;

   function Encrypt(This       : in out TX;
                    Tweak      : in     BC.Block;
                    Plaintext  : in     BC.Block) return BC.Block is
      Key: Bytes := To_Bytes(This.Key);
      Ciphertext: BC.Block;
      T: constant Bytes := BC.To_Bytes(Tweak);
   begin
      Key(T'Last-(T'Length-1)..T'Last) := Key(T'Last-(T'Length-1)..T'Last) xor T;
      BC.Prepare_Key(BC.To_Key(Key));  -- set new Key
      BC.Encrypt(Plaintext, Ciphertext);

      return Ciphertext;
   end Encrypt;

   function Decrypt(This       : in out TX;
                    Tweak      : in     BC.Block;
                    Ciphertext : in     BC.Block) return BC.Block is
      Key: Bytes := To_Bytes(This.Key);
      Plaintext: BC.Block;
      T: constant Bytes := BC.To_Bytes(Tweak);
   begin
      Key(T'Last-(T'Length-1)..T'Last) := Key(T'Last-(T'Length-1)..T'Last) xor T;
      BC.Prepare_Key(BC.To_Key(Key));  -- set new Key
      BC.Decrypt(Ciphertext, Plaintext);

      return Plaintext;
   end Decrypt;

end Crypto.Symmetric.Tweakable_Blockcipher_TX;