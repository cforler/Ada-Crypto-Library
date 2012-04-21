with Crypto.Types;
with Crypto.Symmetric.Blockcipher;
with Crypto.Symmetric.Tweakable_Blockcipher;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>);
   with function To_Bytes(K : in BC.Key_Type) return Crypto.Types.Bytes;

package Crypto.Symmetric.Tweakable_Blockcipher_TX is

   package Tweakable_Blockciphers is new Crypto.Symmetric.Tweakable_Blockcipher(Block      => BC.Block,
										Key_Type   => BC.Key_Type,
										Tweak_Type => BC.Block);

   type TX is new Tweakable_Blockciphers.TB_Interface with private;

   overriding
   procedure Key_Setup(This : in out TX;
                       Key  : in     BC.Key_Type);

   overriding
   function Encrypt(This       : in out TX;
                    Tweak      : in     BC.Block;
                    Plaintext  : in     BC.Block) return BC.Block;

   overriding
   function Decrypt(This       : in out TX;
                    Tweak      : in     BC.Block;
                    Ciphertext : in     BC.Block) return BC.Block;

private

   type TX is new Tweakable_Blockciphers.TB_Interface with record
      Key: BC.Key_Type;
   end record;

end Crypto.Symmetric.Tweakable_Blockcipher_TX;
