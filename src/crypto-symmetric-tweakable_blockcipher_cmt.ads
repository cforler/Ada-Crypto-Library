with Crypto.Symmetric.Blockcipher;
with Crypto.Symmetric.Tweakable_Blockcipher;

generic
   with package BC is new Crypto.Symmetric.Blockcipher(<>);
   with function "xor"(Left, Right: BC.Block) return BC.Block is <>;

package Crypto.Symmetric.Tweakable_Blockcipher_CMT is

   package Tweakable_Blockciphers is new Crypto.Symmetric.Tweakable_Blockcipher(Block      => BC.Block,
                                                                                 Key_Type   => BC.Key_Type,
                                                                                 Tweak_Type => BC.Block);

   --type CMT is new Tweakable_Blockciphers.TB_Interface with null record;
   type CMT is new is limited inteface;
   
   overriding
   procedure Key_Setup(This : in out CMT;
                       Key  : in     BC.Key_Type);

   overriding
   function Encrypt(This       : in out CMT;
                    Tweak      : in     BC.Block;
                    Plaintext  : in     BC.Block) return BC.Block;

   overriding
   function Decrypt(This       : in out CMT;
                    Tweak      : in     BC.Block;
                    Ciphertext : in     BC.Block) return BC.Block;

end Crypto.Symmetric.Tweakable_Blockcipher_CMT;
