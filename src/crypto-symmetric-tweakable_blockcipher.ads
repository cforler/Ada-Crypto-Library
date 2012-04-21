generic
   type Block is private;
   type Key_Type is private;
   type Tweak_Type is private;

package Crypto.Symmetric.Tweakable_Blockcipher is
   type TB_Interface is limited interface;

   procedure Key_Setup(This : in out TB_Interface;
                       Key  : in     Key_Type) is abstract;

   function Encrypt(This       : in out TB_Interface;
                    Tweak      : in     Tweak_Type;
                    Plaintext  : in     Block) return Block is abstract;

   function Decrypt(This       : in out TB_Interface;
                    Tweak      : in     Tweak_Type;
                    Ciphertext : in     Block) return Block is abstract;

end Crypto.Symmetric.Tweakable_Blockcipher;
