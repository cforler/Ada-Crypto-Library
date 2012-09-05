with Crypto.Symmetric.Oneway_Blockcipher;

generic
   with package C is new Crypto.Symmetric.Oneway_Blockcipher(<>);
   with function Shift_Left (Value: C.Block; Amount: Natural) return C.Block;
   with function "xor" (Left, Right : C.Block)    return C.Block is <>;

package Crypto.Symmetric.MAC.CMAC is
   use C;

   type Blocks  is array (Integer range <>) of Block;

   -- low level
   procedure Init(Key : in Key_Type);

   procedure Sign(Message_Block : in Block);
   procedure Final_Sign(Final_Message_Block : in  Block;
                        Bytes_Read          : in  Natural;
                        Tag                 : out Block);


   procedure Verify(Message_Block : in Block);
   function Final_Verify(Final_Message_Block : in Block;
                         Bytes_Read          : in Natural;
                         Tag                 : in Block)
                         return Boolean;


   -- high level API
   procedure Sign(Message : in  Blocks;
                  Key     : in  Key_Type;
                  Tag     : out Block);


   function Verify(Message : in Blocks;
                   Key     : in Key_Type;
                   Tag     : in Block) return Boolean;


private
   Blocklength_Not_Supported : exception;

   -- Function Generate_Constants calculates the two constants U and U2 (see paper)
   -- in addiction to the blocklength. Both constants are generated from L.
   function Generate_Constants(Value : Bytes) return Block;

   pragma Inline (Init, Sign);

   pragma Optimize (Time);


end Crypto.Symmetric.MAC.CMAC;
