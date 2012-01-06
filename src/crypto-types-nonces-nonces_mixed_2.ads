-- Mixed solution 2 concatenates a random value and a counter.
-- Every time a nonce is requested, this generator increment a counter and the random number value remains unchanged.
-- Upon reset, the counter is set to the initial value and a new random value will be generated.
with Ada.Direct_IO;

generic
   Counter_Size: Positive := Block'Size / 16; -- set the size of the counter (in bytes)
   with function To_Block_Type(Byte_Array: in Crypto.Types.Bytes) return Crypto.Types.Nonces.Block;
   with function To_Bytes(Block: in Crypto.Types.Nonces.Block) return Crypto.Types.Bytes;

package Crypto.Types.Nonces.Nonces_Mixed_2 is

   package N renames Crypto.Types.Nonces;
   package ADIO is new Ada.Direct_IO(N.Block);

   type Nonce_Mixed_2 is new N.Nonce with private;

   procedure Initialize(This  : in out Nonce_Mixed_2;
                        IV    : in     N.Block);

   -- The function Update calls the function Inc.
   overriding
   function Update(This : in out Nonce_Mixed_2) return N.Block;

private

   -- The function Inc increments the counter by one and
   -- stores the counter in the second half (default) of the Block.
   function Inc(B: in Block) return Block;

   -- The function Set_Random generates a new random value and
   -- store it in the first half (default) of the Block.
   procedure Set_Random(B: in out N.Block);

   CS: Positive := (Block'Size/8) - Counter_Size;
   End_of_Block: Positive := (Block'Size / 8)-1;

   type Nonce_Mixed_2 is new N.Nonce with
      record
         IV: Crypto.Types.Bytes(CS..End_of_Block);
      end record;

end Crypto.Types.Nonces.Nonces_Mixed_2;