-- Mixed solution 1 concatenates a random number and a counter.
-- Every time a nonce is requested, this generator increment a counter by one and generates a new random value.
-- Upon reset, the counter is set to the initial value and a new random value will be generated.
with Ada.Direct_IO;

generic
   Counter_Size: Positive := Block'Size / 16; -- set the size of the counter (in bytes)
   with function To_Block_Type(Byte_Array: in Crypto.Types.Bytes) return Crypto.Types.Nonces.Block;
   with function To_Bytes(Block: in Crypto.Types.Nonces.Block) return Crypto.Types.Bytes;

package Crypto.Types.Nonces.Nonces_Mixed_1 is

   package N renames Crypto.Types.Nonces;
   package ADIO is new Ada.Direct_IO(N.Block);

   type Nonce_Mixed_1 is new N.Nonce with private;

   procedure Initialize(This  : in out Nonce_Mixed_1;
                        IV    : in     N.Block);

   -- The function Update calls the function Inc, generate a random value,
   -- and returns the concatenation random || counter (default: half half) as Block.
   overriding
   function Update(This : in out Nonce_Mixed_1) return N.Block;

private

   -- The function Inc increments the counter by one and
   -- stores the counter in the second half (default) of the Block.
   function Inc(B: in Block) return Block;

   type Nonce_Mixed_1 is new N.Nonce with null record;

   CS: Positive := (Block'Size/8) - Counter_Size;
   End_of_Block: Positive := (Block'Size / 8)-1;

end Crypto.Types.Nonces.Nonces_Mixed_1;