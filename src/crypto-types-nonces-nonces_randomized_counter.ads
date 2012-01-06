-- The randomized counter generator concatenates a random number and a counter.
-- Every time a nonce is requested, this generator increment a counter by one and generates a new random value.
-- Upon reset, the counter coninues from the last position and a new random value will be generated.
with Ada.Direct_IO;

generic
   Counter_Size: Positive := Block'Size / 16;
   with function To_Block_Type(Byte_Array: in Crypto.Types.Bytes) return Crypto.Types.Nonces.Block;
   with function To_Bytes(Block: in Crypto.Types.Nonces.Block) return Crypto.Types.Bytes;

package Crypto.Types.Nonces.Nonces_Randomized_Counter is

   package N renames Crypto.Types.Nonces;
   package ADIO is new Ada.Direct_IO(N.Block);

   type Nonce_RC is new N.Nonce with private;

   procedure Initialize(This        : in out Nonce_RC;
                        File_Path   : in String;
                        IV          : in N.Block);

   procedure Initialize(This : in out Nonce_RC; File_Path  : in String);

   -- The function Update calls the function Inc, generate a random value,
   -- and returns the concatenation random || counter (default: half half) as Block.
   -- The Block is also stored in the File (to remember the last used nonce).
   overriding
   function Update(This : in out Nonce_RC) return N.Block;

   overriding
   procedure Finalize(This : in out Nonce_RC);

private

   -- The function Inc increments the counter by one and
   -- stores the counter in the second half (default) of the Block.
   function Inc(B: in Block) return Block;

   type Nonce_RC is new N.Nonce with
      record
         File: ADIO.File_Type;
      end record;

   CS: Positive := (Block'Size/8) - Counter_Size;

end Crypto.Types.Nonces.Nonces_Randomized_Counter;