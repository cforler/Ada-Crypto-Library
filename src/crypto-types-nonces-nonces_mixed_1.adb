with Crypto.Types;
with Crypto.Random;

package body Crypto.Types.Nonces.Nonces_Mixed_1 is

   package CT renames Crypto.Types;

   -------------------------------------------------------------------------------

   function Inc(B: in Block) return Block is
      Result   : CT.Bytes := To_Bytes(B);
      Counter  : CT.Bytes := Result(CS..End_of_Block);
      use CT;
   begin
      Counter := Counter + 1;
      Result(CS..End_of_Block) := Counter;

      return To_Block_Type(Result);
   end Inc;

   -------------------------------------------------------------------------------

   function Update(This : in out Nonce_Mixed_1) return N.Block is

      Rand_Array    : Crypto.Types.Bytes(0..CS-1);
      Counter_Array : Crypto.Types.Bytes(0..End_of_Block);
      Result_Array  : Crypto.Types.Bytes(0..End_of_Block);

      Counter: Block;
   begin
      This.Mutex.Seize;

      -- increment counter and store in byte array
      Counter := Inc(This.Value);
      Counter_Array := To_Bytes(Counter);

      -- get random numbers
      Crypto.Random.Read(Rand_Array);

      -- fill result array (half counter and half random is default)
      Result_Array (0..CS-1)           := Rand_Array;
      Result_Array (CS..End_of_Block)  := Counter_Array(CS..End_of_Block);

      This.Value := To_Block_Type(Result_Array);
      This.Mutex.Release;

      return This.Value;
   end Update;

   -------------------------------------------------------------------------------

   procedure Initialize(This  : in out Nonce_Mixed_1;
                        IV    : in     N.Block) is
   begin
      This.Value := IV;
   end Initialize;

end Crypto.Types.Nonces.Nonces_Mixed_1;