with Crypto.Types.Random;

package body Crypto.Types.Nonces.Nonces_Mixed_2 is
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

   function Update(This : in out Nonce_Mixed_2) return N.Block is
      Return_Value  : Block;
   begin
      This.Mutex.Seize;
      This.Value := Inc(This.Value);

      declare
         Val: constant Crypto.Types.Bytes := To_Bytes(This.Value);
      begin
         if Val(CS..End_of_Block) = This.IV then
            Set_Random(This.Value);
         end if;
      end;
      Return_Value := This.Value;
      This.Mutex.Release;
      return This.Value;
   end Update;

   -------------------------------------------------------------------------------

   procedure Set_Random(B: in out N.Block) is
      Rand_Array    :            Crypto.Types.Bytes(0..CS-1);
      Counter_Array : constant   Crypto.Types.Bytes(0..End_of_Block) := To_Bytes(B);
      Result_Array  :            Crypto.Types.Bytes(0..End_of_Block);

   begin
      Crypto.Types.Random.Read(Rand_Array);

      Result_Array (0..CS-1)                 := Rand_Array;
      Result_Array (CS..End_of_Block)  := Counter_Array(CS..End_of_Block);

      B := To_Block_Type(Result_Array);
   end Set_Random;

   -------------------------------------------------------------------------------

   procedure Initialize(This  : in out Nonce_Mixed_2;
                        IV    : in     N.Block) is
   begin
      This.IV := To_Bytes(IV)(CS..End_of_Block);
      This.Value := IV;
      Set_Random(This.Value);
   end Initialize;

end Crypto.Types.Nonces.Nonces_Mixed_2;
