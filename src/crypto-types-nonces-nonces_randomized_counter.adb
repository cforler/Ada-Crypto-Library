with Crypto.Types;
with Crypto.Types.Random;
with Ada.IO_Exceptions;

package body Crypto.Types.Nonces.Nonces_Randomized_Counter is

   package CT renames Crypto.Types;

   -------------------------------------------------------------------------------

   function Inc(B: in Block) return Block is
      Result   : CT.Bytes := To_Bytes(B);
      Counter  : CT.Bytes := Result(CS..(Block'Size / 8)-1);
      use CT;
   begin
      Counter := Counter + 1;
      Result(CS..(Block'Size / 8)-1) := Counter;

      return To_Block_Type(Result);
   end Inc;

   -------------------------------------------------------------------------------

   function Update(This : in out Nonce_RC) return N.Block is

      Rand_Array    : Crypto.Types.Bytes(0..CS-1);
      Counter_Array : Crypto.Types.Bytes(0..(Block'Size / 8)-1);
      Result_Array  : Crypto.Types.Bytes(0..(Block'Size / 8)-1);

      Counter: Block;
   begin
      This.Mutex.Seize;

      -- increment counter and store in byte array
      Counter := Inc(This.Value);
      Counter_Array := To_Bytes(Counter);

      -- get random numbers
      Crypto.Types.Random.Read(Rand_Array);

      -- fill result array (half counter, half random)
      Result_Array (0..CS-1)                 := Rand_Array;
      Result_Array (CS..(Block'Size / 8)-1)  := Counter_Array(CS..(Block'Size / 8)-1);

      This.Value := To_Block_Type(Result_Array);
      ADIO.Write(This.File, This.Value);
      ADIO.Set_Index(This.File,1);
      This.Mutex.Release;

      return This.Value;
   end Update;

   -------------------------------------------------------------------------------

   procedure Aux_Init(This : in out Nonce_RC; IV: in N.Block) is
   begin
      if ADIO.End_Of_File (This.File) then
         This.Value := IV;
         ADIO.Write(This.File,This.Value);
      else
         ADIO.Read(This.File, This.Value);
      end if;
      ADIO.Set_Index(This.File,1);
   end Aux_Init;

   -------------------------------------------------------------------------------

   procedure Initialize(This : in out Nonce_RC;
                        File_Path  : in String;
                        IV: in N.Block) is
   begin
      ADIO.Open(This.File, ADIO.Inout_File, File_Path);
      Aux_Init(This, IV);
   exception
      when Ada.IO_Exceptions.Status_Error |  Ada.IO_Exceptions.Name_Error  =>
         ADIO.Create(This.File, ADIO.Inout_File, File_Path);
         Aux_Init(This, IV);
   end Initialize;

   -------------------------------------------------------------------------------

   procedure Initialize(This : in out Nonce_RC; File_Path  : in String) is
   begin
      ADIO.Open(This.File, ADIO.Inout_File, File_Path);
      ADIO.Read(This.File, This.Value);
      ADIO.Set_Index(This.File,1);
   end Initialize;

   -------------------------------------------------------------------------------

   procedure Finalize(This : in out Nonce_RC) is
   begin
      if ADIO.Is_Open(This.File) then
         ADIO.Close(This.File);
      end if;
   end Finalize;

end Crypto.Types.Nonces.Nonces_Randomized_Counter;