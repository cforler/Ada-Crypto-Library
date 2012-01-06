with Ada.IO_Exceptions;

package body Crypto.Types.Nonces.Nonces_Ctr is

   function Update(This : in out Nonce_Ctr) return N.Block is
   begin
      This.Mutex.Seize;
      This.Value := Inc(This.Value);
      ADIO.Write(This.File,This.Value);
      ADIO.Set_Index(This.File,1);
      This.Mutex.Release;
      return This.Value;
   end Update;

-------------------------------------------------------------------------------

   procedure Aux_Init(This : in out Nonce_Ctr; IV: in N.Block) is
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

   procedure Initialize(This : in out Nonce_Ctr; File_Path : in String) is
   begin
      ADIO.Open(This.File, ADIO.Inout_File, File_Path);
      ADIO.Read(This.File, This.Value);
      ADIO.Set_Index(This.File,1);
   end Initialize;

-------------------------------------------------------------------------------

   procedure Initialize(This : in out Nonce_Ctr; File_Path : in  String; IV: in N.Block) is
   begin
      ADIO.Open(This.File, ADIO.Inout_File, File_Path);
      Aux_Init(This, IV);
   exception
      when Ada.IO_Exceptions.Status_Error |  Ada.IO_Exceptions.Name_Error  =>
         ADIO.Create(This.File, ADIO.Inout_File, File_Path);
         Aux_Init(This, IV );
   end Initialize;

--------------------------------------------------------------------------------

   procedure Finalize(This : in out Nonce_Ctr) is
   begin
      if ADIO.Is_Open(This.File) then
         ADIO.Close(This.File);
      end if;
   end Finalize;

--------------------------------------------------------------------------------

end Crypto.Types.Nonces.Nonces_Ctr;