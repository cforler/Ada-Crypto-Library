with Crypto.Types.Nonces;
with Ada.Direct_IO;

generic
   with function Inc(B : in Crypto.Types.Nonces.Block) return Crypto.Types.Nonces.Block;

package Crypto.Types.Nonces.Nonces_Ctr is
   package N renames Crypto.Types.Nonces;
   package ADIO is new Ada.Direct_IO(N.Block);

   type Nonce_Ctr is new N.Nonce with private;

   overriding
   function Update(This : in out Nonce_Ctr) return N.Block;

   overriding
   procedure Finalize(This : in out Nonce_Ctr);

   procedure Initialize(This : in out Nonce_Ctr;
                        File_Path  : in String;
                        IV: in N.Block);

   procedure Initialize(This : in out Nonce_Ctr; File_Path  : in String);

private
   type Nonce_Ctr is new N.Nonce with
      record
         File   : ADIO.File_Type;
      end record;

end Crypto.Types.Nonces.Nonces_Ctr;