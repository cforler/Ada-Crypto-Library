with Ada.Finalization;
private with Crypto.Types.Mutexes;

generic
   type Block is private;

package Crypto.Types.Nonces is
   package Fin renames Ada.Finalization;

   type Nonce is abstract limited new Fin.Limited_Controlled with private;

   overriding
   procedure Finalize(This : in out Nonce) is null;

   not overriding
   function Update(This : in out Nonce) return Block is abstract;

private
   type Nonce is abstract new Fin.Limited_Controlled with
      record
         Value : Block;
         Mutex : Crypto.Types.Mutexes.Mutex_Type;
      end record;
end Crypto.Types.Nonces;