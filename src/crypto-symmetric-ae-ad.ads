generic

package Crypto.Symmetric.AE.AD is

   type AEAD_Scheme is limited interface;

   procedure Encrypt(This             : in out AEAD_Scheme;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is abstract;

   -- Keep in mind that if you're using Decrypt_And_Verify with Read_Ciphertext_Again
   -- as default, the whole Ciphertext will be stored in main memory.

   -- Keep in mind that if you're using Decrypt_And_Verify without specifying a
   -- Callback_Reader for Read_Ciphertext_Again, the whole Ciphertext will be stored
   -- in Main Memory.
   function Decrypt_And_Verify(This                   : in out AEAD_Scheme;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is abstract;

end Crypto.Symmetric.AE.AD;