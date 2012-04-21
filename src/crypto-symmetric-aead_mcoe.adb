-- This is a reference implementation of the Generic McOE Scheme,
-- which can be found at http://eprint.iacr.org/2011/644.pdf.
-- The annotations (described with step 1, step 2, ...) refers to the
-- algortihms on page 5 (with Tag-splitting) and 6 (without Tag-splitting)
-- in the paper.
with Ada.Containers.Indefinite_Vectors;

package body Crypto.Symmetric.AEAD_McOE is

   -- useful constants
   Bytes_Per_Block : constant Positive := Block'Size / 8;
   Zero_Bytes: constant Bytes(0..(Bytes_Per_Block - 1)) := (others => 0);
   Zero_Block: constant Block := To_Block_Type(Zero_Bytes);

   -- package initializations
   package Vectors is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                            Element_Type => Bytes);
   Masked_Plaintext: Vectors.Vector;

   -----------------------------------------------------------------
   ----
   ---- auxiliary functions and procedures
   ----
   -----------------------------------------------------------------

   -- This procedure takes out the first element of the Vector
   -- Masked_Plaintext and deletes it.
   procedure Read_Masked_Plaintext(B     : out Bytes;
                         Count : out Natural) is
      use Ada.Containers;
   begin
      if Masked_Plaintext.Length = 0 then
         Count := 0;
      else
         if Masked_Plaintext.First_Element'Length = Bytes_Per_Block then
            B := Masked_Plaintext.First_Element;
            Count := Bytes_Per_Block;
         else
            declare
               Temp: Bytes := Zero_Bytes;
            begin
               Temp(Temp'First..Masked_Plaintext.First_Element'Length-1) := Masked_Plaintext.First_Element;
               B := Temp;
               Count := Masked_Plaintext.First_Element'Length;
            end;
         end if;
         Masked_Plaintext.Delete_First;
      end if;
   end Read_Masked_Plaintext;

   -----------------------------------------------------------------

   -- This function converts the byte-length of a block in the
   -- binary representation.
   function Convert_Length_To_Block(N: in Natural) return Block is
      B: Bytes := Zero_Bytes;
      W: constant Word := Word(N);
      B_Word: constant Byte_Word := To_Byte_Word(W); -- subtype Byte_Word is Bytes (0 .. 3);
   begin
      B(B'Last-(B_Word'Length-1)..B'Last) := B_Word;
      return To_Block_Type(B);
   end Convert_Length_To_Block;

   -----------------------------------------------------------------

   -- This procedure decrypt and write each entire Ciphertext block.
   -- Note, that if the last Ciphertext block is smaller than Bytes_Per_Block,
   -- it won't be decrypted here, so its not a part of this procedure.
   -- This procedure won't be called, if the calculated Tag isn't the same as the specified.
   procedure Write_Decrypted_Plaintext(This                  : in out AEAD_McOE;
				       Old_Tweak             : in Block;
                                       Read_Masked_Plaintext : in Callback_Reader;
                                       Write_Plaintext       : in Callback_Writer) is
      Plaintext: Block;
      Prev_Block: Bytes := Zero_Bytes;
      Curr_Block: Bytes := Zero_Bytes;
      Bytes_Read: Natural;
      Tweak: Block := Old_Tweak;
   begin
      Read_Masked_Plaintext(Prev_Block, Bytes_Read);
      if Bytes_Read = Bytes_Per_Block then -- if |Message| = 0 there is nothing to write
         loop
            Read_Masked_Plaintext(Curr_Block, Bytes_Read);
            Plaintext := This.TB.Decrypt(Tweak, To_Block_Type(Prev_Block));
            Tweak := Plaintext xor To_Block_Type(Prev_Block);

            if Bytes_Read = Bytes_Per_Block then
               Write_Plaintext(To_Bytes(Plaintext));
               Prev_Block := Curr_Block;
            else
               -- If Bytes_Read = 0, then Prev_Block contains the entire Tag, so it may not be written.
               -- If Bytes_Read < Bytes_Per_Block, then Prev_Block contains the last bytes of the
               -- Ciphertext. In this case, the last Ciphertext bytes will be written after calling
               -- this procedure (in Decrypt_Auth_Split_Tag()).
               exit;
            end if;
         end loop;
      end if;
   end Write_Decrypted_Plaintext;

   -----------------------------------------------------------------

   function Aux_Header(This        : in out AEAD_McOE;
                       Read_Header : in     Callback_Reader) return Block is
      Bytes_Read: Natural;
      Curr_Block: Bytes := Zero_Bytes;
      Prev_Block: Bytes := Zero_Bytes;
      Tau: Block;
   begin
      Read_Header(Prev_Block, Bytes_Read);

      if Bytes_Read /= Bytes_Per_Block then
         Prev_Block(Bytes_Read..Prev_Block'Last) := (others => 0);
         Prev_Block(Bytes_Read) := 16#80#;
         Tau := This.TB.Encrypt(This.Tweak, To_Block_Type(Prev_Block)); -- tau <-- E_K(U, H_LH)
      else
         loop
            Read_Header(Curr_Block, Bytes_Read);

            if Bytes_Read = Bytes_Per_Block then
               This.Tweak := This.TB.Encrypt(This.Tweak, To_Block_Type(Prev_Block)) xor To_Block_Type(Prev_Block);
               Prev_Block := Curr_Block;
            elsif Bytes_Read = 0 then
               declare
                  Add_Block: constant Bytes(Zero_Bytes'Range) := (16#80#, others => 0);
               begin
                  This.Tweak := This.TB.Encrypt(This.Tweak, To_Block_Type(Add_Block)) xor To_Block_Type(Add_Block);
               end;
               Tau := This.TB.Encrypt(This.Tweak, To_Block_Type(Prev_Block)); -- tau <-- E_K(U, H_LH)
               exit;
            else
               Curr_Block(Bytes_Read..Curr_Block'Last) := (others => 0);
               Curr_Block(Bytes_Read) := 16#80#;
               Tau := This.TB.Encrypt(This.Tweak, To_Block_Type(Curr_Block)); -- tau <-- E_K(U, H_LH)
               Prev_Block := Curr_Block;
               exit;
            end if;
         end loop;

         This.Tweak := Tau xor To_Block_Type(Prev_Block); -- U <-- tau xor H_LH
      end if;

      return Tau;

   end Aux_Header;

   -----------------------------------------------------------------

   procedure Aux(This:      in out AEAD_McOE;
                 Plaintext: in     Bytes;
                 Write:     in     Callback_Writer) is
      Ciphertext: Block;
   begin
      Ciphertext := This.TB.Encrypt(This.Tweak, To_Block_Type(Plaintext));
      Write(To_Bytes(Ciphertext));
      This.Tweak := To_Block_Type(Plaintext) xor Ciphertext;
   end Aux;

   -----------------------------------------------------------------

   procedure Encrypt_Auth_Split_Tag(This         : in out AEAD_McOE;
                                    Last_P_Block : in     Bytes;
                                    Bytelen      : in     Natural;
                                    Tau          : in     Bytes;
                                    Write        : in     Callback_Writer) is
      M: Bytes := Zero_Bytes;
      One_Block: constant Bytes(Zero_Bytes'Range) := (others => 16#FF#);
      C: Block := Zero_Block;
      C2: Block := Zero_Block;
   begin
      -- step 6
      M(M'First..Bytelen-1) := Last_P_Block(Last_P_Block'First..Bytelen-1);
      M(Bytelen..M'Last)    := Tau(Tau'First..Bytes_Per_Block-Bytelen-1);
      -- step 7
      M := To_Bytes(This.TB.Encrypt(To_Block_Type(One_Block), Convert_Length_To_Block(Bytelen))) xor M;
      -- step 8
      C := This.TB.Encrypt(This.Tweak, To_Block_Type(M));
      -- step 10
      This.Tweak := To_Block_Type(M) xor C;
      -- step 11
      C2 := This.TB.Encrypt(This.Tweak, To_Block_Type(Tau));
      -- step 13: concatenate last Ciphertext block and tag
      Write(To_Bytes(C));                 -- write last Ciphertext bytes || first part of the tag (step 9)
      Write(To_Bytes(C2)(0..Bytelen-1));  -- second part (step 12)

   end Encrypt_Auth_Split_Tag;

   -----------------------------------------------------------------

   procedure Aux_Enc(This             : in out AEAD_McOE;
                     Tau              : in     Block;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is
      Bytes_Read: Natural;
      Curr_Block: Bytes := Zero_Bytes;
      Prev_Block: Bytes := Zero_Bytes;
   begin
      -- step 5
      Read_Plaintext(Prev_Block, Bytes_Read);

      if Bytes_Read = Bytes_Per_Block then
         loop
            Read_Plaintext(Curr_Block, Bytes_Read);
            if Bytes_Read = Bytes_Per_Block then
               Aux(This, Prev_Block, Write_Ciphertext);
               Prev_Block := Curr_Block;
            elsif Bytes_Read = 0 then  -- without tag-splitting
               Aux(This, Prev_Block, Write_Ciphertext);
               -- write Tag
               Write_Ciphertext(To_Bytes(This.TB.Encrypt(This.Tweak, Tau)));
               exit;
            else  -- with tag-splitting
               Aux(This, Prev_Block, Write_Ciphertext);
               -- step 6..13
               Encrypt_Auth_Split_Tag(This         => This,
                                      Last_P_Block => Curr_Block,
                                      Bytelen      => Bytes_Read,
                                      Tau          => To_Bytes(Tau),
                                      Write        => Write_Ciphertext);
               exit;
            end if;
         end loop;
      elsif Bytes_Read = 0 then  -- without tag-splitting
         -- goto step 6
         -- encrypt tau and write Tag
         Write_Ciphertext(To_Bytes(This.TB.Encrypt(This.Tweak, Tau)));
      else
         -- step 6..13
         Encrypt_Auth_Split_Tag(This         => This,
                                Last_P_Block => Prev_Block,
                                Bytelen      => Bytes_Read,
                                Tau          => To_Bytes(Tau),
                                Write        => Write_Ciphertext);
      end if;
   end Aux_Enc;

   -----------------------------------------------------------------

   procedure Decrypt_Auth_Split_Tag(This       : in out AEAD_McOE;
                                    Prev_Block : in     Bytes;
                                    Last_Block : in     Bytes;
                                    Bytelen    : in     Natural;
                                    Tau        : in     Bytes;
                                    Old_Tweak  : in     Block;
                                    Read       : in     Callback_Reader;
                                    Write      : in     Callback_Writer;
                                    Authentic  : out    Boolean) is

      One_Block: constant Bytes(Zero_Bytes'Range) := (others => 16#FF#);
      M: Block := Zero_Block;
      C: Bytes := Zero_Bytes;
      Tag: Bytes := Zero_Bytes;
      T: Block;
   begin
      declare
         Overlap: constant Natural := Bytes_Per_Block - Bytelen;
      begin
         -- extract the Tag
         Tag(Tag'First..Overlap-1) := Prev_Block(Bytelen..Prev_Block'Last);
         Tag(Overlap..Tag'Last)    := Last_Block(Last_Block'First..Bytelen-1);
      end;
      -- step 6
      C(C'First..Bytelen-1) := Prev_Block(Prev_Block'First..Bytelen-1);
      C(Bytelen..C'Last)    := Tag(Tag'First..(Bytes_Per_Block-Bytelen-1));
      -- step 7
      M := This.TB.Decrypt(This.Tweak, To_Block_Type(C));
      -- step 8
      This.Tweak := M xor To_Block_Type(C);
      -- step 9
      -- The last Plaintext bytes won't be written masked nor calculated again.
      M := This.TB.Encrypt(To_Block_Type(One_Block), Convert_Length_To_Block(Bytelen)) xor M;
      -- step 11
      T := This.TB.Encrypt(This.Tweak, To_Block_Type(Tau));
      -- step 12
      Authentic := To_Bytes(M)(Bytelen..Bytes_Per_Block-1) = Tau(0..Bytes_Per_Block-Bytelen-1)
        and To_Bytes(T)(0..Bytelen-1) = Tag(Bytes_Per_Block-Bytelen..Tag'Last);
      if Authentic then
         -- write unmasked Plaintext, except the last bytes of the Ciphertext
         Write_Decrypted_Plaintext(This                  => This,
				   Old_Tweak             => Old_Tweak,
                                   Read_Masked_Plaintext => Read,
                                   Write_Plaintext       => Write);
         -- Write the last bytes of the Ciphertext, which does not fill a whole block.
         -- This step eliminates the expense of applying step 8-9 again in
         -- the procedure Write_Decrypted_Plaintext.
         Write(To_Bytes(M)(0..Bytelen-1));
      end if;
   end Decrypt_Auth_Split_Tag;

   -----------------------------------------------------------------

   function Aux_Dec(This                   : in out AEAD_McOE;
                    Tau                    : in     Block;
                    Read_Ciphertext        : in     Callback_Reader;
                    Read_Ciphertext_Again  : in     Callback_Reader;
                    Write_Plaintext        : in     Callback_Writer;
                    Store_Internally       : in     Boolean)
                    return Boolean is

      Bytes_Read: Natural;
      Old_Tweak: constant Block := This.Tweak;
      Curr_Block: Bytes := Zero_Bytes;
      Prev_Block: Bytes := Zero_Bytes;
      Plaintext: Block;
      Verification_Bool: Boolean := False;
   begin
      -- step 5
      Read_Ciphertext(Prev_Block, Bytes_Read);
      if Bytes_Read /= Bytes_Per_Block then
         raise Invalid_Ciphertext_Error;
      else
         loop
            Read_Ciphertext(Curr_Block, Bytes_Read);
            if Bytes_Read = Bytes_Per_Block then
               Plaintext := This.TB.Decrypt(This.Tweak, To_Block_Type(Prev_Block));

               if Store_Internally then
	               -- write the Ciphertext block in vector
                  Masked_Plaintext.Append(Prev_Block);
               end if;
               This.Tweak := Plaintext xor To_Block_Type(Prev_Block);
               Prev_Block := Curr_Block;
            elsif Bytes_Read = 0 then -- without tag-splitting
               if Store_Internally then
                  -- write the Ciphertext block in vector
                  Masked_Plaintext.Append(Prev_Block);
               end if;

               if To_Block_Type(Prev_Block) = This.TB.Encrypt(This.Tweak, Tau) then
                  Write_Decrypted_Plaintext(This                  => This,
					    Old_Tweak             => Old_Tweak,
                                            Read_Masked_Plaintext => Read_Ciphertext_Again,
                                            Write_Plaintext       => Write_Plaintext);
                  Verification_Bool := True;
               end if;
               exit;
            else  -- with tag-splitting
               if Store_Internally then
                  -- write the last Ciphertext block in vector, which is included in Prev_Block,
                  -- and the entire Tag
                  Masked_Plaintext.Append(Prev_Block);
                  Masked_Plaintext.Append(Curr_Block(Curr_Block'First..Bytes_Read-1));
               end if;
               -- step 6..12
               Decrypt_Auth_Split_Tag(This       => This,
                                      Prev_Block => Prev_Block,
                                      Last_Block => Curr_Block,
                                      Bytelen    => Bytes_Read,
                                      Tau        => To_Bytes(Tau),
                                      Old_Tweak  => Old_Tweak,
                                      Read       => Read_Ciphertext_Again,
                                      Write      => Write_Plaintext,
                                      Authentic  => Verification_Bool);
               exit;
            end if;
         end loop;
      end if;

      return Verification_Bool;

   end Aux_Dec;

   -----------------------------------------------------------------
   ----
   ---- overriding functions and procedures
   ----
   -----------------------------------------------------------------

   procedure Init_Encrypt(This   : out    AEAD_McOE;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class) is
   begin
      This.TB.Key_Setup(Key);
      This.Nonce_Value := Nonce.Update;
      This.Tweak := This.TB.Encrypt(Zero_Block, This.Nonce_Value);
   end Init_Encrypt;

   -----------------------------------------------------------------

   procedure Init_Decrypt(This        : out AEAD_McOE;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block) is
   begin
      This.TB.Key_Setup(Key);
      This.Nonce_Value := Nonce_Value;
      This.Tweak := This.TB.Encrypt(Zero_Block, This.Nonce_Value);
   end Init_Decrypt;

   -----------------------------------------------------------------

   procedure Encrypt(This             : in out AEAD_McOE;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is

      -- step 2 not applicable (Nonce is the only AD)
      -- step 3 (applied in Init_Encrypt)
      Tau: constant Block := This.Tweak;
   begin
      -- step 4
      This.Tweak := This.Tweak xor This.Nonce_Value;

      Aux_Enc(This, Tau, Read_Plaintext, Write_Ciphertext);

   end Encrypt;

   -----------------------------------------------------------------

   procedure Encrypt(This             : in out AEAD_McOE;
                     Read_Header      : in     Callback_Reader;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is

      Tau: Block;
   begin
      -- step 2..4
      This.Tweak := This.Tweak xor This.Nonce_Value; -- to make up the xor operation
      Tau := Aux_Header(This, Read_Header);

      Aux_Enc(This, Tau, Read_Plaintext, Write_Ciphertext);
   end Encrypt;

   -----------------------------------------------------------------

   function Decrypt_And_Verify(This                   : in out AEAD_McOE;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is
      -- step 2 not applicable (Nonce is the only AD)
      -- step 3 (applied in Init_Decrypt)
      Tau: constant Block := This.Tweak;
   begin
      if Read_Ciphertext_Again = null then
         declare
            RCA: constant Callback_Reader := Read_Masked_Plaintext'Access;
         begin
            This.Tweak := This.Tweak xor This.Nonce_Value;

            return Aux_Dec(This, Tau, Read_Ciphertext, RCA, Write_Plaintext, True);
         end;
      else
         This.Tweak := This.Tweak xor This.Nonce_Value;

         return Aux_Dec(This, Tau, Read_Ciphertext, Read_Ciphertext_Again, Write_Plaintext, False);
      end if;
   end Decrypt_And_Verify;

   -----------------------------------------------------------------

   function Decrypt_And_Verify(This                   : in out AEAD_McOE;
                               Read_Header            : in     Callback_Reader;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is
   Tau: Block;
   begin
      if Read_Ciphertext_Again = null then
         declare
            RCA: constant Callback_Reader := Read_Masked_Plaintext'Access;
         begin
            -- step 2..4
            This.Tweak := This.Tweak xor This.Nonce_Value; -- to make up the xor operation
            Tau := Aux_Header(This, Read_Header);

            return Aux_Dec(This, Tau, Read_Ciphertext, RCA, Write_Plaintext, True);
         end;
      else
            -- step 2..4
         This.Tweak := This.Tweak xor This.Nonce_Value; -- to make up the xor operation
         Tau := Aux_Header(This, Read_Header);

         return Aux_Dec(This, Tau, Read_Ciphertext, Read_Ciphertext_Again, Write_Plaintext, False);
      end if;
   end Decrypt_And_Verify;

   -----------------------------------------------------------------
end Crypto.Symmetric.AEAD_McOE;
