-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Ada.Containers.Indefinite_Vectors;

package body Crypto.Symmetric.AE_OCB is

   -- useful constants
   Zero_Bytes: constant Bytes(0..(Bytes_Per_Block - 1)) := (others => 0);
   Zero_Block: constant Block := To_Block_Type(Zero_Bytes);
   -- If Store_Internally = True, every Ciphertext block will be stored in the Vector Masked_Plaintext
   Store_Internally: Boolean := False;

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
   procedure Read_Masked_Plaintext(B : out Bytes;
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

   -- This procedure calculates all needed irreducible polynomials
   -- and stores them into an array called L_Array.
   procedure Generate_L(A: in out Block_Array) is
      Tmp, Prev_L: Bytes := Zero_Bytes;
   begin
      BC.Encrypt(Zero_Block, A(0)); -- L(0) := L
      -- generate L(-1)
      A(-1) := Shift_Right(A(0), 1);
      Tmp := To_Bytes(A(-1));
      Prev_L := To_Bytes(A(0));

      if (Prev_L(Prev_L'Last) and 16#01#) /= 0 then -- LSB of L = 1?
         if Bytes_Per_Block = 16 then -- message blocksize n = 128 bit
            Tmp(Tmp'Last) := Tmp(Tmp'Last) xor 16#43#;
         elsif Bytes_Per_Block = 8 then -- message blocksize n = 64 bit
            Tmp(Tmp'Last) := Tmp(Tmp'Last) xor 16#0d#;
         else
            raise Blocklength_Not_Supported;
         end if;
         Tmp(Tmp'First) := Tmp(Tmp'First) xor 16#80#;
         A(-1) := To_Block_Type(Tmp);
      end if;

      -- generate L(1..31);
      for I in 1..A'Last loop
         A(I) := Shift_Left(A(I-1), 1);
         Tmp := To_Bytes(A(I));
         Prev_L := To_Bytes(A(I-1));

         if (Prev_L(Prev_L'First) and 16#80#) /= 0 then -- MSB of Prev_L = 1?
            if Bytes_Per_Block = 16 then
               Tmp(Tmp'Last) := Tmp(Tmp'Last) xor 16#87#;
            elsif Bytes_Per_Block = 8 then
               Tmp(Tmp'Last) := Tmp(Tmp'Last) xor 16#1b#;
            else
               raise Blocklength_Not_Supported;
            end if;
            A(I) := To_Block_Type(Tmp);
         end if;
      end loop;

   end Generate_L;

   -----------------------------------------------------------------

   -- This function converts the byte-length of the last Ciphertext block
   -- in the binary representation and returns a Block_Type.
   function Convert_Length_To_Block(N: in Natural) return Block is
      B: Bytes := Zero_Bytes;
      W: constant Word := Word(N);
      B_Word: constant Byte_Word := To_Byte_Word(W); -- subtype Byte_Word is Bytes (0 .. 3);
   begin
      B(B'Last-(B_Word'Length-1)..B'Last) := B_Word;
      return To_Block_Type(B);
   end Convert_Length_To_Block;

   -----------------------------------------------------------------

   -- This procedure seperates the last Ciphertext block and the Tag (out of
   -- one or two blocks).
   procedure Extract(This       : in     AE_OCB;
                     A          : in     Bytes;
                     B          : in     Bytes := Zero_Bytes;
                     Bytes_Read : in     Natural;  -- Bytes_Read never zero when calling this procedure!
                     Bitlen     : in out Block;
                     Bytelen    : out    Natural;
                     Last_Block : in out Bytes;    -- initialized as Zero_Block
                     Tag        : in out Bytes;    -- initialited as Zero_Block
                     Two_Blocks : in     Boolean;
                     Dec        : out    Boolean) is
      Overlap: constant Integer := Bytes_Read - This.Taglen;
   begin
      Dec := False;
      if Two_Blocks then

         if Store_Internally then
            -- both blocks (A and B) must be append to Masked_Plaintext
            Masked_Plaintext.Append(A);
            Masked_Plaintext.Append(B);
         end if;
         -- A is Ciphertext and within B is the Tag, with T <= BPB
         if (Bytes_Read - This.Taglen) = 0 then
            Last_Block := A;
            Tag(Tag'First..This.Taglen-1) := B(B'First..This.Taglen-1);
            Bytelen := Bytes_Per_Block;
            Bitlen := Convert_Length_To_Block(8 * Bytelen);

         -- A contains a part of the Tag
         elsif (Bytes_Read - This.Taglen) < 0 then
            Last_Block(Last_Block'First..Bytes_Per_Block-abs(Overlap)-1) := A(A'First..Bytes_Per_Block-abs(Overlap)-1);
            Tag(Tag'First..abs(Overlap)-1) := A(Bytes_Per_Block-abs(Overlap)..A'Last);
            Tag(abs(Overlap)..abs(Overlap)+Bytes_Read-1) := B(B'First..B'First+Bytes_Read-1);
            Bytelen := (Bytes_Per_Block-abs(Overlap));
            Bitlen := Convert_Length_To_Block(8 * Bytelen);

         -- A is an entire Ciphertext block. B contains the last
         -- Ciphertext bytes and the Tag, so A must be decrypt.
         elsif (Bytes_Read - This.Taglen) > 0 then
            Dec := True;
            Last_Block(Last_Block'First..Overlap-1) := B(B'First..Overlap-1);
            Tag(Tag'First..This.Taglen-1) := B(Overlap..Bytes_Read-1);
            Bytelen := Overlap;
            Bitlen := Convert_Length_To_Block(8 * Bytelen);
         end if;

      else
         if Store_Internally then
            -- Only block A must be append to Masked_Plaintext
            Masked_Plaintext.Append(A);
         end if;

         if Overlap < 0 then
            raise Constraint_Error with "Invalid Ciphertext";
         elsif Overlap = 0 then -- no Ciphertext was written (|M| = 0)
            Tag(Tag'First..This.Taglen-1) := A(A'First..A'First+This.Taglen-1);
            Bytelen := 0;
            Bitlen := Convert_Length_To_Block(Bytelen);
         else
            Last_Block(Last_Block'First..Last_Block'First+Overlap-1) := A(A'First..A'First+Overlap-1);
            Tag(Tag'First..Tag'First+This.Taglen-1) := A(Overlap..Bytes_Read-1);
            Bytelen := Overlap;
            Bitlen := Convert_Length_To_Block(8 * Bytelen);
         end if;

      end if;
   end Extract;

   -----------------------------------------------------------------

   -- This function counts the number of trailing 0-bits in the binary
   -- representation of "Value" (current number of en- or de-crypted
   -- blocks, started at 1.
   function Number_Of_Trailing_Zeros(Value : in Positive) return Natural is
      C: Natural := 0;
      X: Word := Word(Value);
   begin
      if (X and 16#01#) /= 0 then
         return C;
      else
         C := C + 1;
         for I in 1..Positive'Size loop
            X := Shift_Right(X,1);
            if (X and 16#01#) /= 0 then
               return C;
            else
               C := C + 1;
            end if;
         end loop;
      end if;
      return C;
   end Number_Of_Trailing_Zeros;

   -----------------------------------------------------------------

   -- This procedure is called every time a block must be encrypted.
   -- Also the Offset, Checksum and the number of encrypted blocks
   -- will be updated. The encrypted block will be written.
   procedure Aux_Enc (This     :  in     AE_OCB;
                      Offset   :  in out Block;
                      Checksum :  in out Block;
                      Count    :  in out Positive;
                      Write    :  in     Callback_Writer;
                      Input    :  in     Block;
                      Output   :  in out Block) is
   begin
      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Count));
      BC.Encrypt(Input xor Offset, Output);
      Output := Output xor Offset;
      Write(To_Bytes(Output));
      Checksum := Checksum xor Input;
      Count := Count + 1;
   end Aux_Enc;

   -----------------------------------------------------------------

   -- This procedure is called every time a block must be decrypted.
   -- Also the Offset, Checksum and the number of decrypted blocks
   -- will be updated. The decrypted block will be first masked
   -- and then written.
   procedure Aux_Dec (This     :  in     AE_OCB;
                      Offset   :  in out Block;
                      Checksum :  in out Block;
                      Count    :  in out Positive;
                      Input    :  in     Block;
                      Output   :  in out Block) is
   begin

      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Count));
      BC.Decrypt(Input xor Offset, Output);
      Output := Output xor Offset;

      Checksum := Checksum xor Output;
      Count := Count + 1;

   end Aux_Dec;

   -----------------------------------------------------------------

   -- This procedure decrypt and write each Ciphertext block. It won't
   -- be called, if the calculated Tag isn't the same as the specified.
   procedure Write_Decrypted_Plaintext(This                  : in AE_OCB;
                                       Read_Ciphertext_Again : in Callback_Reader;
                                       Write_Plaintext       : in Callback_Writer;
                                       Dec_Bool              : in Boolean;
                                       Last_P_Block          : in Bytes;
                                       Last_B_Bytelen        : in Natural) is

      Bytes_Read: Natural;
      Blockcount: Positive := 1;
      Offset: Block := This.Offset;
      Checksum: Block := Zero_Block;
      Plaintext: Block;

      First_Block: Bytes := Zero_Bytes;
      Second_Block: Bytes := Zero_Bytes;
      Third_Block: Bytes := Zero_Bytes;
   begin
      Read_Ciphertext_Again(First_Block, Bytes_Read);
      if Bytes_Read = 0 then
         raise Constraint_Error with "Invalid Ciphertext";
      elsif
        Bytes_Read < Bytes_Per_Block then
         null;
      else
         Read_Ciphertext_Again(Second_Block, Bytes_Read);

         -- If Bytes_Read = 0, only Last_P_Block must be written
         -- If (Bytes_Per_Block > Bytes_Read > 0) and Dec_Bool = True then
         -- First_Block will be decrypt, else only Last_P_Block will be written
         if Bytes_Read = Bytes_Per_Block then
            loop
               Read_Ciphertext_Again(Third_Block, Bytes_Read);

               if Bytes_Read = Bytes_Per_Block then
                  Aux_Dec(This     => This,
                          Offset   => Offset,
                          Checksum => Checksum,
                          Count    => Blockcount,
                          Input    => To_Block_Type(First_Block),
                          Output   => Plaintext);
                  Write_Plaintext(To_Bytes(Plaintext));
                  First_Block := Second_Block;
                  Second_Block := Third_Block;
               elsif Bytes_Read = 0 then
                  Bytes_Read := Bytes_Per_Block;
                  exit;
               else
                  Aux_Dec(This     => This,
                          Offset   => Offset,
                          Checksum => Checksum,
                          Count    => Blockcount,
                          Input    => To_Block_Type(First_Block),
                          Output   => Plaintext);
                  Write_Plaintext(To_Bytes(Plaintext));

                  First_Block := Second_Block;
                  Second_Block := Third_Block;
                  exit;
               end if;
            end loop;
         end if;
      end if;

      if Dec_Bool then
         Aux_Dec(This     => This,
                 Offset   => Offset,
                 Checksum => Checksum,
                 Count    => Blockcount,
                 Input    => To_Block_Type(First_Block),
                 Output   => Plaintext);
         Write_Plaintext(To_Bytes(Plaintext));
      end if;

      -- write last Plaintext bytes
      if Last_B_Bytelen > 0 then
         Write_Plaintext(Last_P_Block(Last_P_Block'First..Last_B_Bytelen-1));
      end if;
   end Write_Decrypted_Plaintext;

   -----------------------------------------------------------------
   ----
   ---- overriding functions and procedures
   ----
   -----------------------------------------------------------------

   procedure Init_Encrypt(This   : out    AE_OCB;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class) is
   begin
      BC.Prepare_Key(Key);
      This.Nonce_Value := Nonce.Update;
      This.Taglen := Bytes_Per_Block;
      Generate_L(This.L_Array);
      BC.Encrypt(This.Nonce_Value xor This.L_Array(0), This.Offset);
   end Init_Encrypt;

   -----------------------------------------------------------------

   procedure Init_Decrypt(This        : out AE_OCB;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block) is
   begin
      BC.Prepare_Key(Key);
      This.Nonce_Value := Nonce_Value;
      This.Taglen := Bytes_Per_Block;
      Generate_L(This.L_Array);
      BC.Encrypt(This.Nonce_Value xor This.L_Array(0), This.Offset);
   end Init_Decrypt;

   -----------------------------------------------------------------

   procedure Encrypt(This             : in out AE_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is

      Bytes_Read: Natural;
      Ciphertext : Block;
      Offset: Block := This.Offset;
      Blockcount: Positive := 1;
      Checksum: Block := Zero_Block;
      X: Block;

      Last_C_Block: Bytes := Zero_Bytes;     -- last Ciphertext block in bytes
      Last_P_Block: Bytes(Zero_Bytes'Range); -- last Plaintext block in bytes
      Last_B_Bitlen: Block := Zero_Block;    -- bit-length of the last block represented as Block

      Prev_Block: Bytes := Zero_Bytes;
      Curr_Block: Bytes := Zero_Bytes;
   begin
      Read_Plaintext(Prev_Block, Bytes_Read);
      if Bytes_Read < Bytes_Per_Block then
         Last_P_Block(Prev_Block'First..Prev_Block'First+Prev_Block'Length-1) := Prev_Block;
         Last_B_Bitlen := Convert_Length_To_Block(8 * Bytes_Read);
      else
         loop
            Read_Plaintext(Curr_Block, Bytes_Read);

            if Bytes_Read = Bytes_Per_Block then

               Aux_Enc(This       => This,
                       Offset     => Offset,
                       Checksum   => Checksum,
                       Write      => Write_Ciphertext,
                       Count      => Blockcount,
                       Input      => To_Block_Type(Prev_Block),
                       Output => Ciphertext);
               Prev_Block := Curr_Block;

            elsif Bytes_Read = 0 then
               Last_P_Block := Prev_Block;
               -- Assigning is important for later use:
               Bytes_Read := Bytes_Per_Block;
               Last_B_Bitlen := Convert_Length_To_Block(8 * Bytes_Read);
               exit;
            else
               Aux_Enc(This       => This,
                       Offset     => Offset,
                       Checksum   => Checksum,
                       Write      => Write_Ciphertext,
                       Count      => Blockcount,
                       Input      => To_Block_Type(Prev_Block),
                       Output => Ciphertext);
               Last_P_Block := Curr_Block;
               Last_B_Bitlen := Convert_Length_To_Block(8 * Bytes_Read);
               exit;
            end if;

         end loop;
      end if;

      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Blockcount));

      X := Last_B_Bitlen xor This.L_Array(-1) xor Offset; -- length of C[m] in bits represented as an n-bit block
      BC.Encrypt(X, Ciphertext); -- Ciphertext = Y[m]

      if Bytes_Read > 0 then
         declare
            Y: constant Bytes := To_Bytes(Ciphertext);
         begin
            Last_C_Block(0..Bytes_Read-1) := Last_P_Block(0..Bytes_Read-1) xor Y(Y'First..Y'First+Bytes_Read-1);
         end;
      end if;

      Checksum := Checksum xor Ciphertext xor To_Block_Type(Last_C_Block);
      -- calculate the Tag
      BC.Encrypt(Checksum xor Offset, Ciphertext);

      -- concatenate the last block and Tag (if necessary)
      declare
         C: constant Bytes := To_Bytes(Ciphertext);
      begin
         if Bytes_Read < Bytes_Per_Block then
            declare
               -- |B| = |last message block| + |desired Tag|
               B: Bytes(0..(Bytes_Read+This.Taglen-1));
            begin
               -- concatenate last Ciphertext bytes with the Tag
               B(B'First..Bytes_Read-1) := Last_C_Block(Last_C_Block'First..Bytes_Read-1);
               B(Bytes_Read..B'Last) := C(C'First..C'First+This.Taglen-1);
               if B'Length > Bytes_Per_Block then
                  Write_Ciphertext(B(B'First..Bytes_Per_Block-1));
                  declare
                     -- This step is only for normalizing the index (starting at 0)
                     Temp: constant Bytes(0..(B'Length-Bytes_Per_Block)-1) := B(Bytes_Per_Block..B'Last);
                  begin
                     Write_Ciphertext(Temp);
                  end;
               else
                  Write_Ciphertext(B(B'First..B'Last));
               end if;
            end;
         else
            -- write the last Ciphertext block an the Tag
            Write_Ciphertext(Last_C_Block);
            Write_Ciphertext(C(C'First..This.Taglen-1));
         end if;
      end;

   end Encrypt;

   -----------------------------------------------------------------

   function Aux_Decrypt(This                   : in AE_OCB;
                        Read_Ciphertext        : in Callback_Reader;
                        Read_Ciphertext_Again  : in Callback_Reader;
                        Write_Plaintext        : in Callback_Writer)
                        return Boolean is

      Bytes_Read: Natural;
      Blockcount: Positive := 1;
      Offset: Block := This.Offset;
      Checksum: Block := Zero_Block;
      Plaintext: Block;
      Tag: Bytes := Zero_Bytes;
      T: Block;
      Dec_Bool: Boolean;
      Verification_Bool: Boolean := False;

      Last_P_Block: Bytes := Zero_Bytes;  -- last Plaintext block in bytes
      Last_C_Block: Bytes := Zero_Bytes;  -- last Ciphertext block in bytes
      Last_B_Bitlen: Block := Zero_Block; -- bit-length of the last block represented as Block
      Last_B_Bytelen: Natural;            -- byte-length of the last block

      First_Block: Bytes := Zero_Bytes;
      Second_Block: Bytes := Zero_Bytes;
      Third_Block: Bytes := Zero_Bytes;
   begin
      Read_Ciphertext(First_Block, Bytes_Read);

      if Bytes_Read = 0 then
         -- Tag must be at least 1 byte
	 raise Constraint_Error with "Invalid Ciphertext";
      elsif Bytes_Read < Bytes_Per_Block then
         Extract(This       => This,
                 A          => First_Block,
                 Bytes_Read => Bytes_Read,
                 Bitlen     => Last_B_Bitlen,
                 Bytelen    => Last_B_Bytelen,
                 Last_Block => Last_C_Block,
                 Tag        => Tag,
                 Two_Blocks => False,
                 Dec        => Dec_Bool);
      else
         Read_Ciphertext(Second_Block, Bytes_Read);

         if Bytes_Read < Bytes_Per_Block then
            if Bytes_Read = 0 then
               -- First_Block was the last block and filled up
               Bytes_Read := Bytes_Per_Block;
               Extract(This       => This,
                       A          => First_Block,
                       Bytes_Read => Bytes_Read,
                       Bitlen     => Last_B_Bitlen,
                       Bytelen    => Last_B_Bytelen,
                       Last_Block => Last_C_Block,
                       Tag        => Tag,
                       Two_Blocks => False,
                       Dec        => Dec_Bool);
            else
               Extract(This       => This,
                       A          => First_Block,
                       B          => Second_Block,
                       Bytes_Read => Bytes_Read,
                       Bitlen     => Last_B_Bitlen,
                       Bytelen    => Last_B_Bytelen,
                       Last_Block => Last_C_Block,
                       Tag        => Tag,
                       Two_Blocks => True,
                       Dec        => Dec_Bool);
            end if;
         else
            loop
               Read_Ciphertext(Third_Block, Bytes_Read);

               if Bytes_Read = Bytes_Per_Block then
                  Aux_Dec(This     => This,
                          Offset   => Offset,
                          Checksum => Checksum,
                          Count    => Blockcount,
                          Input    => To_Block_Type(First_Block),
                          Output   => Plaintext);

                  if Store_Internally then
                     Masked_Plaintext.Append(First_Block);
                  end if;

                  First_Block := Second_Block;
                  Second_Block := Third_Block;
               elsif Bytes_Read = 0 then
                  -- Second_Block was a full block
                  Bytes_Read := Bytes_Per_Block;
                  Extract(This       => This,
                          A          => First_Block,
                          B          => Second_Block,
                          Bytes_Read => Bytes_Read,
                          Bitlen     => Last_B_Bitlen,
                          Bytelen    => Last_B_Bytelen,
                          Last_Block => Last_C_Block,
                          Tag        => Tag,
                          Two_Blocks => True,
                          Dec        => Dec_Bool);
                  exit;
               else
                  -- decrypt First_Block
                  Aux_Dec(This     => This,
                          Offset   => Offset,
                          Checksum => Checksum,
                          Count    => Blockcount,
                          Input    => To_Block_Type(First_Block),
                          Output   => Plaintext);

                  if Store_Internally then
                     Masked_Plaintext.Append(First_Block);
                  end if;

                  -- Assigning is important because, if Dec_Bool = True,
                  -- First_Block will be used later for decryption.
                  First_Block := Second_Block;
                  Second_Block := Third_Block;

                  Extract(This       => This,
                          A          => First_Block,
                          B          => Second_Block,
                          Bytes_Read => Bytes_Read,
                          Bitlen     => Last_B_Bitlen,
                          Bytelen    => Last_B_Bytelen,
                          Last_Block => Last_C_Block,
                          Tag        => Tag,
                          Two_Blocks => True,
                          Dec        => Dec_Bool);
                  exit;
               end if;
            end loop;
         end if;
      end if;

      -- If B containts last Ciphertext bytes
      -- and the Tag, A must be decrypt.
      if Dec_Bool then
         Aux_Dec(This     => This,
                 Offset   => Offset,
                 Checksum => Checksum,
                 Count    => Blockcount,
                 Input    => To_Block_Type(First_Block),
                 Output   => Plaintext);
      end if;

      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Blockcount));

      declare
         X: Block;
      begin
         X := Last_B_Bitlen xor This.L_Array(-1) xor Offset;
         BC.Encrypt(X, Plaintext);
      end;

      if Last_B_Bytelen > 0 then -- only false if |Message| = 0
         declare
            Y: constant Bytes := To_Bytes(Plaintext);
         begin
            Last_P_Block(0..Last_B_Bytelen-1) := Last_C_Block(Last_C_Block'First..Last_B_Bytelen-1) xor Y(Y'First..Y'First+Last_B_Bytelen-1);
         end;
      end if;

      Checksum := Checksum xor Plaintext xor To_Block_Type(Last_C_Block);
      BC.Encrypt(Checksum xor Offset, T);

      declare
         Calculated_Tag: constant Bytes := To_Bytes(T);
      begin
         if Tag(Tag'First..This.Taglen-1) = Calculated_Tag(Calculated_Tag'First..This.Taglen-1) then
            Verification_Bool := True;
            Write_Decrypted_Plaintext(This, Read_Ciphertext_Again, Write_Plaintext, Dec_Bool, Last_P_Block, Last_B_Bytelen);
         end if;
      end;

      return Verification_Bool;

   end Aux_Decrypt;

   -----------------------------------------------------------------

   function Decrypt_And_Verify(This                   : in out AE_OCB;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer)
                               return Boolean is

      RCA: constant Callback_Reader := Read_Masked_Plaintext'Access;
   begin
      if Read_Ciphertext_Again = null then

         Store_Internally := True;

         return Aux_Decrypt(This                   => This,
                            Read_Ciphertext        => Read_Ciphertext,
                            Read_Ciphertext_Again  => RCA,
                            Write_Plaintext        => Write_Plaintext);
      else
         return Aux_Decrypt(This                   => This,
                            Read_Ciphertext        => Read_Ciphertext,
                            Read_Ciphertext_Again  => Read_Ciphertext_Again,
                            Write_Plaintext        => Write_Plaintext);
      end if;

   end Decrypt_And_Verify;

   -----------------------------------------------------------------
   ----
   ---- additional functions and procedures
   ----
   -----------------------------------------------------------------

   procedure Init_Encrypt(This   : out    AE_OCB;
                          Key    : in     Key_Type;
                          Nonce  : in out N.Nonce'Class;
                          Taglen : in     Positive) is
   begin
      BC.Prepare_Key(Key);
      This.Nonce_Value := Nonce.Update;
      This.Taglen := Taglen;
      Generate_L(This.L_Array);
      BC.Encrypt(This.Nonce_Value xor This.L_Array(0), This.Offset);

   end Init_Encrypt;

   -----------------------------------------------------------------

   procedure Init_Decrypt(This        : out AE_OCB;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block;
                          Taglen      : in  Positive) is
   begin
      BC.Prepare_Key(Key);
      This.Nonce_Value := Nonce_Value;
      This.Taglen := Taglen;
      Generate_L(This.L_Array);
      BC.Encrypt(This.Nonce_Value xor This.L_Array(0), This.Offset);

   end Init_Decrypt;

   -----------------------------------------------------------------

end Crypto.Symmetric.AE_OCB;
