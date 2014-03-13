with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Crypto.Types;

package body Crypto.Symmetric.AE_OCB3 is

   -- useful constants
   Zero_Bytes: constant Bytes(0..(Bytes_Per_Block - 1)) := (others => 0);
   Zero_Block: constant Block := To_Block_Type(Zero_Bytes);
   -- If Store_Internally = True, every Ciphertext block will be stored in
   -- the Vector Masked_Plaintext
   Store_Internally: Boolean := False;

   --Hui
   L_Star, L_Dollar : Block := Zero_Block;  
   Nonce_Init : Bytes := Zero_Bytes;
   AD : Block := Zero_Block; -- no AD, Hash(AD) returns zeros();

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

   function Double_S(S: Block) return Block is
      use Crypto.Types;
      Result : Bytes(0..(Bytes_Per_Block - 1)) := To_Bytes(S);
      --Tmp : Bytes := Zero_Bytes;
      Tmp_1 : B_Block128 := To_B_Block128(To_Bytes(S));
   begin
      if Result(0) < 128 then
         Result:=To_Bytes( Shift_Left(Tmp_1,1));
      else
         Result :=  To_Bytes(Shift_Left(Tmp_1,1)) xor 2#1000_0111#;
      end if;
      return To_Block_Type(Result);
   end Double_S;
   
   -----------------------------------------------------------------
   -- This procedure should be run before encryption and decryption.
   procedure Setup(Key: in  Key_Type;
                     A: in out Block_Array) is
      Tmp : Bytes(0 .. Bytes_Per_Block - 1);
   begin
      BC.Encrypt(Zero_Block, L_Star); --L_*
      Tmp := To_Bytes(L_Star);
      Error_Output.Put("L_*");
      for i in Tmp'Range loop
          Error_Output.Put(To_Hex(Tmp(i)));
      end loop;
      Error_Output.New_Line;

      L_Dollar := Double_S(L_Star);   --L_$
      Tmp := To_Bytes(L_Dollar);
      Error_Output.Put("L_$");
      for i in Tmp'Range loop
          Error_Output.Put(To_Hex(Tmp(i)));
      end loop;
      Error_Output.New_Line;

      A(-1) := Double_S(L_Dollar); -- (-1) refers to L[0] in the docu
      for i in 0..31 loop
          A(i) := Double_S(A(i-1));
      end loop;
      Tmp := To_Bytes(A(0));
      Error_Output.Put("L_1");
      for i in Tmp'Range loop
          Error_Output.Put(To_Hex(Tmp(i)));
      end loop;
      Error_Output.New_Line;
   end Setup;

   -----------------------------------------------------------------

   function Stretch_Then_Shift(Value        : Bytes;
                               Amount       : Natural) return Block is
      K : Block := To_Block(Value);
      L : Bytes := Value;
      R : Bytes(0..Bytes_Per_Block*2);
   begin
      
--        --stretch 
--        L := To_Bytes(Shift_Left(K, Amount));    
--    
--        --110
--        K := K xor Shift_Left(K, 8); -- specified in docu      
--        R := To_Bytes(Shift_Right(K, Bytes_Per_Block*8 - Amount));      
--  
--        --Shift
--        L := To_Bytes(To_Block(L) xor To_Block(R)); 

      
      R(0..Bytes_Per_Block-1) := Value;
      R(Bytes_Per_Block..Bytes_Per_Block+7) 
        := Value(0..7) 
        xor Value(1..8);
      
      R:=Shift_Left(R, Amount);
      
      return To_Block(R(1..Bytes_Per_Block));
   end Stretch_Then_Shift;

   -----------------------------------------------------------------

    function Padding_One_Zero (Value      : Bytes;
                               Bytes_Read : Natural ) return Block is
      Result : Bytes := Zero_Bytes;
      One_Zero : constant Byte := 2#1000_0000#;
   begin
      if Bytes_Read < Bytes_Per_Block then
         Result(0 .. Bytes_Read - 1) := Value(0 .. Bytes_Read - 1);
         Result(Bytes_Read) := One_Zero;
      elsif Bytes_Read = Bytes_Per_Block then
         Result := Value;
      end if;
      return To_Block(Result);
   end Padding_One_Zero;

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
            raise  Constraint_Error with "Invalid_Ciphertext_Error";
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
      X: Word :=  Word(Value);
   begin
      if (Word(Value) and 16#01#) /= 0 then
         return C;
      else
         C := C + 1;
         for I in 1..Positive'Size loop
            X := Shift_Right(X,1);
            if (Word(X) and 16#01#) /= 0 then
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
      Tmp : Bytes(0 .. Bytes_Per_Block - 1);
   begin
      --113 Offset = Offset XOR L
      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Count) - 1);
      Tmp := To_Bytes(Offset);
      Error_Output.Put("Offset");
      for i in Tmp'Range loop
          Error_Output.Put(To_Hex(Tmp(i)));
      end loop;
      Error_Output.New_Line;      

      --114
      BC.Encrypt(Input xor Offset, Output);
      Output := Output xor Offset;
      Write(To_Bytes(Output));
      --115
      Checksum := Checksum xor Input;
      Tmp := To_Bytes(Checksum);
      Error_Output.Put("Checksum");
      for i in Tmp'Range loop
          Error_Output.Put(To_Hex(Tmp(i)));
      end loop;
      Error_Output.New_Line; 
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
      --313 
      Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Count) - 1);    

      --314
      BC.Decrypt(Input xor Offset, Output);
      Output := Output xor Offset;
     
      --315
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
	 raise  Constraint_Error with "Invalid_Ciphertext_Error";	 
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
      This.Nonce_Value := Nonce.Update;
      BC.Prepare_Key(Key);
      Setup(Key, This.L_Array);
      This.Taglen := Bytes_Per_Block;
   end Init_Encrypt;

   -----------------------------------------------------------------

   procedure Init_Decrypt(This        : out AE_OCB;
                          Key         : in  Key_Type;
                          Nonce_Value : in  Block) is
   begin
      BC.Prepare_Key(Key);
      Setup(Key, This.L_Array);
      This.Nonce_Value := Nonce_Value;
      This.Taglen := Bytes_Per_Block;
      BC.Encrypt(This.Nonce_Value xor This.L_Array(0), This.Offset);
   end Init_Decrypt;

   -----------------------------------------------------------------

   procedure Encrypt(This             : in out AE_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer) is

      
        
      Empty_Callback_Reader : Callback_Reader := EmptyReader'Access;
      
   begin
      Encrypt(This             => This,
              Read_Plaintext   => Read_Plaintext,
              Write_Ciphertext => Write_Ciphertext,
              Read_AD          => Empty_Callback_Reader);

   end Encrypt;

   
   
   
   
   
   -----------------------------------------------------------------

   function Aux_Decrypt(This                   : in AE_OCB;
                        Read_Ciphertext        : in Callback_Reader;
                        Read_Ciphertext_Again  : in Callback_Reader;
                        Write_Plaintext        : in Callback_Writer;
                        Read_AD		       : in Callback_Reader)
                        return Boolean is

      Bytes_Read: Natural;
      Blockcount: Positive := 1;
      Offset: Block := This.Offset;
      Checksum: Block := Zero_Block;
      Plaintext: Block;
      Tag, Tmp: Bytes := Zero_Bytes;
      T, Pad : Block;
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
         raise  Constraint_Error with "Invalid_Ciphertext_Error";
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

     
      
      
      if Last_B_Bytelen = 0 then
         null;
         
      elsif Last_B_Bytelen < Bytes_Per_Block then 
       --317
      Offset := Offset xor L_Star;
       
      --318
      BC.Encrypt(Offset, Pad);
         declare
            C: Bytes := To_Bytes(Pad);
         begin
            --319
            Last_P_Block(0..Bytes_Read-1) := Last_C_Block(0..Bytes_Read-1) xor C(0 .. Bytes_Read-1);
         end;
         --320 
         Checksum := Checksum xor Padding_One_Zero(Last_P_Block, Bytes_Read);   
         
      else
         Aux_Dec(This     => This,
                 Offset   => Offset,
                 Checksum => Checksum,
                 Count    => Blockcount,
                 Input    => To_Block_Type(Last_C_Block),
                 Output   => Plaintext);
         
         Masked_Plaintext.Append(New_Item => Last_C_Block);
         Last_B_Bytelen := 0;
      end if;
      
      --321
      Offset := Offset xor L_Dollar;
      
      --322 Final -> Tag
      BC.Encrypt(Checksum xor Offset, T);      
     
      --"Hashing" of the Associated Data
      T := T xor Hash_AD(This    => This,
                         Read_AD => Read_AD);
      
      
      declare
         Calculated_Tag: constant Bytes := To_Bytes(T);
      begin
         Error_Output.Put_Line("Original Tag:");
         for I in Tag'First..This.Taglen-1 loop
            Error_Output.Put(To_Hex(B => Tag(I)));
         end loop;
         Error_Output.New_Line;
         Error_Output.Put_Line("Calculated Tag:");
         for I in Calculated_Tag'First..This.Taglen-1 loop
            Error_Output.Put(To_Hex(B => Calculated_Tag(I)));
         end loop;
            
         
         if Tag(Tag'First..This.Taglen-1) = Calculated_Tag(Calculated_Tag'First..This.Taglen-1) then
            Verification_Bool := True;
            Write_Decrypted_Plaintext(This, Read_Ciphertext_Again, Write_Plaintext, Dec_Bool, Last_P_Block, Last_B_Bytelen);
         end if;
      end;

      return Verification_Bool;

   end Aux_Decrypt;

   
   
   -----------------------------------------------------------------
   
   function Aux_Decrypt(This                   : in AE_OCB;
                        Read_Ciphertext        : in Callback_Reader;
                        Read_Ciphertext_Again  : in Callback_Reader;
                        Write_Plaintext        : in Callback_Writer)
                        return Boolean is
   begin
      
      return Aux_Decrypt(This		       => This,
                         Read_Ciphertext       => Read_Ciphertext,
                  	 Read_Ciphertext_Again => Read_Ciphertext_Again,
                         Write_Plaintext       => Write_Plaintext,
                         Read_AD	       => null);
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

   procedure Init_Encrypt(This                : out    AE_OCB;
                          Key                 : in     Key_Type;
                          N_Init              : in out N.Nonce'Class;
                          Bytes_Of_N_Read     : in     Positive;
                          Taglen              : in     Positive) is
         Top, Bottom : Bytes := Zero_Bytes;
         Tmp_Top : Bytes(0..(Bytes_Per_Block - 1)) := (others => 2#1111_1111#);
         Tmp_Bottom : Bytes := Zero_Bytes;
         Ktop : Block := Zero_Block;
         Tmp : Bytes(0..(Bytes_Per_Block - 1));
   begin  
         --102
         if Bytes_Of_N_Read > 16 then
            raise Noncelength_Not_Supported;
         else
            BC.Prepare_Key(Key);
            Setup(Key, This.L_Array);
            This.Nonce_Value := N_Init.Update;
            This.Taglen := Taglen;
            --Generate_L(This.L_Array);
            
            --106: Nonce_Init = 0^(127- |N|) 1 N
            Nonce_Init(Bytes_Per_Block - Bytes_Of_N_Read .. Bytes_Per_Block - 1) := To_Bytes(This.Nonce_Value)(0.. Bytes_Of_N_Read - 1) ;
            Nonce_Init(Bytes_Per_Block - Bytes_Of_N_Read - 1) := 2#0000_0001# ;
            
            Error_Output.Put("Nonce");
            for i in Nonce_Init'Range loop
                Error_Output.Put(Nonce_Init(i)'Img);
            end loop;
            Error_Output.New_Line; 
            

            --107: Top = Nonce AND (1^122 0^6)
            Tmp_Top(Tmp_Top'Last) := 2#1100_0000#;
            Top := Nonce_Init and Tmp_Top;

            --108: Bottom = Nonce AND (0^122 1^6)
            Tmp_Bottom(Tmp_Bottom'Last) := 2#0011_1111#;
            Bottom := Nonce_Init and Tmp_Bottom;
            Error_Output.Put("Bottom");
            for i in Bottom'Range loop
                Error_Output.Put(Bottom(i)'Img);
            end loop;
            Error_Output.New_Line;
          

            --109: Ktop = Ek(Top)
            BC.Encrypt(To_Block(Top), Ktop);
            Tmp := To_Bytes(Ktop);
            Error_Output.Put("Ktop");
            for i in Tmp'Range loop
                Error_Output.Put(To_Hex(Tmp(i)));
            end loop;
            Error_Output.New_Line;

            --110-111: Offset = (Stretch<<Bottom)[1..128]
            This.Offset := Stretch_Then_Shift(To_Bytes(Ktop), Integer(Bottom(Bottom'Last)));
            Tmp := To_Bytes(This.Offset);
            Error_Output.Put("Initial Offset");
            for i in Tmp'Range loop
                Error_Output.Put(To_Hex(Tmp(i)));
            end loop;
            Error_Output.New_Line;  
         end if;
   end Init_Encrypt;

   -----------------------------------------------------------------

   procedure Init_Decrypt(This                : out    AE_OCB;
                          Key                 : in     Key_Type;
                          N_Init              : in     Block;
                          Bytes_Of_N_Read     : in     Positive;
                          Taglen              : in     Positive) is
         Top, Bottom : Bytes := Zero_Bytes;
         Tmp_Top : Bytes(0..(Bytes_Per_Block - 1)) := (others => 2#1111_1111#);
         Tmp_Bottom : Bytes := Zero_Bytes;
         Ktop : Block := Zero_Block;    
   begin
      --302
      if Bytes_Of_N_Read > 16 then
         raise Noncelength_Not_Supported;
      else
         BC.Prepare_Key(Key);
         Setup(Key, This.L_Array);
         This.Nonce_Value := N_Init;
         This.Taglen := Taglen;
     
      --306
         Nonce_Init(Bytes_Per_Block - Bytes_Of_N_Read .. Bytes_Per_Block - 1) := To_Bytes(This.Nonce_Value)(0.. Bytes_Of_N_Read - 1) ;
         Nonce_Init(Bytes_Per_Block - Bytes_Of_N_Read - 1) := 2#0000_0001# ;
            
      --307
         Tmp_Top(Tmp_Top'Last) := 2#1100_0000#;
         Top := Nonce_Init and Tmp_Top;

      --308
         Tmp_Bottom(Tmp_Bottom'Last) := 2#0011_1111#;
         Bottom := Nonce_Init and Tmp_Bottom;
        
      --309
         BC.Encrypt(To_Block(Top), Ktop);

      --310-311
         This.Offset := Stretch_Then_Shift(To_Bytes(Ktop), Integer(Bottom(Bottom'Last)));         
      end if;
   end Init_Decrypt;
   
   
   function Hash_AD(This     	  : in  AE_OCB;
                  Read_AD 	  : in	Callback_Reader) return Block is
      Curr_Block: Bytes := Zero_Bytes;
      Curr_Encrypted: Block := Zero_Block;
      Return_Block: Bytes := Zero_Bytes;
      Bytes_Read: Natural;
      Count : Natural := 0;
      
      Offset : Block := Zero_Block;
      Sum    : Block;
      
      
   begin
      
      if Read_AD = null then 
         return Zero_Block; 
      end if;
      
      loop
         Read_AD(Curr_Block, Bytes_Read);
         Count := Count+1;
         if Bytes_Read = Bytes_Per_Block then
            Offset := Offset xor This.L_Array(Number_Of_Trailing_Zeros(Count) - 1);
            BC.Encrypt(To_Block(Curr_Block) xor Offset, Curr_Encrypted);
            Return_Block := Return_Block xor To_Bytes(Curr_Encrypted);
         elsif Bytes_Read > 0 then
            Offset := Offset xor L_Star;
            BC.Encrypt(Padding_One_Zero(Curr_Block, Bytes_Read) xor Offset, Curr_Encrypted);
            Return_Block := Return_Block xor To_Bytes(Curr_Encrypted);
            exit;
         else
            exit;
         end if;   
      end loop;
      
      if Count = 1 and Bytes_Read = 0 then
         Sum := Zero_Block;
      else
        Sum := To_Block(Return_Block);
      end if;
   
      return Sum;
   
   end Hash_AD;
   
      
   ------------------------------------------------------------------------
   
   procedure Encrypt(This             : in out AE_OCB;
                     Read_Plaintext   : in     Callback_Reader;
                     Write_Ciphertext : in     Callback_Writer;
                     Read_AD	      : in     Callback_Reader) is

      Bytes_Read: Natural;
      Ciphertext : Block;
      Offset: Block := This.Offset;
      Blockcount: Positive := 1;
      Checksum: Block := Zero_Block;
      Nonce: Bytes := Zero_Bytes;
      Pad : Block;

      Last_C_Block: Bytes := Zero_Bytes;     -- last Ciphertext block in bytes
      Last_P_Block: Bytes(Zero_Bytes'Range); -- last Plaintext block in bytes

      Prev_Block: Bytes := Zero_Bytes;
      Curr_Block: Bytes := Zero_Bytes;

      Tmp1 :Bytes := Zero_Bytes;
      Tmp : Block := To_Block(Zero_Bytes xor 2#0000_1111#);--8+4+2+1=15
   begin

      Error_Output.Put_Line("Test for Double_S");
      Tmp1 :=To_Bytes(Double_S(Tmp)); 
      for i in Tmp1'Range loop
         Error_Output.Put(Tmp1(i)'Img);
      end loop;
      Error_Output.New_Line;
      Error_Output.New_Line;

      --read the first block of plaintext
      Read_Plaintext(Prev_Block, Bytes_Read);

      --check if the block is the last block
      if Bytes_Read < Bytes_Per_Block then
         Last_P_Block(Prev_Block'First..Prev_Block'First+Prev_Block'Length-1) := Prev_Block;
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
                       Output     => Ciphertext);
               Prev_Block := Curr_Block;

            elsif Bytes_Read = 0 then
               
               Aux_Enc(This       => This,
                       Offset     => Offset,
                       Checksum   => Checksum,
                       Write      => Write_Ciphertext,
                       Count      => Blockcount,
                       Input      => To_Block_Type(Prev_Block),
                       Output     => Ciphertext);
               
               --Last_P_Block := Prev_Block;
               -- Assigning is important for later use:
               Bytes_Read := Bytes_Per_Block;
               exit;
            else
               Aux_Enc(This       => This,
                       Offset     => Offset,
                       Checksum   => Checksum,
                       Write      => Write_Ciphertext,
                       Count      => Blockcount,
                       Input      => To_Block_Type(Prev_Block),
                       Output     => Ciphertext);
               Last_P_Block := Curr_Block;
               exit;
            end if;

         end loop;
      end if;
      
--        Put("LAST P BLOCK");
--           for i in Last_P_Block'Range loop
--               Put(To_Hex(Last_P_Block(i)));
--           end loop;
      
      if Bytes_Read > 0 and Bytes_Read < Bytes_Per_Block then
         --117
            Offset := Offset xor L_Star;
            Tmp1 :=To_Bytes(Offset);
            Error_Output.Put("Offset_*"); 
            for i in Tmp1'Range loop
                Error_Output.Put(Tmp1(i)'Img);
            end loop;
            Error_Output.New_Line;
         --118
            BC.Encrypt(Offset, Pad);
         declare
            C: Bytes := To_Bytes(Pad);
         begin
         --119
            Last_C_Block(0..Bytes_Read-1) := Last_P_Block(0..Bytes_Read-1) xor C(C'First..C'First+Bytes_Read-1);
         end;
         --120 
         Checksum := Checksum xor Padding_One_Zero(Last_P_Block, Bytes_Read);
         Tmp1 := To_Bytes(Checksum);

         Error_Output.Put("Checksum_*");
         for i in Tmp1'Range loop
             Error_Output.Put(Tmp1(i)'Img);
         end loop;
         Error_Output.New_Line; 
      end if;
      --121
      Offset := Offset xor L_Dollar;
      
      --122 calculate the Tag
      BC.Encrypt(Checksum xor Offset, Ciphertext);
      Tmp1 := To_Bytes(Ciphertext);
      Error_Output.Put("Tag: ");
      for i in Tmp1'Range loop
         Error_Output.Put(Tmp1(i)'Img);
      end loop;
      Error_Output.New_Line;
      
      --Calculating the Hash of the Associated Data, XORing with Checksum
      
      Ciphertext := Ciphertext xor Hash_AD(This    => This,
                                           Read_AD => Read_AD);
      
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
            --Write_Ciphertext(Last_C_Block); --already done above in loop
            Write_Ciphertext(C(C'First..This.Taglen-1));
         end if;
      end;

   end Encrypt;
   
   function Decrypt_And_Verify(This                   : in out AE_OCB;
                               Read_Ciphertext        : in     Callback_Reader;
                               Read_Ciphertext_Again  : in     Callback_Reader := null;
                               Write_Plaintext        : in     Callback_Writer;
                               Read_AD	      	      : in     Callback_Reader)
                               return Boolean is
      
      RCA: constant Callback_Reader := Read_Masked_Plaintext'Access;
      
   begin
      if Read_Ciphertext_Again = null then

         Store_Internally := True;

         return Aux_Decrypt(This                   => This,
                            Read_Ciphertext        => Read_Ciphertext,
                            Read_Ciphertext_Again  => RCA,
                            Write_Plaintext        => Write_Plaintext,
                            Read_AD	           => Read_AD);
      else
         return Aux_Decrypt(This                   => This,
                            Read_Ciphertext        => Read_Ciphertext,
                            Read_Ciphertext_Again  => Read_Ciphertext_Again,
                            Write_Plaintext        => Write_Plaintext,
                            Read_AD	           => Read_AD);
      end if;

   end Decrypt_And_Verify;
   
   
   
   procedure EmptyReader(B : out Bytes; Count: out Natural) is
   begin
      Count := 0;
      B := (others=>0);
   end;
   
   procedure Encrypt(This             : in out AE_OCB;
                     Plaintext        : in     Bytes;
                     Ciphertext       : out    Bytes;
                     AD	      	      : in     Bytes) is
      
      package Vectors_Package is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                            Element_Type => Byte);
      Ciphertext_Vector : Vectors_Package.Vector;
      Plaintext_Vector : Vectors_Package.Vector;
      AD_Vector : Vectors_Package.Vector;
      
      Plaintext_Position : Natural := 0;
      AD_Position : Natural := 0;
      
      procedure Get_Bytes_Plaintext(B : in Bytes) is
      begin
--           Put_Line("Get_Bytes_Plaintext");
         for I in B'Range loop
            Plaintext_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Get_Bytes_Ciphertext(B : in Bytes) is
      begin
--            Put_Line("Get_Bytes_Ciphertext");
         for I in B'Range loop
            Ciphertext_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Get_Bytes_AD(B : in Bytes) is
      begin
--           Put_Line("Get_Bytes_AD");
         for I in B'Range loop
            AD_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Give_Bytes_Plaintext(B : out Bytes; Count: out Natural) is
         Rest : Natural;
      begin
--           Put_Line("Give_Bytes_Plaintext");
         Rest := Integer(Plaintext_Vector.Length) - Plaintext_Position;
--           Put_Line(Integer'Image(Rest));
         if Rest >= Bytes_Per_Block then
            Count := Bytes_Per_Block;
            for I in Plaintext_Position..Plaintext_Position+Bytes_Per_Block-1 loop
               B(I-Plaintext_Position):= Plaintext_Vector.Element(Index => I);
            end loop;
            Plaintext_Position:= Plaintext_Position+Bytes_Per_Block;
         elsif Rest = 0 then
            Count := 0;
         else
            Count := Rest;
            for I in Plaintext_Position..Plaintext_Position+Rest-1 loop
               B(I-Plaintext_Position):= Plaintext_Vector.Element(Index => I);
            end loop;
         end if;
         
--           Put("Plaintext-Bytes: ");
--           for I in B'Range loop
--              Put(To_Hex(B(I)));   
--           end loop;
--           Put("Plaintext Bytes Amount" & Natural'Image(Count));
         
         
      end Give_Bytes_Plaintext;
      
      procedure Give_Bytes_AD(B : out Bytes; Count: out Natural) is
         Rest : Natural;
      begin
--           Put_Line("Give_Bytes_AD");
         Rest := Integer(AD_Vector.Length) - AD_Position;
         if Rest >= Bytes_Per_Block then
            Count := Bytes_Per_Block;
            for I in AD_Position..AD_Position+Bytes_Per_Block-1 loop
               B(I-AD_Position):= AD_Vector.Element(Index => I);
            end loop;
            AD_Position:= AD_Position+Bytes_Per_Block;
         elsif Rest = 0 then
            Count := 0;
         else
            Count := Rest;
            for I in AD_Position..AD_Position+Rest-1 loop
               B(I-AD_Position):= AD_Vector.Element(Index => I);
            end loop;
         end if;
      end Give_Bytes_AD;
      
      procedure Give_Bytes_Ciphertext(B : out Bytes) is
         Curs : Vectors_Package.Cursor := Ciphertext_Vector.First;
         Counter : Integer := 0;
      begin
--           Put_Line("Give_Bytes_Ciphertext" & Integer'Image(Integer(Ciphertext_Vector.Length)));
         
         while(Vectors_Package.Has_Element(Curs)) loop
            B(Counter):= Ciphertext_Vector.Element(Vectors_Package.To_Index(Curs));
            Counter := Counter+1;
            Vectors_Package.Next(Curs);
         end loop;
         
      end Give_Bytes_Ciphertext;
   begin
      
      Get_Bytes_Plaintext(B => Plaintext);
      Get_Bytes_AD(B => AD);
      
      
      Encrypt(This             => This,
              Read_Plaintext   => Give_Bytes_Plaintext'Unrestricted_Access ,
              Write_Ciphertext => Get_Bytes_Ciphertext'Unrestricted_Access,
              Read_AD          => Give_Bytes_AD'Unrestricted_Access);
      
      
      Give_Bytes_Ciphertext(B => Ciphertext);
      
   end Encrypt;
   
   function Decrypt_And_Verify(This                   : in out AE_OCB;
                               Ciphertext             : in     Bytes;
                               Plaintext              : out    Bytes;
                               AD	      	      : in     Bytes)
                               return Boolean is 
      Verfied : Boolean := False;
      
      package Vectors_Package is new Ada.Containers.Vectors(Index_Type   => Natural,
                                                            Element_Type => Byte);
      
      Ciphertext_Vector : Vectors_Package.Vector;
      Plaintext_Vector : Vectors_Package.Vector;
      AD_Vector : Vectors_Package.Vector;
      
      Ciphertext_Position : Natural := 0;
      AD_Position : Natural := 0;
      
      procedure Get_Bytes_Plaintext(B : in Bytes) is
      begin
--           Put_Line("Get_Bytes_Plaintext");
         for I in B'Range loop
            Plaintext_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Get_Bytes_Ciphertext(B : in Bytes) is
      begin
--            Put_Line("Get_Bytes_Ciphertext");
         for I in B'Range loop
            Ciphertext_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Get_Bytes_AD(B : in Bytes) is
      begin
--           Put_Line("Get_Bytes_AD");
         for I in B'Range loop
            AD_Vector.Append(B(I));
         end loop;
      end;
      
      procedure Give_Bytes_Ciphertext(B : out Bytes; Count: out Natural) is
         Rest : Natural;
      begin
--           Put_Line("Give_Bytes_Ciphertext");
         Rest := Integer(Ciphertext_Vector.Length) - Ciphertext_Position;
--           Put_Line(Integer'Image(Rest));
         if Rest >= Bytes_Per_Block then
            Count := Bytes_Per_Block;
            for I in Ciphertext_Position..Ciphertext_Position+Bytes_Per_Block-1 loop
               B(I-Ciphertext_Position):= Ciphertext_Vector.Element(Index => I);
            end loop;
            Ciphertext_Position:= Ciphertext_Position+Bytes_Per_Block;
         elsif Rest = 0 then
            Count := 0;
         else
            Count := Rest;
            for I in Ciphertext_Position..Ciphertext_Position+Rest-1 loop
               B(I-Ciphertext_Position):= Ciphertext_Vector.Element(Index => I);
            end loop;
         end if;
      end Give_Bytes_Ciphertext;
      
      procedure Give_Bytes_AD(B : out Bytes; Count: out Natural) is
         Rest : Natural;
      begin
--           Put_Line("Give_Bytes_AD");
         Rest := Integer(AD_Vector.Length) - AD_Position;
         if Rest >= Bytes_Per_Block then
            Count := Bytes_Per_Block;
            for I in AD_Position..AD_Position+Bytes_Per_Block-1 loop
               B(I-AD_Position):= AD_Vector.Element(Index => I);
            end loop;
            AD_Position:= AD_Position+Bytes_Per_Block;
         elsif Rest = 0 then
            Count := 0;
         else
            Count := Rest;
            for I in AD_Position..AD_Position+Rest-1 loop
               B(I-AD_Position):= AD_Vector.Element(Index => I);
            end loop;
         end if;
      end Give_Bytes_AD;
      
      procedure Give_Bytes_Plaintext(B : out Bytes) is
         Curs : Vectors_Package.Cursor := Plaintext_Vector.First;
         Counter : Integer := 0;
      begin
--           Put_Line("Give_Bytes_Plaintext" & Integer'Image(Integer(Plaintext_Vector.Length)));
         
         while(Vectors_Package.Has_Element(Curs)) loop
            B(Counter):= Plaintext_Vector.Element(Vectors_Package.To_Index(Curs));
            Counter := Counter+1;
            Vectors_Package.Next(Curs);
         end loop;
         
      end Give_Bytes_Plaintext;
      
   begin
      
      Get_Bytes_Ciphertext(B => Ciphertext);
      Get_Bytes_AD(B => AD);
      
      Verfied:=
      Decrypt_And_Verify(This       	 => This,
                         Read_Ciphertext => Give_Bytes_Ciphertext'Unrestricted_Access,
                         Write_Plaintext => Get_Bytes_Plaintext'Unrestricted_Access,
                         Read_AD         => Give_Bytes_AD'Unrestricted_Access);
      
      Give_Bytes_Plaintext(B => Plaintext);
      
      return Verfied;
   end  Decrypt_And_Verify;
   
     
   
   
      


end Crypto.Symmetric.AE_OCB3;
