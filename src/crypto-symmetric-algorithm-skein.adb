------------------------------------------------------------------------
--
-- Implementation of the Skein hash function.
--
-- Source code author: Martin Kausche, 2008.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------

--with skein_debug;               use skein_debug;
with Crypto.Types.Skein.Nodebug;             use Crypto.Types.Skein.Nodebug;
with Crypto.Symmetric.Algorithm.Threefish;
with Crypto.Types.Skein; use Crypto.Types.Skein;



package body Crypto.Symmetric.Algorithm.Skein is

    package threefish renames Crypto.Symmetric.Algorithm.Threefish;

    function Message_Bit_Padding(
            Message        : in Bytes;
            Desired_Length : in Natural) return Bytes is
        Local_Message : Bytes := Message;
    begin
        if Desired_Length/8 > Message'Length then
            -- the input message is to short and we would have to
            --fill more than one Byte
            Put_Line("The input-Message is shorter than the given" &
                "Message-length, please check!");
            Raise Program_Error;
        elsif Desired_Length/8 = Message'Length then
            --we have to do nothing here, the message already
            --has the desired length and we will use every Bit
            return Local_Message;
        elsif Desired_Length mod 8 = 0 then
            --if we have longer inputs, but the desired length is a multiple of
            --a full Byte, then we have to do nothing but send it back
            return Local_Message(0..(Desired_Length/8)-1);
        else
            --only parts of the input message will be used
            --the rest will be padded with one true-Bit in the
            --most significant not used Bit-Position
            --the remaining Bits will be set to false
            Set_Bit(b        => Local_Message((Desired_Length/8)),  --counting begins at 0
                    Position => 7- (Desired_Length mod 8),
                    Value    => True);
            for i in (Desired_Length mod 8)+1..7 loop
                Set_Bit(b    => Local_Message((Desired_Length/8)),  --counting begins at 0
                    Position => 7 - i,
                    Value    => false);
            end loop;

            --only return the Bytes we really want to use
            Put_Line("Desired_Lenght" & Integer'Image(Desired_Length));
            Put_Line("We returned from 0 to" & Integer'Image(Desired_Length/8));
            return Local_Message(0..Desired_Length/8);
        end if;
    end Message_Bit_Padding;

    function Get_Bit_Padding_Status(
            Message        : in Bytes;
            Desired_Length : in Natural) return Boolean is
    begin
        return not (Desired_Length/8 = Message'Length);
    end Get_Bit_Padding_Status;

    function Message_Byte_Padding(
            Mode             : in Skein_Mode;
            Original_Message : in Bytes) return Bytes is
        function Get_P(Original_Message : Bytes) return Natural is
        begin
            if Original_Message'Length = 0 then
                return Get_Number_Of_Bytes(Mode);
            elsif Original_Message'Length mod Get_Number_Of_Bytes(Mode) = 0 then
                return 0;
            else
                return Get_Number_Of_Bytes(Mode)
                       - (Original_Message'Length
                           mod Get_Number_Of_Bytes(Mode));
            end if;
        end Get_P;
        p : Natural := Get_P(Original_Message);
        Padded_Message : Bytes(0..Original_Message'Length-1+p);
    begin
        --write the original_message to the Padded Message
        Padded_Message(0..Original_Message'Length-1)
            := Original_Message;

        --Ada.text_IO.Put_Line("padding with p=" & Integer'Image(P));
        for i in reverse 1..p loop
            Padded_Message(Original_Message'Length-1 + i)
                := Byte(0);
            --Ada.Text_IO.Put(Show_Hex(Padded_Message(Original_Message'Last + i)) & " ");
        end loop;

        return Padded_Message;
    end Message_Byte_Padding;

    function Get_Current_Tweak(
            T_S       : in Bytes;
            N_M       : in Skein_Message_Length;    --Number of Bytes in Input Message
            Index     : in Natural;    --index of Message Block we are curently working on
            N_b       : in Natural;    --Number State-Bytes for the current mode
            First_Run : in Boolean;    --a_i
            Last_Run  : in Boolean;    --b_i
            B         : in Boolean)    --was there any BitPadding?
            return Bytes is
        Current_Tweak : Bytes := T_S;
    begin
        --first we just add the current already calculated bytes,
        --or if we are on the last word then we just add the number of bytes
        --at all (this only differs if the length of the input was not
        --a multiple of N_b
        --the first 96 Bits are reserved for this

        --we just use 2**64,
        --add min(N_M, (i+1)*N_b)
        --we really need to add here because in case of treehashing
        --there will already be some "countings" here
        if Skein_Message_Length( (Index+1)*N_b ) < N_M then
            Current_Tweak(0..7) := Current_Tweak(0..7)
                                    + (Index+1)*N_b;
            --Current_Tweak(0..7) := Natural_To_Bytes((Index+1)*N_b,8);
        else
            --Current_Tweak(0..7) := Skeinword_To_Bytes(Skeinword(N_M));
            Current_Tweak(0..7) := Current_Tweak(0..7)
                                    + Natural(N_M);
        end if;

        --if its the first computation in this UBI we set bit 126 in the 128Bit Tweak
        --this is the last but one in the last byte of the Tweak Block
        Set_Bit(Current_Tweak(Current_Tweak'Last),
                6,                  --bit 126 of the total
                First_Run);

        --if its the last computation in this UBI we set bit 127 in the 128 Bit Tweak
        --this is the last Bit in the last byte
        --if there was any BIT-Padding, we also set Bit 119 ---
        Set_Bit(Current_Tweak(Current_Tweak'Last),
                7,                  --bit 127 of the total
                Last_Run);


        Set_Bit(Current_Tweak(Current_Tweak'Last-1),
                7,                  --bit 119of the total
                Last_Run and B);

        --we are done and can return the value
        return Current_Tweak;
    end Get_Current_Tweak;


    procedure Straight_UBI(
            Mode                  : in     Skein_Mode;
            G                     : in     Bytes;    --starting value on N_B Bytes
            Full_Message          : in     Bytes;    --Message of variable lenght
            Full_Message_Bits     : in     Natural;  --the length of the input Message in Bits
            T_S                   : in     Bytes;    --Starting Tweak T_S of 16 Byte
            Result                :    out Bytes)   --the result of UBI:
    is
        Current_Tweak      : Bytes := T_S;  --we will manipulate this in every UBI-round
        Current_Message    : Bytes(0..Get_Number_Of_Bytes(Mode)-1);
                                            --the current 8,16,24 Bytes

        Current_Key        : Bytes := G;    --the "Keys" for the BlockCipher
                                            --as the first "Key" we use the G

        --here we store the current result of the block cipher
        --this is also holding the XOR of the blockcipher-result with message
        Current_Result     : Bytes := Current_Key;   --this is correct for the first step :)

        --we have to do a Bit-Padding if the input-length mod 8 != 0
        Bit_Padded_Message      : Bytes
                                := Message_Bit_Padding(
                                        Message         => Full_Message   ,
                                        Desired_Length  => Full_Message_Bits);
        --lets see if there was any Bit-Padding
        B                       : Boolean
                                := Get_Bit_Padding_Status(
                                        Message         => Full_Message   ,
                                        Desired_Length  => Full_Message_Bits);
        --do the padding for the Message, so that we have a length mod N_b = 0
        --we do this here because we do not know how long the message will be
        --so we can initialize the unconstrained type Bytes right here..
        Byte_Padded_Message     : Bytes := Message_Byte_Padding(
                                                Mode => Mode,
                                                Original_Message => Bit_Padded_Message);

        Bytes_Processed    : Natural := 0;  --counter for the processed Bytes, we need this for the
                                            --tweak-calculation and to know when to finisch
        N_M : Natural := 0;
    begin
        --check if G has the correct Size
        if not (G'Length = Get_Number_Of_Bytes(Mode)) then
            Put_Line("This is the wrong input size or mode, please check.");
            Put_Line(Integer'Image(G'Length) & " vs." & Integer'Image(Get_Number_Of_Bytes(Mode)));
            Raise Program_Error;
        end if;

        N_M := Full_Message'Length;


        -- divide the Message to little pieces and process this
        while true loop
            Current_Tweak := Get_Current_Tweak(
                T_S       => T_S,
                N_M       => Skein_Message_Length(N_M),               --we normaly dont need a special variable here!!!
                Index     => Bytes_Processed / Get_Number_Of_Bytes(Mode),
                N_b       => Get_Number_Of_Bytes(Mode),
                First_Run => Bytes_Processed = 0,
                Last_Run  => Full_Message'Length - Bytes_Processed <= Get_Number_Of_Bytes(Mode),
                B         => B);

            Current_Message := Byte_Padded_Message(Bytes_Processed..Bytes_Processed+Get_Number_Of_Bytes(Mode)-1);

            Current_Key     := Current_Result;

--            Put_Line("We are starting threefish now "
--                & Integer'Image(Current_Tweak'Length)
--                & Integer'Image(Current_Message'Length)
--                & Integer'Image(Current_Key'Length)
--                );
            --now we have the tweak,key and ne message and can use the blockcipher
            threefish.encrypt(
                Mode             => Mode,
                Block_Cipher_Key => Current_Key,
                Tweak            => Current_Tweak,
                Plaintext        => Current_Message,
                Result           => Current_Result);

            --as a final step lets XOR the result with the message
            for i in Current_Result'Range loop
                Current_Result(i) := Current_Result(i) xor Current_Message(i);
            end loop;

            --increment the counter for the next run
            Bytes_Processed := Bytes_Processed + Get_Number_Of_Bytes(Mode);

            --if this was the last block we can finish this here
            exit when Bytes_Processed = Byte_Padded_Message'Length;
        end loop;
--        Ada.Text_IO.Put_Line("Result: " & Integer'Image(Result'Length)
--                             & "         Current_Result: " & Integer'Image(Current_Result'Length));

        --maybe we should do:
        --for i in 0..Current_Result'Last loop
        --  Result(Result'First+i) := Current_Result(i);
        --end loop;
        Result := Current_Result;

        Put_Line("UBI is done for:"
            & "  G'Length:" & Integer'Image(G'Length)
            & "  M'Length:" & Integer'Image(Full_Message'Length)
            & "  Bit_Padded_Message'Length:" & Integer'Image(Bit_Padded_Message'Length));
    end Straight_UBI;

    task body Tree_UBI_Task
        is
        Local_Mode                  : Skein_Mode;
        Local_G                     : Bytes(0..Get_Number_Of_Bytes(Mode)-1);
        Local_Full_Message          : Bytes(0..Longest_Message_Bytes-1);
        Local_Full_Message_Length   : Natural;
        Local_T_S                   : Bytes(0..15);
        Local_Result_Access         : Bytes_Access;
        Local_Result_First          : Natural;
        Local_Result_Last           : Natural;
        --Local_Result                : Bytes(0..Get_Number_Of_Bytes(Mode)-1);
        --since we have to save a larger "internally" Full Message, we need to save
        --the real size of the Full_message here too
        Full_Message_First          : Natural;
        Full_Message_Last           : Natural;

        --our local pointer to the Message length counter
        Local_Length_Access        : Skein_Tree_Message_Length_Counter_Access;
    begin
        loop
            select
                accept compute( Mode                : in     Skein_Mode;
                                G                   : in     Bytes;
                                Full_Message        : in     Bytes;
                                Full_Message_Length : in     Natural;
                                T_S                 : in     Bytes;
                                Result_Access       : in out Bytes_Access;
                                Result_First        : in     Natural;
                                Result_Last         : in     Natural;
                                Length_Access       : in out Skein_Tree_Message_Length_Counter_Access)
                        do

                    Local_Mode                  := Mode;
                    Local_G                     := G;
                    Full_Message_First          := Full_Message'First;
                    Full_Message_Last           := Full_MEssage'Last;

                    Put_Line("Local_Full_Message'Range: " & Integer'Image(Local_Full_Message'First)
                        & ".." &  Integer'Image(Local_Full_Message'Last));
                    Put_Line("Full_Message'Range: " & Integer'Image(Full_Message_First)
                        & ".." &  Integer'Image(Full_Message_Last));

                    Local_Full_Message(Full_Message_First .. Full_Message_Last)
                                                := Full_Message;
                    Local_Full_Message_Length   := Full_Message_Length;
                    Local_T_S                   := T_S;
                    Local_Result_Access         := Result_Access;

                    Local_Result_First          := Result_First;
                    Local_Result_Last           := Result_Last;

                    Local_Length_Access         := Length_Access;

                end compute;

                --now we can compute
                Straight_UBI(
                    Mode                => Local_Mode,
                    G                   => Local_G,
                    Full_Message        => Local_Full_Message(Full_Message_First .. Full_Message_Last),
                    Full_Message_Bits   => Local_Full_Message_Length,
                    T_S                 => Local_T_S,
                    Result              => Local_Result_Access.all(Local_Result_First..Local_Result_Last)
                    );

                Local_Length_Access.all.Increase(Value => Get_Number_Of_Bytes(Mode));

            or terminate;
            end select;
        end loop;
    end Tree_UBI_Task;

    procedure Tree_UBI(
            Mode                  : in     Skein_Mode;
            G                     : in     Bytes;    --starting value on N_B Bytes
            Full_Message          : in     Bytes;    --Message of variable lenght
            Full_Message_Bits     : in     Natural;  --the length of the input Message in Bits
            T_S                   : in     Bytes;    --Starting Tweak T_S of 16 Byte
            Y_l                   : in     Natural;  --loaf-size for treemode
            Y_f                   : in     Natural;  --node-size for treemode
            Y_M                   : in     Natural;  --max tree height
            Result                :    out Bytes;  --the result of UBI:
            Number_Of_Tasks       : in     Natural := 2)
        is

        N_b     : Natural := Get_Number_Of_Bytes(Mode); --this is in BYTES
        --leaf size N_l= N_b* 2**Y_l BITS
        N_l     : Natural
                := N_b * 2**Y_l;
        --node size N_n = N_b * 2**Y_f BITS
        N_n     : Natural
                := N_b * 2**Y_f;

        N       : Natural := 0;     --we use this inside the tree
                                    --for leaf level it is N_l otherwise N_n

        --we need to know the max. length an Message can have, so we can initialize
        --the task correctly
        UBI_Tasks : array(0..Number_Of_Tasks) of Tree_UBI_Task(Mode, Full_Message'Length);
                                                                        --this is much more than we really need
                                                                        --needs to be done better!!

        M_old   : Bytes := Full_Message;
        M_new   : Bytes := Full_Message; --we will use only parts of this

        --we need these variables if we use multiple task
        M_new_Access                   : Bytes_Access;
        M_new_Protected_Length_Access  : Skein_Tree_Message_Length_Counter_Access
                                       := new Skein_Tree_Message_Length_Counter;

        Tree_Level_Is_Done : Boolean := false;

        M_New_Length : Natural := 0;     --we need to know till which Bits we can use the M_new



        --we will use this variable to store the temporary data
        G_Temp  :Bytes(0..N_b-1)
                := (others => Byte(0));

        --we need a variable which increments
        l       : Natural := 0;


        --we need to know how many pieses there are
        Last_Piece : Natural := 0;

        Current_Full_Length_Bits : Natural := Full_Message_Bits; --the length of the full current message M_l
        Current_Length_Bits : Natural := 0;  --the length of the current Block M_l,i (most time this will be N*8)

        Current_Tweak : Bytes(0..15) := (others => Byte(0));    --we use this for the current Tweak always
    begin
        Put_Line("N_l=" & Integer'Image(N_l) & "     N_n=" & Integer'Image(N_n));
        --split M in Blocks, each block has 8*N_l Bits
        --the last Block has between 1 and 8*N_l bits

        while true loop
            --build M_{l+1} = M_new over and over again
            -- -- cut into smaler pieces
            -- --concartinate and compute again
            --for the first run use N_l, later use N_n
            --manipulate the Tweak in every run...

            --exit if M_new'Size = N_b --> Result := M_new
            --
            --if max level is reached compute one last Result := UBI(G,M_new,T);
            --and exit

            --we can write the data always to the first bytes of M_new
            --we have be careful with sending slices to the UBI Procedure
            --because indexes will be kept :(
            if l = 0 then
                N := N_l;       --these are Byte-lengths
                --Ada.Text_IO.Put_Line("::we are doing the first run with the leaf size");
            else
                N := N_n;
                --Ada.Text_IO.Put_Line("::we are doing one of the other runs with the node size");
            end if;
            --cut the message into pieces of size 8*N Bits
            --these are N Bytes
            --> only the last block will be between 1 and 8*N Bits
            --how many pieced will that be?
            if Current_Full_Length_Bits mod (N*8) = 0 then   --our input is a multiple of the node/leaf size
                Last_Piece := (Current_Full_Length_Bits/8) / N -1; --index starts at zero
            elsif Current_Full_Length_Bits < N*8 then      --our message is shorter than a single leaf would be
                Last_Piece := 0;                    --there is only one leaf
            else                                    --our message_length_bits is not a multiple of N*8 AND longer than N*8
                Last_Piece := (Current_Full_Length_Bits/8) / N;
            end if;

            --fill the Bytes pointer with current data
            --and reset the Message Length counter for the current tre-level
            M_new_Access := new Bytes(M_new'Range);
            M_New_Access.all := M_New;
            M_new_Protected_Length_Access.all.Reset;
            M_new_Protected_Length_Access.all.Set_Final_Length(Value => (Last_Piece+1)*N_b);



            for i in 0..Last_Piece loop
                Put_LIne("i= " & Integer'Image(i));

                --the current M_l,i is N Bytes long
                --from i*N..(i+1)*N -1 -- the old M_l is M_old
                --it will be written to position: i*N_b..(i+1)*N_b -1 of M_l+1 which is M_new always
                --we also need to save the "last" Bit, so we know which is the current length
                --of M_new
                if (i = Last_Piece) and not (Current_Full_Length_Bits mod (N*8) = 0) then
                    Current_Length_Bits := Current_Full_Length_Bits mod (N*8);
                else
                    Current_Length_Bits := N*8;
                end if;

                --calculate the Tweak for the current run
                --i*N_m + (l+1)*2^112
                Current_Tweak := T_S;
                Current_Tweak(0..7) := Natural_To_Bytes(i*N,8);
                Current_Tweak(14) := Byte(l+1);

                --which parts of the input message do we need?

                Put_Line("Current_Length_Bits: " & Integer'Image(Current_Length_Bits));
                Put_Line("inmessage: " & Integer'Image(i*N) & " to" & Integer'Image(i*N + Current_Length_Bits/8 -1));
                Put_Line("outmessage: " & Integer'Image(i*N_b) & " to" & Integer'Image((i+1)*N_b -1));
--                Straight_UBI(
--                    Mode => Mode,
--                    G => G,
--                    --Full_Message => M_old(i*N..(i+1)*N -1),
--                    Full_Message => M_old(i*N..i*N + Current_Length_Bits/8 -1),
--                    Full_Message_Bits => Current_Length_Bits,
--                    T_S => Current_Tweak,     --we need to add: i*N_m + (l+1)*2^112 here!!!!!
--                    Result => M_new(i*N_b..(i+1)*N_b -1)
--                    );
                M_New_Length := M_New_Length + N_b;

                --the Task implementation of tree-UBI
                UBI_Tasks(i mod Number_Of_Tasks).compute(
                    Mode                => Mode,
                    G                   => G,
                    --Full_Message => M_old(i*N..(i+1)*N -1),
                    Full_Message        => M_old(i*N..i*N + Current_Length_Bits/8 -1),
                    Full_Message_Length => Current_Length_Bits,
                    T_S                 => Current_Tweak,     --we need to add: i*N_m + (l+1)*2^112 here!!!!!
                    Result_Access       => M_new_Access,
                    Result_First        => i*N_b,
                    Result_Last         => (i+1)*N_b -1,
                    Length_Access       => M_new_Protected_Length_Access
                    );


            end loop;
            --we are outside the loop and can check if we are done,
            --if not we will do another round after incrementing l
--            while true loop
                M_new_Protected_Length_Access.all.Is_Final_Length_Reached(Tree_Level_Is_Done);
--                exit when Tree_Level_Is_Done;
--            end loop;
            --check if all tasks are done


            --we could check if M_New_Length = (Last_Piece+1)*N_b
            --delay 10.0;
            --write back the Pointer Data to the variable
            M_New := M_New_Access.all;




            if M_New_Length = N_b then
                --Ada.Text_IO.Put_Line("We just give back from 0 to " & Integer'Image(N_b-1));
                --the result of treehashing is this M_new
                Result := M_new(0..N_b-1);
                exit when true;
            elsif l = Y_m -1 then
                --we reached the maximum tree level
                --use the other formula to compute G_0, which is the result

                --calculate the Tweak for the current run
                --we need to add Y_m*2^112 here
                Current_Tweak := T_S;
                Current_Tweak(14) := Byte(Y_m);

                Straight_UBI(
                    Mode => Mode,
                    G => G,
                    Full_Message => M_New(0..M_New_Length-1),
                    Full_Message_Bits => M_New_Length*8,
                    T_S => Current_Tweak,
                    Result => Result
                    );
                exit when true;
            end if;

            -- we could not exit, so we have to set the "new_Message" as the old one
            --and do one more round
            Put_Line(" ");
            Put_Line("The resulting  Message M_l after l=" & Integer'Image(l));
            for i in 0..M_New_Length loop
                Put(To_Hex(M_New(i)));
                Put(" ");
                if (i+1) mod 16 = 0 then
                    Put_LIne(" ");
                end if;
            end loop;
            Put_Line(" ");

            --we can do this because we are at least in the second tree-level
            --here the bit-length is always Byte-lenght*8
            Current_Full_Length_Bits := M_New_Length*8;
            M_Old := M_new;
            M_New_Length := 0;
            M_New := (others => Byte(0));

            l := l +1;  --go to the next tree level
            null;
        end loop;
        null;
    end Tree_UBI;


    procedure Output(
            Mode       : in     Skein_Mode;
            G          : in     Bytes;      --the chaining value
            N_0        : in     Natural;    --number of required output BITS
            Result     :    out Bytes)     --the result, if N_0 mod 8 != 0
                                            --the last byte is only partially used
        is
        --we need at least the size for the current Mode
        Local_Result : Bytes(0..N_0/8 + Get_Number_Of_Bytes(Mode));  --build N_b bytes more, we cut it later..
        Counter : Natural := 0;
        T_out : Bytes(0..15)
                     := (15 => Byte(63),
                         others => Byte(0));
    begin
        --we use the first N_0/8 Bytes
        while true loop
            Straight_UBI(Mode,
                G => G,
                Full_Message => Natural_To_Bytes(Counter,8),
                Full_Message_Bits => 8*8,
                T_S => T_out,          --this is T_{out}* 2^{120}
                Result => Local_Result(Counter*Get_Number_Of_Bytes(Mode)
                                        .. Counter*Get_Number_Of_Bytes(Mode) + Get_Number_Of_Bytes(Mode) -1)
                );

            Counter := Counter + 1;
            exit when Counter*Get_Number_Of_Bytes(Mode) >= N_0/8;
        end loop;

        --we just need the first N_0/8 Bytes
        if N_0 mod 8 = 0 then
            Result := Local_Result(0..N_0/8 - 1);
        else
            Result := Local_Result(0..N_0/8);       --one more byte
            --the user has to clearify whisch Bits he really wants to us
            --output are full Bytes always, never single Bits
        end if;
    end Output;

    function Get_Configuration_String(
            N_0 : in Natural;
            Y_l : in Natural := 0;
            Y_f : in Natural := 0;
            Y_m : in Natural := 0)
        return Bytes is
        Conf_String : Bytes(0..31)
                    := (others => Byte(0));
    begin
        --shema identifier ASCII-String "SHA3" in Bytes 0..3
        Conf_String(3) := Byte(16#33#);
        Conf_String(2) := Byte(16#41#);
        Conf_String(1) := Byte(16#48#);
        Conf_String(0) := Byte(16#53#);
        --Conf_String(0..3) := Natural_To_Bytes(16#33414853#,4);

        --the version number
        Conf_String(4..5) := Natural_To_Bytes(1,2);

        --6..7 reserved - willbe zero for now

        --8..15 output-length in Bits
        Conf_String(8..15) := Natural_To_Bytes(N_0, 8);

        -- 16 tree leaf size
        Conf_String(16..16) := Natural_To_Bytes(Y_l,1);
        -- 17 tree fan out enc.
        Conf_String(17..17) := Natural_To_Bytes(Y_f,1);
        -- 18 max tree height
        Conf_String(18..18) := Natural_To_Bytes(Y_m,1);

        -- 19..31 reserved, set to zero

        return Conf_String;
    end Get_Configuration_String;

    --the full Init call
    procedure Init(
            Mode : in     Skein_Mode;   --the Mode we want to use Skein (internal size)
            N_0  : in     Natural;      --the desired output length in Bits
            K    : in     Bytes ;       --the Key for MAC-Mode
            Y_l  : in     Natural;      --tree hashing leaf size
            Y_f  : in     Natural;      --tree hashing fan-out
            Y_m  : in     Natural;      --tree hashing maximim tree height
            State:    out Bytes         --the outputof the init process
            ) is
        --initialize K_Tick, we will set it to all-zero
        --or some value depending on K
        Empty_G: Bytes(0..Get_Number_Of_Bytes(Mode)-1)
                := (others => Byte(0));
        K_Tick : Bytes(0..Get_Number_Of_Bytes(Mode)-1);
        T_Key : Bytes(0..15) := (others => Byte(0));

        --data used for the Configuration calculations
        C      : Bytes(0..31)
               := Get_Configuration_String(N_0 => N_0,
                                           Y_l => Y_l,
                                           Y_f => Y_f,
                                           Y_m => Y_m);
        T_Conf : Bytes(0..15)
               := (15 => Byte(4), others => Byte(0));
    begin
        --calculate K_Tick
        Put_Line(":::::::::::Calculation of K_Tick started:::::::::::");
        if K'Length = 0 then                --This isnt correct, this is not possible in Ada.
            Put_Line("empty Key found");
            K_Tick := (others => Byte(0));
        else
            Straight_UBI(
                Mode                => Mode,
                G                   => Empty_G,
                Full_Message        => K,
                Full_Message_Bits   => K'Length * 8,
                T_S                 => T_Key,
                Result              => K_Tick);
        end if;

        --now we have K_Tick and can go on with the Configuration calculation
        Put_Line(":::::::::::Calculation of Configuration Block started:::::::::::");
        Straight_UBI(
            Mode                => Mode,
            G                   => K_Tick,
            Full_Message        => C,
            Full_Message_Bits   => 32*8,        --this is fix
            T_S                 => T_Conf,
            Result              => State);
    end Init;

    --the Simple Init
    --we need to implement it this way because a default value
    --for the key can not be realized in the full-Init
    procedure Init(
            Mode : in     Skein_Mode;
            N_0  : in     Natural;
            State:    out Bytes) is
        K : Bytes(0..-1) := (others => Byte(0));
    begin
        --call the "long" Init
        Init(
            Mode => Mode,
            N_0  => N_0,
            K    => K ,
            Y_l  => 0,
            Y_f  => 0,
            Y_m  => 0,
            State=> State);
    end Init;

    procedure Update(
            Mode            : in     Skein_Mode;
            Old_State       : in     Bytes;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Type_Value      : in     Byte;
            Y_l             : in     Natural;
            Y_f             : in     Natural;
            Y_m             : in     Natural;
            New_State       :    out Bytes) is
        Tweak  : Bytes(0..15)
               := (15 => Type_Value,
                    others => Byte(0));
        --we need this Tweak to compare it with the input-tweak
        --only if the input-tweak is the T_msg and at least one Y != 0
        --then we use tree-hashing
        T_Msg  : Bytes(0..15)
               := (15 => Byte(48),
                   others => Byte(0));
    begin
        Put_Line(":::::::::::Calculation of Update-Block started:::::::::::");
        --!!!!!!!!!!! we have to differ between normal UBI and TreeUBI here
        if Tweak = T_Msg
        and ((not(Y_l = 0)
                or not(Y_f = 0)
                or not(Y_m = 0)))
        then
            null;
            --we want treehasing
            Put_Line("using Tree-hashing");
            Tree_UBI(
                    Mode                  => Mode,
                    G                     => Old_State,
                    Full_Message          => Message,    --Message of variable lenght
                    Full_Message_Bits     => Message_Length,  --the length of the input Message in Bits
                    T_S                   => Tweak,    --Starting Tweak T_S of 16 Byte
                    Y_l                   => Y_l,
                    Y_f                   => Y_f,
                    Y_M                   => Y_M,
                    Result                => New_State);

        else
            --we want straight UBI
            Straight_UBI(
                Mode                => Mode,
                G                   => Old_State,
                Full_Message        => Message,         --the current message
                Full_Message_Bits   => Message_Length,  --the current message length in bits
                T_S                 => Tweak,           --the current tweak
                Result              => New_State);
        end if;
    end Update;

    --the simplified Update function
    procedure Update(
            Mode            : in     Skein_Mode;
            Old_State       : in     Bytes;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            New_State       :    out Bytes) is
        Type_Value_Msg  : Byte := Byte(48);
    begin
        --call the full Update, with the parameters set to defaults
        --we need to do it this way because a default of T_Msg isnt possible
        Update(
                Mode            => Mode,
                Old_State       => Old_State,
                Message         => Message,
                Message_Length  => Message_Length,
                Type_Value      => Type_Value_Msg,
                Y_l             => 0,
                Y_f             => 0,
                Y_m             => 0,
                New_State       => New_State);
    end Update;

    procedure Final(
            Mode        : in     Skein_Mode;
            Old_State   : in     Bytes;
            N_0         : in     Natural;
            New_State   :    out Bytes) is
        --the final result , the size is correct only for N_0 mod 8 = 0!!
        H      : Bytes(0..N_0/8-1) := (others => Byte(0));
    begin
        Put_Line(":::::::::::Calculation of Output-function started:::::::::::");
        --as a last step we do the Output-function
        Output( Mode    => Mode,
                G       => Old_State,
                N_0     => N_0,
                Result  => H);

        --fill the output byte per Byte with the result
        for i in New_State'Range loop
            New_State(i) := H(i);
        end loop;
    end Final;

    procedure Hash(
            Mode        : in     Skein_Mode;
            N_0         : in     Natural;
            K           : in     Bytes;
            Y_l         : in     Natural;
            Y_f         : in     Natural;
            Y_m         : in     Natural;
            Tuple_Array : in     Skein_Message_Tweak_Tuple_Pointer_Array;
            Result      :    out Bytes) is
        Initresult  : Bytes(0..Get_Number_Of_Bytes(Mode)-1)
                    := (others => Byte(0));
        Updateresult: Bytes(0..Get_Number_Of_Bytes(Mode)-1)
                    := (others => Byte(0));
    begin
        Init(Mode   => Mode,
            N_0     => N_0,
            K       => K,
            Y_l     => Y_l,
            Y_f     => Y_f,
            Y_m     => Y_m,
            State   => Initresult);

        for i in Tuple_Array'Range loop
            Update(Mode         => Mode,
                Old_State       => Initresult,
                Message         => Tuple_Array(i).all.Message,
                Message_Length  => Tuple_Array(i).all.Message_Length,
                Type_Value      => Tuple_Array(i).all.Type_Value,
                Y_l             => Y_l,
                Y_f             => Y_f,
                Y_m             => Y_m,
                New_State       => Updateresult);
        end loop;

        Final(Mode => Mode,
            Old_State => UpdateResult,
            N_0 => N_0,
            New_State => Result);
    end Hash;

    procedure Hash(
            Mode            : in     Skein_Mode;
            N_0             : in     Natural;
            K               : in     Bytes;
            Y_l             : in     Natural;
            Y_f             : in     Natural;
            Y_m             : in     Natural;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Type_Value      : in     Byte;
            Result          :    out Bytes) is
        Initresult  : Bytes(0..Get_Number_Of_Bytes(Mode)-1)
                    := (others => Byte(0));
        Updateresult: Bytes(0..Get_Number_Of_Bytes(Mode)-1)
                    := (others => Byte(0));
    begin
        Init(Mode => Mode,
            N_0     => N_0,
            K       => K,
            Y_l     => Y_l,
            Y_f     => Y_f,
            Y_m     => Y_m,
            State   => Initresult);

        Update(Mode => Mode,
            Old_State       => Initresult,
            Message         => Message,
            Message_Length  => Message_Length,
            Type_Value      => Type_Value,
            Y_l             => Y_l,
            Y_f             => Y_f,
            Y_m             => Y_m,
            New_State       => Updateresult);

        Final(Mode      => Mode,
            Old_State   => UpdateResult,
            N_0         => N_0,
            New_State   => Result);
    end Hash;

    procedure Hash(
            Mode            : in     Skein_Mode;
            N_0             : in     Natural;
            Message         : in     Bytes;
            Message_Length  : in     Natural;
            Result          :    out Bytes) is
        K_empty : Bytes(0..-1) := (others => Byte(0));
        Type_Value_Msg  : Byte := Byte(48);
    begin
        --just call the full Hash with empty Key
        --and tree vaiables set to zero
        Hash(
            Mode            => Mode,
            N_0             => N_0,
            K               => K_empty,
            Y_l             => 0,
            Y_f             => 0,
            Y_m             => 0,
            Message         => Message,
            Message_Length  => Message_Length,
            Type_Value      => Type_Value_Msg,
            Result          => Result);

    end Hash;


begin

	null;
end Crypto.Symmetric.Algorithm.Skein;
