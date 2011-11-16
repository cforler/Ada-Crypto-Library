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
-- exception does not however invalidate any other reasons why tthe
-- executable file might be covered by the GNU Public License.

-- with Ada.Text_IO; use Ada.Text_IO;

package body Crypto.Types.Elliptic_Curves.Zp.Database is

   procedure Get_Elliptic_Curve(ECZ: out Elliptic_Curve_Zp; 
				ECP: out EC_Point;
				order: out Big_Unsigned;
				length: in Bit_Length) is
      temp : Precomputed_Elliptic_Curve;
      temp_length : Bit_Length;
      LEN_EX : exception;
   begin
      if(length < 6) then
	 temp_length := 5;
      elsif(length < 193) then
	 temp_length := 192;
      elsif(length < 225) then
	 temp_length := 224;
      elsif(length < 257) then
	 temp_length := 256;
      elsif(length < 385) then
	 temp_length := 384;
      elsif (length < 522) then
	 temp_length := 521;
      else
	 raise LEN_EX with "BitLength is not supported. (Max BitLength = 521)";
      end if;
      
      temp := ECD.Element(temp_length);
      ECZ.B := Big.Utils.To_Big_Unsigned(temp.b);
      ECZ.P := Big.Utils.To_Big_Unsigned(temp.p);
      if temp_length = 5 then
	 ECZ.A := Big.Utils.To_Big_Unsigned("11");
      else
	 ECZ.A := (ECZ.P - Big.Big_Unsigned_Three) mod ECZ.P;
      end if ;
      order := Big.Utils.To_Big_Unsigned(temp.r);
      ECP.X := Big.Utils.To_Big_Unsigned(temp.Gx);
      ECP.Y := Big.Utils.To_Big_Unsigned(temp.Gy);
      
      --		put_line("start");
      --		Big.Utils.Put_Line(ECZ.B);
      --		Big.Utils.Put_Line(ECZ.P);
      --		Big.Utils.Put_Line(ECZ.A);
      --		put_line("end");
      
   end;

   procedure Set_Elliptic_Curve_Map is
      p_5_test : Precomputed_Elliptic_Curve;
      p_192 : Precomputed_Elliptic_Curve;
      p_224 : Precomputed_Elliptic_Curve;
      p_256 : Precomputed_Elliptic_Curve;
      p_384 : Precomputed_Elliptic_Curve;
      p_521 : Precomputed_Elliptic_Curve;
   begin
      p_5_test.p(1..2)	:= "23";
      p_5_test.r(1..1)	:= "6";
      p_5_test.s(1..1) 	:= " ";
      p_5_test.c(1..1)	:= " ";
      p_5_test.b(1..2)	:= "12";
      p_5_test.Gx(1..1)	:= "6";
      p_5_test.Gy(1..1)	:= "8";
      p_5_test.length 	:= 5;
      ECD.Insert(p_5_test.length, p_5_test);

      p_192.p(1..58)	:= "6277101735386680763835789423207666416083908700390324961279";
      p_192.r(1..58)	:= "6277101735386680763835789423176059013767194773182842284081";
      p_192.s(1..44) 	:= "16#3045ae6fc8422f64ed579528d38120eae12196d5#";
      p_192.c(1..52)	:= "16#3099d2bbbfcb2538542dcd5fb078b6ef5f3d6fe2c745de65#";
      p_192.b(1..52)	:= "16#64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1#";
      p_192.Gx(1..52)	:= "16#188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012#";
      p_192.Gy(1..52)	:= "16#07192b95ffc8da78631011ed6b24cdd573f977a11e794811#";
      p_192.length 	:= 192;
      ECD.Insert(p_192.length, p_192);

      p_224.p(1..68)	:= "26959946667150639794667015087019630673557916260026308143510066298881";
      p_224.r(1..68)	:= "26959946667150639794667015087019625940457807714424391721682722368061";
      p_224.s(1..44) 	:= "16#bd71344799d5c7fcdc45b59fa3b9ab8f6a948bc5#";
      p_224.c(1..60)	:= "16#5b056c7e11dd68f40469ee7f3c7a7d74f7d121116506d031218291fb#";
      p_224.b(1..60) 	:= "16#b4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4#";
      p_224.Gx(1..60)	:= "16#b70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21#";
      p_224.Gy(1..60)	:= "16#bd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34#";
      p_224.length 	:= 224;
      ECD.Insert(p_224.length, p_224);

      p_256.p(1..78)	:= "115792089210356248762697446949407573530086143415290314195533631308867097853951";
      p_256.r(1..78)	:= "115792089210356248762697446949407573529996955224135760342422259061068512044369";
      p_256.s(1..44) 	:= "16#c49d360886e704936a6678e1139d26b7819f7e90#";
      p_256.c(1..68) 	:= "16#7efba1662985be9403cb055c75d4f7e0ce8d84a9c5114abcaf3177680104fa0d#";
      p_256.b(1..68)	:= "16#5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b#";
      p_256.Gx(1..68)	:= "16#6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296#";
      p_256.Gy(1..68)	:= "16#4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5#";
      p_256.length 	:= 256;
      ECD.Insert(p_256.length, p_256);

      p_384.p(1..116)	:= "39402006196394479212279040100143613805079739270465446667948293404245721771496870329047266088258938001861606973112319";
      p_384.r(1..116)	:= "39402006196394479212279040100143613805079739270465446667946905279627659399113263569398956308152294913554433653942643";
      p_384.s(1..44) 	:= "16#a335926aa319a27a1d00896a6773a4827acdac73#";
      p_384.c(1..100)	:= "16#79d1e655f868f02fff48dcdee14151ddb80643c1406d0ca10dfe6fc52009540a495e8042ea5f744f6e184667cc722483#";
      p_384.b(1..100)	:= "16#b3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef#";
      p_384.Gx(1..100)	:= "16#aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7#";
      p_384.Gy(1..100)	:= "16#3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f#";
      p_384.length 		:= 384;
      ECD.Insert(p_384.length, p_384);

      p_521.p(1..157)	:= "6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151";
      p_521.r(1..157)	:= "6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449";
      p_521.s(1..44) 	:= "16#d09e8800291cb85396cc6717393284aaa0da64ba#";
      p_521.c(1..135)	:= "16#0b48bfa5f420a34949539d2bdfc264eeeeb077688e44fbf0ad8f6d0edb37bd6b533281000518e19f1b9ffbe0fe9ed8a3c2200b8f875e523868c70c1e5bf55bad637#";
      p_521.b(1..135) 	:= "16#051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00#";
      p_521.Gx(1..134)	:= "16#c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66#";
      p_521.Gy(1..135)	:= "16#11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650#";
      p_521.length 	:= 521;
      ECD.Insert(p_521.length, p_521);
   end;

begin
   Set_Elliptic_Curve_Map;
end Crypto.Types.Elliptic_Curves.Zp.Database;


