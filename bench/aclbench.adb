-- Author: schmolli

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

--TODO: ! benchmarken
--TODO: ! Dokumentation
--TODO: ! vortrag
--TODO: ! poster
--TODO: ! papers
--TODO: ! schranken testen auf single core
--TODO: ? schranken granularer?
--TODO: ? Aufraumen und Kommentare entfernen


with Bench;                  use Bench;
with Bench.Big_Numbers;      --use Bench.Big_Numbers;
--with Test.ECDSA;				use Test.ECDSA;
--with Test.ECNR;				   use Test.ECNR;
--with Test.ECDH;           	use Test.ECDH;
--with Test.ECMQV;          	use Test.ECMQV;
--with Test.ECIES;          	use Test.ECIES;
--with Test.DSA;             use Test.DSA;
--with Test.RSA;             use Test.RSA;
--with Test.AES;             use Test.AES;
--with Test.SHA;             use Test.SHA;
--with Test.TDES;            use Test.TDES;
--with Test.RMAC;            use Test.RMAC;
--with Test.HMAC;            use Test.HMAC;
--with Test.Modes;           use Test.Modes;
--with Test.Twofish;         use Test.Twofish;
--with Test.Serpent;         use Test.Serpent;
--with Test.Blowfish;        use Test.Blowfish;
--with Test.Whirlpool;       use Test.Whirlpool;
--with Test.Oneway_Modes;    use Test.Oneway_Modes;
--with Test.Elliptic_Curves; use Test.Elliptic_Curves;

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Calendar;        use Ada.Calendar;
with Ada.Command_Line;
with Crypto.Types;        use Crypto.Types;

procedure aclBench is



   Start_All : Time ;
   Ende_All  : Time ;
   Start     : Time ;
   Ende      : Time ;
   Spanne    : Duration ;
   I         : Natural;
   CSV       : Boolean := False;

--   function Hash return Boolean is
--   begin
--      New_Line;
--      Put_Line("Hashfunction-Bench");
--      Put_Line("=================");
--      New_Line;
--      if Test_SHA1 and Test_SHA256 and Test_SHA384 and Test_SHA512 and
--        Test_Whirlpool then
--         return True;
--      else return False;
--      end if;
--   end Hash;

   ---------------------------------------------------------------------------

--   function AES return Boolean is
--   begin
--      New_Line;
--      Put_Line("AES-Bench");
--      Put_Line("--------");
--      if Test_AES128 
--         and Test_AES192 
--         and Test_AES256 
--         then return True;
--      else return False;
--      end if;
--   end AES;

   ---------------------------------------------------------------------------

--   function TDES return Boolean is
--   begin
--      New_Line;
--      Put_Line("TDES-Bench");
--      Put_Line("---------");
--      if Test_TDES and Test_Obsolete_TDES then  return True;
--      else return False;
--      end if;
--   end TDES;

   ---------------------------------------------------------------------------

--   function Twofish return Boolean is
--   begin
--      New_Line;
--      Put_Line("Twofish-Bench");
--      Put_Line("------------");
--      if Test_Twofish128 and Test_Twofish192 and Test_Twofish256 then
--         return True;
--      else return False;
--      end if;
--   end Twofish;

   ---------------------------------------------------------------------------

--   function Serpent return Boolean is
--   begin
--      New_Line;
--      if Test_Serpent256 then
--         return True;
--      else return False;
--      end if;
--   end Serpent;

   ---------------------------------------------------------------------------

--   function DSA return Boolean is
--   begin
--      New_Line;
--      Put_Line("DSA-bench");
--      Put_Line("--------");
--      return Test_DSA;
--   end DSA;

   ---------------------------------------------------------------------------

--   function Big(Size : Natural) return Boolean is
--        package Big is new Bench.Big_Numbers(Size);
--   begin
--      New_Line;
--      Put_Line("Big_Numbers-Bench");
--      Put_Line("================");
----      return Bench_Big_Numbers;
--      return Big.Bench_Big_Numbers;
--   end Big;

----   function RSA(Size : Natural) return Boolean is
--   function RSA return Boolean is
----      package RSA is new Bench.RSA(Size);
--   begin
--      New_Line;
--      Put_Line("RSA-Bench");
--      Put_Line("--------");
----      return RSA.Bench_RSA;
--      return Test_RSA;
--   end RSA;

   ---------------------------------------------------------------------------

--    function Modes return Boolean is
--    begin
--        New_Line;
--        Put_Line("Modes-Of-Operations-Bench");
--        Put_Line("========================");
--        if Test_CBC and Test_CFB and Test_CTR and Test_OFB then  return True;
--        else return False;
--        end if;
--    end Modes;

   ---------------------------------------------------------------------------

--   function Oneway_Modes return Boolean is
--   begin
--      New_Line;
--      Put_Line("Oneway-Modes-Of-Operations-Bench");
--      Put_Line("===============================");
--      if Test_Oneway_CFB and Test_Oneway_CTR and Test_Oneway_OFB then
--         return True;
--      else return False;
--      end if;
--   end Oneway_Modes;

   ---------------------------------------------------------------------------

--   function Big return Boolean is
----      package Big is new Bench.Big_Numbers(192);
--      package Big is new Bench.Big_Numbers(12800);
----      package Big is new Bench.Big_Numbers(32000);
----      package Big is new Bench.Big_Numbers(6400);
----      package Big is new Bench.Big_Numbers(512);
----      package Big is new Bench.Big_Numbers(128);
----      package Big is new Bench.Big_Numbers(64);
----        package Big is new Bench.Big_Numbers(32);
--   begin
--      New_Line;
--      Put_Line("Big_Numbers-Bench");
--      Put_Line("================");
----      return Bench_Big_Numbers;
--      return Big.Bench_Big_Numbers;
--   end Big;

   function Big(Size : Natural) return Boolean is
        package Big is new Bench.Big_Numbers(Size, CSV);
   begin
      if not CSV then
         New_Line;
         Put_Line("Big_Numbers-Bench");
         Put_Line("================");
      end if;
--      return Bench_Big_Numbers;
      return Big.Bench_Big_Numbers;
   end Big;

   ---------------------------------------------------------------------------

--   function EC return Boolean is
--   begin
--      New_Line;
--      Put_Line("Elliptic-Curves-Bench");
--      Put_Line("=====================");
--      if  Test_EC_Zp and  Test_EC_NSS_BF then
--         return True;
--      else
--         return False;
--      end if;
--   end EC;


   ---------------------------------------------------------------------------

--   function MACs return Boolean is
--      result: Boolean := true;
--   begin
--      New_Line;
--      Put_Line("Message-Authentification-Codes");
--      Put_Line("==============================");

--      if not  Test_Rmac then
--         Result := False;
--      end if;

--      New_Line;

--      if not  Test_HMAC_SHA1 then
--         Result := False;
--      end if;

--      New_Line;

--      if not  Test_HMAC_SHA256 then
--         Result := False;
--      end if;

--       New_Line;

--      if not  Test_HMAC_SHA512 then
--         Result := False;
--      end if;

--      New_Line;

--      if not Test_HMAC_Whirlpool then
--         Result := False;
--      end if;

--      return Result;

--   end MACs;

   ---------------------------------------------------------------------------

--   function Blowfish return Boolean is
--   begin
--      return Test_Blowfish128;
--   end Blowfish;

   ---------------------------------------------------------------------------



   Result : Boolean;

begin

   if Ada.Command_Line.Argument_Count = 1 then
      if Ada.Command_Line.Argument (1) = "-CSV" then CSV := True; end if;
   end if;
   
--   Put("CSV : ");  Put(Boolean'Image(CSV));

   Start_All := Clock;

--   New_Line;
--   Put_Line("Blockcipher-Bench");
--   Put_Line("================");

--   Start := Clock;
--   Result := AES;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration AES : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Serpent;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration SERPENT : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := TDES;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration TDES : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Twofish;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration Twofish : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Blowfish;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration Blowfish : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Oneway_Modes;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration Oneway_Modes : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Modes;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration Modes : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Hash;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration Hash : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := MACs;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration MACs : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := Big;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration BIG     : ");   Put(Duration'Image(Spanne));    New_Line;
   
   I := Mod_Type'Size;
   if CSV then Put_Line("Init_Length, Schul, Russ, Kara, Kara_P, TC, TC_P"); end if;
   while I <= 4096 loop
   Start := Clock;
--   loop
      Result := Big(I);
      I := I + Mod_Type'Size;
--      I := I + 128;
--      I := I + 512;
--      I := I + 1024;
--      I := I + 2048;
--      I := I + 64000;
   end loop;
   Ende := Clock;            Spanne := Ende - Start;
   Put("Duration BIG     : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := EC;
--   Ende := Clock;            Spanne := Ende - Start;
--   Put("Duration EC      : ");   Put(Duration'Image(Spanne));    New_Line;

--   New_Line;
--   Put_Line("Asymmetric-Bench");
--   Put_Line("================");

--   Start := Clock;
--   Result := DSA;
--   Ende := Clock;                  Spanne := Ende - Start;
--   Put("Duration DSA       : ");   Put(Duration'Image(Spanne));    New_Line;

--   Start := Clock;
--   Result := RSA;
--   Ende := Clock;                Spanne := Ende - Start;
--   Put("Duration RSA     : ");   Put(Duration'Image(Spanne));    New_Line;

--TODO: in Mod_Type umwandeln
--   I := 512;
--   while I <= 2048 loop
--      Start := Clock;
--      Result := RSA(I);
--      Ende := Clock;                Spanne := Ende - Start;
--      Put("Duration RSA     : ");   Put(Duration'Image(Spanne));    New_Line;
--      I := I + 512;
--   end loop;

   New_Line;                         New_Line;                       New_Line;
   Ende_All := Clock;                Spanne := Ende_All - Start_All;
   Put("Duration All         : ");   Put(Duration'Image(Spanne));    New_Line;
end aclbench;

