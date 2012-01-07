with Ada.Text_IO; use Ada.Text_IO;
with Crypto.Types; use Crypto.Types;
with Crypto.Random; use Crypto.Random;
with Crypto.Random_Source.File; use Crypto.Random_Source.File;

procedure Z is
   package BIO is new Ada.Text_IO.Modular_IO (Byte);
   package WIO is new Ada.Text_IO.Modular_IO (Word);
   
   B : Byte;
   Bs : Bytes(1..3);
   W : Word;
   
   Dev_Zero : Random_Source_File;
begin
   Dev_Zero.Initialize("/dev/zero");
   Read(B);
   Read(Bs);
   Read(W);
   
   Bio.Put(B, Base => 16); New_Line;
   Wio.Put(W, Base => 16); New_Line;
   
   Set(Dev_Zero);
   Read(B);
   Read(Bs);
   Read(W);
   
   Bio.Put(B, Base => 16); New_Line;
   Wio.Put(W, Base => 16); New_Line;
   
end Z;
