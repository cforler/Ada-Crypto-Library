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

with Ada.Text_IO;
use Ada.Text_IO;

package body Crypto.Symmetric.Mac is

   package WIO is new Ada.Text_IO.Modular_IO (Word);
   use WIO;

   procedure Fill36 (Word_Array  : out  Words) is
   begin
      Word_Array := (others => 16#36_36_36_36#);
   end Fill36;

   ---------------------------------------------------------------------------

   procedure Fill36 (DWord_Array  : out  DWords) is
   begin
      DWord_Array := (others => 16#36_36_36_36_36_36_36_36#);
   end Fill36;
   
   ---------------------------------------------------------------------------
   
   procedure Fill36 (W : out  W_Block512) is
   begin
      W := (others => 16#36_36_36_36#);
   end Fill36;
   
   ---------------------------------------------------------------------------
   
   procedure Fill36 (D : out  DW_Block512) is
   begin
      D := (others => 16#36_36_36_36_36_36_36_36#);
   end Fill36;
   
   
   ---------------------------------------------------------------------------
   
   procedure Fill36 (D : out  DW_Block1024) is
   begin
      D := (others => 16#36_36_36_36_36_36_36_36#);
   end Fill36;
   
   ---------------------------------------------------------------------------
   
   procedure Fill5C (Word_Array  : out  Words) is
   begin
      Word_Array := (others => 16#5C_5C_5C_5C#);
   end Fill5C;

   ---------------------------------------------------------------------------

   procedure Fill5C (DWord_Array  : out  DWords) is
   begin
      DWord_Array := (others => 16#5C_5C_5C_5C_5C_5C_5C_5C#);
   end Fill5C;

   ---------------------------------------------------------------------------
   
   procedure Fill5C (W : out W_Block512) is
   begin
      W := (others => 16#5C_5C_5C_5C#);
   end Fill5C;
   
   ---------------------------------------------------------------------------
      
    procedure Fill5C (W : out W_Block256) is
   begin
      W := (others => 16#5C_5C_5C_5C#);
   end Fill5C;
   ---------------------------------------------------------------------------

   procedure Fill5C (D : out  DW_Block512) is
   begin
      D := (others => 16#5C_5C_5C_5C_5C_5C_5C_5C# );
   end Fill5C;
      
   ---------------------------------------------------------------------------
   
   procedure Fill5C (D : out  DW_Block1024) is
   begin
      D := (others => 16#5C_5C_5C_5C_5C_5C_5C_5C# );
   end Fill5C;
   
   ---------------------------------------------------------------------------
  
   procedure Copy(Source : in Words; Dest : out Words) is
   begin
      if Dest'Length > Source'Length  then
         Dest(Source'Range) := Source;
         Dest(Source'Length..Dest'Last) := (others => 0);
      else
         Dest := Source(Dest'Range);
      end if;
   end Copy;

   ---------------------------------------------------------------------------

   procedure Copy(Source : in DWords; Dest : out DWords) is
   begin
      if Dest'Length > Source'Length  then
         Dest(Source'Range) := Source;
         Dest(Source'Length..Dest'Last) := (others => 0);
      else
         Dest := Source(Dest'Range);
      end if;
   end Copy;

   ---------------------------------------------------------------------------
   
   procedure Copy(Source : in W_Block160; Dest : out W_Block512) is
   begin
      Copy( Words(Source), Words(Dest));
   end Copy;
   
    ---------------------------------------------------------------------------
   
   procedure Copy(Source : in W_Block256; Dest : out W_Block512) is
   begin
      Copy( Words(Source), Words(Dest));
   end Copy;
   
   ---------------------------------------------------------------------------
   
   procedure Copy(Source : in DW_Block512; Dest : out DW_Block512) is
   begin
      Dest := Source;
   end Copy;
   
   ---------------------------------------------------------------------------
   
    procedure Copy(Source : in DW_Block512; Dest : out DW_Block1024) is
   begin
      Copy(DWords(Source), DWords(Dest));
   end Copy;
  
end Crypto.Symmetric.Mac;


