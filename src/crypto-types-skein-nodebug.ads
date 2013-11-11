------------------------------------------------------------------------
--
-- Implementation of the Skein hash function.
--
-- Source code author: Martin Kausche, 2009.
--
-- This source code is released to the public domain.
--
-- tested with gcc 4.2.4
------------------------------------------------------------------------
-- replace skein_nodebug with skein_debug if you want debug outputs on stdout
-- can be replaced in skein_adb, threefish.adb and skein_types.adb

package Crypto.Types.Skein.Nodebug is
     procedure Put(input      : in String);
    procedure Put_Line(input : in String);

    procedure Put(input              : in Integer;
                  Leading_Spaces     : in Boolean := true);
    procedure Put_Line(input         : in Integer;
                      Leading_Spaces : in Boolean := true);

    procedure Put_Error_Line(input : in String);
end Crypto.Types.Skein.Nodebug;
