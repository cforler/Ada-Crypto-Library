Welcome to the Ada Crypto Library (ACL) aka libadacrypt
=======================================================
This library is an ongoing project that provides the Ada community to
strong cryptography primitives and schemes, focused to ensure data
confidentiality and data integrity.


Disclaimer
-----------
Note that the ACL is an ongoing academic projects. Several parts of
the code is written by students as part of a semester project or a
(bachelor/master) thesis. Usually, those students are neither ADA
professionals nor cryptographic experts. Thus, use this project with
caution.


Implemented Features
-----------------------
 - Symmetric Cryptography
   - Blockciphers: AES, Twofish, 3DES, Serpent
   - Modes of Operation : BPS, CFB, Ctr, OFB 
   - Tweakable Blockcipher Modes:  CMT, XT
   - Hash functions: SHA-1 (broken), SHA-256, (SHA-384), SHA-512, Whirlpool
   - MACs: RMAC, HMAC, CMAC
   - Authenticated Encryption schemes: OCB, SIV and McOE
   - Key Derivation Functions: PBKDF2, scrypt, and SHA-512crypt
 - Big (unsigned) number library
   - Primary cyclic group arithmetic  (Z_p)
   - Binary Field arithmetic support.
   - Elliptic Curve arithmetic
      - Supersingular Elliptic Curves Over Binary Fields  (SS-BF)
      - Non-Supersingular Elliptic Curves Over Binary Fields (NSS-BF)
      - Elliptic Curves Over Z_p (EC-Z_P)
 - Asymmetric Cryptography
   - Probabilistic primality testing
   - DSA signature scheme
   - OEAP-RSA 
   - ECDSA, ECDH
 - Nonce Generator Support: Random, Counter, Mixed
 - AUnit-3.7.1 based Test suite (over 400 tests, about 90% line coverage)


Getting started
-----------------
### Prerequisites 
 * GNAT-4.8 (recommended version: 4.9)
 * GNU Make (recommended version: 4)
 * gcov (recommended version: 4.8.3; optional for testing only)
 * lcov (recommended version: 1.10;  optional for testing only)


### To build and test:
    make
    make acltest
    cd test  
    ./test-tests
	

### To generate LCOV code coverage report (location: test/coverage/):
    make gcov
    cd test  
    ./test-tests
    make lcov
    


### To build the PDF documentation (location: doc/):
    make docu


AUNIT support
-----------
This crypto library assumes that the aunit source PATH is
"/usr/share/ada/adainclude/aunit/". If needed, you should change this
path in the project file acltest.gpr. In additon, you can cahnge the PATH in
test/Makefile.


Other stuff
-----------
### Contact
If you want to report bugs, make suggestions, contribute bugfixes or
beautiful ideas, feel free to contact me at cforler(at)posteo.de


### Legal Note
Note, that depending on where you are, the use of cryptography may be
limited or forbidden by law. Before using this library, make sure you
are legally entitled to do so.

### License 
GNAT Modified General Public License
--------------------------------------
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

As a special exception, if other files instantiate generics from
this unit, or you link this unit with other files to produce an
executable, this unit does not by itself cause the resulting
executable to be covered by the GNU General Public License. This
exception does not however invalidate any other reasons why the
executable file might be covered by the GNU Public License.

Read more about this license at http://en.wikipedia.org/wiki/GNAT_Modified_General_Public_License 