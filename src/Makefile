VER=`head -n 1 ../VERSION`
LIBDIR=/usr/local/lib
INSTDIR=/usr/local/lib/libadacrypt-dev-$(VER)

GNATCLEAN=gnatclean
DEL=rm -f
DELDIR= rm -rf

CC=gnatmake 
CFLAGS=-c  -shared -gnato -s -O3 -gnatn -gnat12 -W -Wall -Werror

all: types asymmetric symmetric
gcov: CFLAGS=-f -c -shared -gnat12 -s -O0 -gnatE -gnatf -gnato -W -Wall -Werror -fprofile-arcs -ftest-coverage -fprofile-arcs -fno-inline
gcov: all
symmetric: hashfunction onewaybc mode mac blockcipher ae tweakable-blockcipher crypto-symmetric-utils
asymmetric: crypto-asymmetric-dsa crypto-asymmetric-rsa ec 
certificate: crypto-certificate
ec:  crypto-asymmetric-ecdsa crypto-symmetric-algorithm-ecdh crypto-symmetric-algorithm-ecies
hashfunction: crypto-symmetric-hashfunction_sha256 crypto-symmetric-hashfunction_sha512 crypto-symmetric-hashfunction_whirlpool
onewaybc: crypto-symmetric-oneway_blockcipher_whirlpool crypto-symmetric-oneway_blockcipher_twofish256 crypto-symmetric-oneway_blockcipher_twofish192  crypto-symmetric-oneway_blockcipher_twofish128 crypto-symmetric-oneway_blockcipher_tripledes crypto-symmetric-oneway_blockcipher_sha512 crypto-symmetric-oneway_blockcipher_sha384 crypto-symmetric-oneway_blockcipher_sha256 crypto-symmetric-oneway_blockcipher_sha1 crypto-symmetric-oneway_blockcipher_serpent256 crypto-symmetric-oneway_blockcipher_blowfish128 crypto-symmetric-oneway_blockcipher_aes256 crypto-symmetric-oneway_blockcipher_aes192 crypto-symmetric-oneway_blockcipher_aes128
mode: crypto-symmetric-mode-oneway_ofb crypto-symmetric-mode-oneway_ctr crypto-symmetric-mode-oneway_cfb crypto-symmetric-mode-ofb crypto-symmetric-mode-ctr crypto-symmetric-mode-cfb crypto-symmetric-mode-cbc crypto-symmetric-mode-bps
mac: crypto-symmetric-mac-rmac crypto-symmetric-mac-hmac_whirlpool crypto-symmetric-mac-hmac_sha512 crypto-symmetric-mac-hmac_sha256 crypto-symmetric-mac-hmac_sha1
blockcipher:  crypto-symmetric-blockcipher_twofish256 crypto-symmetric-blockcipher_twofish192 crypto-symmetric-blockcipher_twofish128 crypto-symmetric-blockcipher_tripledes crypto-symmetric-blockcipher_serpent256 crypto-symmetric-blockcipher_blowfish128 crypto-symmetric-blockcipher_aes256 crypto-symmetric-blockcipher_aes192 crypto-symmetric-blockcipher_aes128
tweakable-blockcipher: crypto-symmetric-tweakable_blockcipher_cmt crypto-symmetric-tweakable_blockcipher_tx
types: crypto-types-big_numbers crypto-types-elliptic_curves-ss_bf crypto-types-elliptic_curves-nss_bf crypto-types-mutexes crypto-types-nonces-nonces_ctr crypto-types-nonces-nonces_mixed_1 crypto-types-nonces-nonces_mixed_2  crypto-types-nonces-nonces_random crypto-types-nonces-nonces_randomized_counter 
ae: crypto-symmetric-aead_mcoe crypto-symmetric-aead_siv.adb crypto-symmetric-ae_ocb.adb

crypto-symmetric-algorithm-ecies:
	$(CC) $(CFLAGS) $@

crypto-symmetric-algorithm-ecdh:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_twofish256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_twofish192:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_twofish128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_tripledes:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_serpent256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_blowfish128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_aes256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_aes192:
	$(CC) $(CFLAGS) $@

crypto-symmetric-blockcipher_aes128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mac-hmac_sha512:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mac-hmac_sha256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mac-hmac_sha1:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mac-hmac_whirlpool:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mac-rmac:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-bps:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-cbc:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-cfb:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-ctr:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-ofb:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-oneway_cfb:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-oneway_ctr:
	$(CC) $(CFLAGS) $@

crypto-symmetric-mode-oneway_ofb:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_aes128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_aes192:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_aes256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_blowfish128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_serpent256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_sha1:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_sha256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_sha384:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_sha512:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_tripledes:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_twofish128:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_twofish192:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_twofish256:
	$(CC) $(CFLAGS) $@

crypto-symmetric-oneway_blockcipher_whirlpool:
	$(CC) $(CFLAGS) $@

crypto-types-big_numbers:
	$(CC) $(CFLAGS) $@

crypto-types-elliptic_curves-nss_bf:
	$(CC) $(CFLAGS) $@

crypto-types-elliptic_curves-ss_bf:
	$(CC) $(CFLAGS) $@

crypto-types-mutexes:
	$(CC) $(CFLAGS) $@

crypto-types-nonces-nonces_ctr:
	$(CC) $(CFLAGS) $@ 

crypto-types-nonces-nonces_mixed_1:
	$(CC) $(CFLAGS) $@

crypto-types-nonces-nonces_mixed_2:
	$(CC) $(CFLAGS) $@

crypto-types-nonces-nonces_random:
	$(CC) $(CFLAGS) $@

crypto-types-nonces-nonces_randomized_counter:
	$(CC) $(CFLAGS) $@

crypto-symmetric-hashfunction_whirlpool:
	$(CC) $(CFLAGS) $@

crypto-symmetric-hashfunction_sha512:
	$(CC) $(CFLAGS) $@

crypto-symmetric-hashfunction_sha256:
	$(CC) $(CFLAGS) $@

crypto-certificate:
	$(CC) $(CFLAGS) $@

crypto-asymmetric-rsa:
	$(CC) $(CFLAGS) $@

crypto-asymmetric-dsa:
	$(CC) $(CFLAGS) $@

crypto-asymmetric-ecdsa:
	$(CC) $(CFLAGS) $@

crypto-symmetric-aead_mcoe:
	$(CC) $(CFLAGS) $@

crypto-symmetric-aead_siv:
	$(CC) $(CFLAGS) $@

crypto-symmetric-ae_ocb:
	$(CC) $(CFLAGS) $@

crypto-symmetric-tweakable_blockcipher_cmt:
	$(CC) $(CFLAGS) $@

crypto-symmetric-tweakable_blockcipher_tx:
	$(CC) $(CFLAGS) $@

crypto-symmetric-utils:
	$(CC) $(CFLAGS) $@

.PHONY : clean
.SILENT : clean

install:
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp *.ads *.adb *.o $(INSTDIR)/

uninstall:
	$(DELDIR) $(INSTDIR)

clean:
	$(GNATCLEAN) *.o *.ali 
	$(DEL) *.gcno *.gcov *.gcda

#SHARED LIB

shared :
	gcc -shared -O3 -Wl,-soname,libadacrypt.so.1 -o libadacrypt.so.$(VER) \
	     crypto*.o

clean-shared:
	$(DEL) libadacrypt.so.$(VER)

install-shared : 
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp crypto*.ads crypto*.ali $(INSTDIR)/
	cp libadacrypt.so $(LIBDIR)/
	$(DEL) $(LIBDIR)/libadacrypt.so
	ln -s  $(LIBDIR)/libadacrypt.so.$(VER) $(LIBDIR)/libadacrypt.so
	ldconfig

uninstall-shared: 
	$(DEL) $(LIBDIR)/libadacrypt.so
	$(DEL) $(LIBDIR)/libadacrypt.so.$(VER)
	$(DEL) $(INSTDIR)/crypto*.ali
