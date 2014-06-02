GNATCLEAN=gnatclean
DEL=rm -f
BB = gnatbind
LL = gnatlink
CC = gnatmake	
GEN_HTML = tools/gen_coverage_page.sh
AUNIT_DIR = aunit-3.4/src

CFLAGS = -W -Werror -ggdb -g -gnat12 -gnato -I../src -I$(AUNIT_DIR) -O3 -gnatn -gnatwrclp
BFLAGS = -shared -aO./ -aO/lib/ -I$(AUNIT_DIR) -aO../src -I- -x
LFLAGS = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src -lacl
LFLAGS = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src


all : aunit big_number test asymmetric hash symmetric kdf


#enable for use of GCOV:
gcov: CFLAGS = -O0 -gnatwrclp  -I../src  -I../src -I$(AUNIT_DIR) -gnat12 -gnatE -gnatf -gnato -W -Wall -Werror -fprofile-arcs -ftest-coverage -fprofile-arcs -fno-inline
gcov: all 

.PHONY : clean
.SILENT : clean
clean: test-clean aunit-clean

aunit:
	$(MAKE) -C $(AUNIT_DIR) all 


#Compile all test-cases/-suites:
test : 
	$(CC) $(CFLAGS) test-tests
	$(BB) $(BFLAGS) test-tests.ali

#Compile all asymmetric cipher test-cases/-suites
asymmetric : 
	$(CC) $(CFLAGS) test-asymmetric_ciphers		
	$(BB) $(BFLAGS) test-asymmetric_ciphers.ali

#Compile all big_number test-cases/-suites
big_number : 
	$(CC) $(CFLAGS) test-$@
	$(BB) $(BFLAGS) test-$@.ali


#Compile all hash test-cases/-suites and MAC's
hash : 
	$(CC) $(CFLAGS) test-$@
	$(BB) $(BFLAGS) test-$@.ali

kdf :
	$(CC) $(CFLAGS) test-$@		
	$(BB) $(BFLAGS) test-$@.ali

#Compile all symmetric cipher and blockcipher test-cases/-suites
symmetric : 
	$(CC) $(CFLAGS) test-symmetric_ciphers		
	$(BB) $(BFLAGS) test-symmetric_ciphers.ali

aunit-clean:
	$(MAKE) -C $(AUNIT_DIR) clean 

test-clean:
	$(GNATCLEAN) *.o *.ali
	$(DEL) *.gcno *.gcov *.gcda
	$(DEL) -r coverage
lcov:
	$(GEN_HTML) 
