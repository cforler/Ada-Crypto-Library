

CC = gnatmake
CFLAGS_O3    = -gnato -c -I../src -I../test -O3 -gnatn -gnatwrclp
CFLAGS       = -gnato -c -I../src -I../test -gnatn -gnatwrclp
CFLAGS_GPROF = -gnato -c -I../src -I../test -gnatn -gnatwrclp -f -pg
CFLAGS_GCOV  = -gnato -c -I../src -I../test -gnatn -gnatwrclp -ftest-coverage -fprofile-arcs
CFLAGS_DBUG  = -gnato -c -I../src -I../test -gnatn -gnatwrclp -g
#CFLAGS_DBUG  =  -c -I../src     -s -g

BB = gnatbind
BFLAGS_O3    = -shared -aO./ -aO/lib/ -aO../src -I- -x
BFLAGS       = -shared -aO./ -aO/lib/ -aO../src -I- -x
BFLAGS_GPROF = -shared -aO./ -aO/lib/ -aO../src -I- -x
BFLAGS_GCOV  = -shared -aO./ -aO/lib/ -aO../src -I- -x
BFLAGS_DBUG  = -shared -aO./ -aO/lib/ -aO../src -I- -x -g

LL = gnatlink
#LFLAGS = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src -lacl
LFLAGS_O3    = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src 
LFLAGS       = -s -z combreloc -dy  -x -X -I../src -aO./ -aO../src
#LFLAGS_GPROF = -s -z combreloc -dy  -x -X -I../src -aO./ -aO../src -pg
LFLAGS_GPROF = -z combreloc -dy  -x -X -I../src -aO./ -aO../src -pg
LFLAGS_GCOV  = -s -z combreloc -dy  -x -X -I../src -aO./ -aO../src -fprofile-generate
LFLAGS_DBUG  =  -I../src -aO./ -aO../src -g

################################################################################
### +++ aclbench +++ ###########################################################
################################################################################
all : aclbench.o

#aclbench.o :
#	$(CC) $(CFLAGS) aclbench
#	$(BB) $(BFLAGS) aclbench.ali
#	$(LL) $(LFLAGS) aclbench.ali

aclbench.o :
	$(CC) $(CFLAGS_O3) aclbench
	$(BB) $(BFLAGS_O3) aclbench.ali
	$(LL) $(LFLAGS_O3) aclbench.ali

#aclbench.o :
#	$(CC) $(CFLAGS_DBUG) aclbench
#	$(BB) $(BFLAGS_DBUG) aclbench.ali
#	$(LL) $(LFLAGS_DBUG) aclbench.ali

#aclbench.o :
#	$(CC) $(CFLAGS_GPROF) aclbench
#	$(BB) $(BFLAGS_GPROF) aclbench.ali
#	$(LL) $(LFLAGS_GPROF) aclbench.ali

#aclbench.o :
#	$(CC) $(CFLAGS_GCOV) aclbench
#	$(BB) $(BFLAGS_GCOV) aclbench.ali
#	$(LL) $(LFLAGS_GCOV) aclbench.ali


################################################################################
### +++ profiling +++ ##########################################################
################################################################################

#profiling: gprof
#DATA=./data/
#DATE=$(shell date)
#FILE = gprof.$(DATE).txt
#gprof:
#	./aclbench
#	gprof    aclbench > '$(DATA)$(FILE)'
#	kprof -f aclbench
#valgrind: valgrind --tool=callgrind ./aclbench 


.PHONY : clean
.SILENT : clean

clean:
	rm -f aclbench aclbench-gcov aclbench.o aclbench.ali *.o *.ali *.gcno *.gcda *.out *.gcov
