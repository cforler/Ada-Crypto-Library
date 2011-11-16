CFLAGS = -fno-strict-aliasing -gnat05  -O3 -gnatn -gnatwrclp

CC = gnatmake	
GNATCLEAN=gnatclean

all: ada_containers ada_containers-aunit_lists aunit aunit-assertions aunit-last_chance_handler aunit-memory aunit-memory-utils aunit-options aunit-reporter aunit-reporter-text aunit-reporter-xml aunit-run aunit-simple_test_cases aunit-test_caller aunit-test_cases aunit-test_filters aunit-test_fixtures aunit-test_results aunit-tests aunit-test_suites aunit-time_measure

ada_containers: 
	$(CC) $(CFLAGS) $@

ada_containers-aunit_lists: 
	$(CC) $(CFLAGS) $@

aunit: 
	$(CC) $(CFLAGS) $@

aunit-assertions: 
	$(CC) $(CFLAGS) $@

aunit-last_chance_handler: 
	$(CC) $(CFLAGS) $@

aunit-memory: 
	$(CC) $(CFLAGS) $@

aunit-memory-utils: 
	$(CC) $(CFLAGS) $@

aunit-options: 
	$(CC) $(CFLAGS) $@

aunit-reporter: 
	$(CC) $(CFLAGS) $@

aunit-reporter-text: 
	$(CC) $(CFLAGS) $@

aunit-reporter-xml: 
	$(CC) $(CFLAGS) $@

aunit-run: 
	$(CC) $(CFLAGS) $@

aunit-simple_test_cases: 
	$(CC) $(CFLAGS) $@

aunit-test_caller: 
	$(CC) $(CFLAGS) $@

aunit-test_cases: 
	$(CC) $(CFLAGS) $@

aunit-test_filters: 
	$(CC) $(CFLAGS) $@

aunit-test_fixtures: 
	$(CC) $(CFLAGS) $@

aunit-test_results: 
	$(CC) $(CFLAGS) $@

aunit-tests: 
	$(CC) $(CFLAGS) $@

aunit-test_suites: 
	$(CC) $(CFLAGS) $@
aunit-time_measure: 
	$(CC) $(CFLAGS) $@

.PHONY : clean
.SILENT : clean

clean:
	$(GNATCLEAN) *.o *.ali
