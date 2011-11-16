SRCDIR = src
DOC= ./doc/
SUBTDIR = test


all: acl test
install: install-acl
uninstall: uninstall-acl 
clean: clean-acl  clean-acltest clean-shared

###########################################################################
################################### ACL ###################################
###########################################################################
acl:
	$(MAKE) -C $(SRCDIR) all

install-acl:
	$(MAKE) -C $(SRCDIR) install	

uninstall-acl:
	$(MAKE) -C $(SRCDIR) uninstall	


clean-acl:
	 $(MAKE) -C $(SRCDIR) clean

###########################################################################
################################# SHARED ##################################
###########################################################################

shared:
	$(MAKE) -C $(SRCDIR) shared

install-shared:
	$(MAKE) -C $(SRCDIR) install-shared

unistall-shared:
	$(MAKE) -C $(SRCDIR) uninstall-shared


clean-shared:	
	$(MAKE) -C $(SRCDIR) clean-shared

###########################################################################
################################### TEST ##################################
###########################################################################

acltest:
	$(MAKE) -C $(SUBTDIR) all

clean-acltest:
	$(MAKE) -C $(SUBTDIR) clean


###########################################################################
############################## DOCUMENTATION ##############################
###########################################################################

docu:
	$(MAKE) -C $(DOC) all
 
install-docu:
	$(MAKE) -C $(DOC) install	

unistall-docu:
	$(MAKE) -C $(DOC) uninstall	

clean-docu:
	$(MAKE) -C $(DOC) clean

