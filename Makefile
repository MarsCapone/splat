SRC_DIR = src

.PHONY: project

all:
	$(MAKE) -C $(SRC_DIR) all


clean: 
	$(MAKE) -C $(SRC_DIR) clean 

depend:
	$(MAKE) -C $(SRC_DIR) depend

test:
	$(MAKE) -C $(SRC_DIR) test
