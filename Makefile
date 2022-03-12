BIN=pberr
DEPS=pberr.m
FILES=$(patsubst %.m,%,$(DEPS))
GENEXT=d,o,mh,err,c,c_date,mh,mih
GRADE=hlc.gc
# this one links to the extras folder in case you need it.
# but note that you may need to change the /usr/local/MERCURY-ROOT/..
# FLAGS=--ml posix --mld /usr/local/mercury-rotd-2021-04-15/extras/lib/mercury -s $(GRADE) -O4 -E
FLAGS=-s $(GRADE) -O4 -E

all:: $(BIN)

install:: $(BIN)
	mv -f -v $(BIN) $(HOME)/bin/

%: %.m $(DEPS)
	mmc $(FLAGS) --make $@

$(BIN): $(DEPS)
	mmc $(FLAGS) --make $(BIN)

clean::
	rm -rf Mercury
	rm -fv $$(for x in $(FILES); do echo $$x.{$(GENEXT)}; done)
	rm -fv $(BIN)
