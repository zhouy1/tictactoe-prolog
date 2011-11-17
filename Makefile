all: moves.pl strategy.pl

%.pl: %.py
	python $< > $@

clean:
	rm moves.pl strategy.pl
