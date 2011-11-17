all: moves.pl wins.pl

%.pl: %.py
	python $< > $@

clean:
	rm moves.pl wins.pl
