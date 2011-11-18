OBJ=moves.pl wins.pl heuristics.pl

all: $(OBJ)

%.pl: %.py
	python $< > $@

clean:
	rm $(OBJ)
