OBJ=moves.pl heuristics.pl

all: $(OBJ)

%.pl: %.py
	python $< > $@

clean:
	rm $(OBJ)
