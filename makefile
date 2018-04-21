all : clean build

src/%.cpp : src/%.fl
	cp $< $@

build : src/Topology.cpp
	cabal build

clean :
	cabal clean
	rm -f src/Topology.cpp
