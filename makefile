all : clean build test

test :
	./dist/build/sculpt/sculpt -o cfg/test.--

src/%.cpp : src/%.fl
	cp $< $@

build : src/Topology.cpp
	cabal build

clean :
	cabal clean
	rm -f src/Topology.cpp
