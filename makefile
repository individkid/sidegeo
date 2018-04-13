src/%.cpp : src/%.fl
	cp $< $@

build : src/Topology.cpp
	cabal build

clean :
	cabal clean
	rm -f src/Topology.cpp

# This is covered by GNU GENERAL PUBLIC LICENSE https://github.com/individkid/sidegeo/blob/master/LICENSE

