all:
	corebuild -pkg dolog simplex.byte
	corebuild -pkg dolog -pkg re2 chem_types.byte
