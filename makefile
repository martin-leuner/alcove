all: doc

doc: makedoc.g createautodoc.g ListOfDocFiles.g PackageInfo.g gap/*.gd
		gap createautodoc.g
	        gap makedoc.g

clean:
	(cd doc ; ./clean)

