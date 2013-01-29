all: doc

doc: makedoc.g createautodoc.g ListOfDocFiles.g PackageInfo.g gap/*.gd gap/*.gi doc/*.xml
		gap createautodoc.g
	        gap makedoc.g

clean:
	( cd doc ; ./clean )

