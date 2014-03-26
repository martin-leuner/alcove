all: doc

doc: makedoc.g PackageInfo.g gap/*.gd gap/*.gi
	        gap makedoc.g

clean:
	( cd doc ; ./clean )

