LoadPackage( "AutoDoc" );

CreateAutomaticDocumentation( "alcove", "gap/AutoDocEntries.g", "doc/", true,
	[
		[ "Matroids", "Constructions", Concatenation( [ "This section describes the basic constructions to create matroids using alcove.",
						" This entails constructors to obtain new matroids as well as the creation of duals and minors." ] ) ],
		[ "Matroids", "Bases,_circuits_and_all_their_companions", Concatenation( [ "Some introductory text to this section.",
						"" ] ) ]
	] );

QUIT;
