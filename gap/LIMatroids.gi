

########################################################
##
## Tweet some properties and attributes to dual matroids
## 
########################################################


## IsUniform

InstallImmediateMethod( Twitter,
			IsMatroid and HasDualMatroid and HasIsUniform,
			0,

 function( matroid )

  SetIsUniform( DualMatroid( matroid ), IsUniform( matroid ) );
  TryNextMethod();

 end

);


## Rank

InstallImmediateMethod( Twitter,
			IsMatroid and HasDualMatroid and HasRankOfMatroid,
			0,

 function( matroid )

  SetRankOfMatroid( DualMatroid( matroid ), SizeOfGroundSet( matroid ) - RankOfMatroid( matroid ) );
  TryNextMethod();

 end

);


InstallGlobalFunction( __alcove_MatroidStandardImplications,
			[ IsMatroid ],

 function( matroid )
  local entry;

## Implications for uniform matroids:

  #entry := ToDoListEntryWhichLaunchesAFunction( [ [ matroid, "IsUniform", true ] ], function() TuttePolynomial( matroid ); end );

# Compute Tutte polynomial of uniform matroids:

  entry := ToDoListEntryWithPointers( matroid, "IsUniform", true, matroid, "TuttePolynomial", function() return TuttePolynomial( matroid ); end );
  SetDescriptionOfImplication( entry, SetDescriptionOfImplication );
  AddToToDoList( entry );


 end

);

