#
#
#########################################################
###
### Tweet some properties and attributes to dual matroids
### 
#########################################################
#
#
### IsUniform
#
#InstallImmediateMethod( Twitter,
#			IsMatroid and HasDualMatroid and HasIsUniform,
#			0,
#
# function( matroid )
#
#  SetIsUniform( DualMatroid( matroid ), IsUniform( matroid ) );
#  TryNextMethod();
#
# end
#
#);
#
#
### Rank
#
#InstallImmediateMethod( Twitter,
#			IsMatroid and HasDualMatroid and HasRankOfMatroid,
#			0,
#
# function( matroid )
#
#  SetRankOfMatroid( DualMatroid( matroid ), SizeOfGroundSet( matroid ) - RankOfMatroid( matroid ) );
#  TryNextMethod();
#
# end
#
#);
#

## Launches a function:
#
# entry := ToDoListEntryWhichLaunchesAFunction( [ [ matroid, "IsUniform", true ] ], function() TuttePolynomial( matroid ); end );
#


###################
## Standard implications for ALL matroids:

InstallGlobalFunction( __alcove_MatroidStandardImplications,
			[ IsMatroid ],

 function( matroid )
  local entry;


#######
## Implications for dual matroids:

# Set the rank:

  entry := ToDoListEntryWithListOfSources( [
						[ matroid, "DualMatroid" ],
						[ matroid, "RankOfMatroid" ]
					],
					[ DualMatroid, matroid ],
					"RankOfMatroid",
					function() return
						SizeOfGroundSet( matroid ) - RankOfMatroid( matroid );
					end );

  SetDescriptionOfImplication( entry, "the rank of the dual is the co-rank" );
  AddToToDoList( entry );

# Transfer uniformity:

  entry := ToDoListEntryWithListOfSources( [
						[ matroid, "DualMatroid" ],
						[ matroid, "IsUniform" ]
					],
					[ DualMatroid, matroid ],
					"IsUniform",
					[ IsUniform, matroid ] );

  SetDescriptionOfImplication( entry, "duals of uniform matroids are uniform" );
  AddToToDoList( entry );

# Transfer automorphism group:

  entry := ToDoListEntryWithListOfSources( [
						[ matroid, "DualMatroid" ],
						[ matroid, "AutomorphismGroup" ]
					],
					[ DualMatroid, matroid ],
					"AutomorphismGroup",
					[ AutomorphismGroup, matroid ] );

  SetDescriptionOfImplication( entry, "dual matroids have the same automorphism group" );
  AddToToDoList( entry );


#######
## Implications for uniform matroids:

##
# Compute Tutte polynomial:

  entry := ToDoListEntryWithListOfSources( [
						[ matroid, "IsUniform", true ]
					],
					 matroid,
					"TuttePolynomial",
					[ TuttePolynomial, matroid ] );

  SetDescriptionOfImplication( entry, "we can write down Tutte polynomials of uniform matroids" );
  AddToToDoList( entry );

##
# Set simplicity:

  entry := ToDoListEntryWithListOfSources( [
						[ matroid, "IsUniform", true ],
						[ matroid, "RankOfMatroid" ]
					],
					matroid,
					"IsSimpleMatroid",
					function() return
						RankOfMatroid( matroid ) > 1
						or
						SizeOfGroundSet( matroid ) < 2;
					end );

  SetDescriptionOfImplication( entry, "uniform matroids are simple iff their rank is greater than one or they have a one-element ground set" );
  AddToToDoList( entry );

##
# Set automorphism group:

  entry := ToDoListEntryWithPointers( matroid, "IsUniform", true,
					matroid,
					"AutomorphismGroup",
					SymmetricGroup( SizeOfGroundSet( matroid ) ) );

  SetDescriptionOfImplication( entry, "the automorphism group of U_{k,n} is S_n" );
  AddToToDoList( entry );

 end

);

