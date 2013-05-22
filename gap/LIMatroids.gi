
###################
## Standard implications for ALL matroids:

InstallGlobalFunction( _alcove_MatroidStandardImplications,
                        [ IsMatroid ],

 function( matroid )
  local entry, entry_list;


  
  entry := ToDoListEntryToMaintainFollowingAttributes(
              [ [ matroid, "DualMatroid" ] ],
              [ matroid, [ DualMatroid, matroid ] ],
              [ [ "the rank of the dual is the co-rank", [ "RankOfMatroid", [ "RankOfMatroid", function() return SizeOfGroundSet( matroid ) - RankOfMatroid( matroid ); end ] ] ],
                [ "duals of uniform matroids are uniform", "IsUniform" ],
                [ "dual matroids have the same automorphism group", "AutomorphismGroup" ] ] );
  
  AddToToDoList( entry );

#######
## Implications for uniform matroids:
  
  entry_list := ToDoListEntry( [ [ matroid, "IsUniform", true ] ],
                               [ [ "we can write down Tutte polynomials of uniform matroids",
                                   [ matroid,
                                     "TuttePolynomial",
                                      [ TuttePolynomial, matroid ] ] ],
                                 [ "the automorphism group of U_{k,n} is S_n",
                                   [ matroid,
                                     "AutomorphismGroup",
                                     [ SymmetricGroup, SizeOfGroundSet( matroid ) ] ] ],
                                 [ "U_{k,n} is connected if and only if 1 < k < n",
                                   [ matroid,
                                     "IsConnected",
                                     function() return
                                       SizeOfGroundSet( matroid ) <= 1
                                       or
                                       (
                                         0 < RankOfMatroid( matroid )
                                         and
                                         RankOfMatroid( matroid ) < SizeOfGroundSet( matroid )
                                       );
                                     end ] ] ] );
  
  Perform( entry_list, AddToToDoList );
##
# Set simplicity:

  entry := ToDoListEntry( [
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
  
end );

###################
## Standard implications for vector matroids:

InstallGlobalFunction( _alcove_VectorMatroidImplications,
                        [ IsMatroid ],

 function( matroid )
  local entry;

##
# If normal form is computed, set rank:

  entry := ToDoListEntry( [ [ matroid, "StandardMatrixOfVectorMatroid" ] ],
                                        matroid,
                                        "RankOfMatroid",
                                        function() return
                                                NrRows( StandardMatrixOfVectorMatroid( matroid )[1] );
                                        end );

  SetDescriptionOfImplication( entry, "the normal form determines the rank" );
  AddToToDoList( entry );

 end

);

