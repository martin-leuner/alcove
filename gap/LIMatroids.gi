
###################
## Standard implications for ALL matroids:

InstallGlobalFunction( _alcove_MatroidStandardImplications,
                        [ IsMatroid ],

 function( matroid )
  local entry, entry_list;



  entry := ToDoListEntryToMaintainFollowingAttributes(
              [ [ matroid, "DualMatroid" ] ],
              [ matroid, [ DualMatroid, matroid ] ],
              [ [ "the rank of the dual is the co-rank", [ "RankOfMatroid", [ "RankOfMatroid", function() return Size( matroid ) - RankOfMatroid( matroid ); end ] ] ],
                [ "duals of uniform matroids are uniform", "IsUniform" ],
                [ "duals of connected matroids are connected", "IsConnected" ],
                [ "dual matroids have the same automorphism group", "AutomorphismGroup" ],
                [ "Tutte polynomial of the dual swaps variables", [ "TuttePolynomial", [ "TuttePolynomial", function()
                                                                                                              local x, y, xy;
                                                                                                              xy := IndeterminatesOfTuttePolynomial();
                                                                                                              x := xy[1];
                                                                                                              y := xy[2];
                                                                                                              return Value( TuttePolynomial(matroid), [x,y], [y,x] );
                                                                                                            end ] ] ],
                [ "cocircuits of the dual matroid are the circuits", [ "Circuits", "Cocircuits" ] ],
                [ "bases of the dual matroid are complements of the bases", [ "Bases", [ "Bases", function()
                                                                                                    local gset;
                                                                                                    gset := GroundSet(matroid);
                                                                                                    return Set( List( Bases(matroid), b -> Difference(gset,b) ) );
                                                                                                  end ] ] ],
              ] );

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
                                     [ SymmetricGroup, Size( matroid ) ] ] ],
                                 [ "U_{k,n} is connected if and only if 1 < k < n",
                                   [ matroid,
                                     "IsConnected",
                                     function() return
                                       Size( matroid ) <= 1
                                       or
                                       (
                                         0 < RankOfMatroid( matroid )
                                         and
                                         RankOfMatroid( matroid ) < Size( matroid )
                                       );
                                     end ] ] ] );

  Perform( entry_list, AddToToDoList );

#######
## Set simplicity:

  entry := ToDoListEntry( [
                                                [ matroid, "IsUniform", true ],
                                                [ matroid, "RankOfMatroid" ]
                                        ],
                                        matroid,
                                        "IsSimpleMatroid",
                                        function() return
                                                RankOfMatroid( matroid ) > 1
                                                or
                                                Size( matroid ) < 2;
                                        end );

  SetDescriptionOfImplication( entry, "uniform matroids are simple iff their rank is greater than one or they have a one-element ground set" );
  AddToToDoList( entry );

#######
## Set RankGeneratingPolynomial:

  entry := ToDoListEntry( [ [ matroid, "TuttePolynomial" ] ],
                          matroid,
                          "RankGeneratingPolynomial",
                          function()
                            local xy, x, y;
                            xy := IndeterminatesOfTuttePolynomial( );
                            x := xy[1];
                            y := xy[2];
                            return Value( TuttePolynomial( matroid ), [ x, y ], [ x+1, y+1 ] );
                          end );

  AddToToDoList( entry );

#######
## Cocircuits and hyperplanes:

  entry := ToDoListEntry( [ [ matroid, "Cocircuits" ] ],
                          matroid,
                          "MatroidHyperplanes",
                          function()
                            local gset;
                            gset := GroundSet( matroid );
                            return Set( List( Cocircuits( matroid ), c -> Difference( gset, c ) ) );
                          end );

  AddToToDoList( entry );

  entry := ToDoListEntry( [ [ matroid, "MatroidHyperplanes" ] ],
                          matroid,
                          "Cocircuits",
                          function()
                            local gset;
                            gset := GroundSet( matroid );
                            return Set( List( MatroidHyperplanes( matroid ), c -> Difference( gset, c ) ) );
                          end );

  AddToToDoList( entry );

#######

 end

);


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

