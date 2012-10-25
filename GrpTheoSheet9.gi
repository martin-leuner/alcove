Print( "\n\n------------------------------------------------------------\n                        Aufgabe 3a                           \n------------------------------------------------------------\n\n" );

A := FpGroupPresentation( PresentationViaCosetTable( PSL(2,7) ) );;
LA := LowIndexSubgroupsFpGroup( A, 8 );;

Print( "# Zeige: PSL(2,7) hat nicht-konjugierte {2,3}-Hall-Untergruppen.\n\n" );
Print( "gap> A := FpGroupPresentation( PresentationViaCosetTable( PSL(2,7) ) );;\n" );
Print( "gap> LA := LowIndexSubgroupsFpGroup( A, 8 );;\n" );
Print( "gap> List( LA, g -> Index( A, g ) );\n" );
Display( List( LA, g -> Index( A, g ) ) );

Print( "\n# Die Untergruppen LA[3] und LA[4] sind S4en, aber sie sind nicht konjugiert:\n\n" );
Print( "gap> LA[3] in ConjugateSubgroups( A, LA[4] );\n" );
Display( LA[3] in ConjugateSubgroups( A, LA[4] ) );

Print( "\n# Zeige weiter: PSL(2,11) hat nicht-isomorphe {2,3}-Hall-Untergruppen.\n\n" );
Print( "gap> B := PSL(2,11);;\n" );
Print( "gap> LB := List( ConjugacyClassesSubgroups( B ), Representative );;\n" );
B := PSL(2,11);;
LB := List( ConjugacyClassesSubgroups( B ), Representative );;
Print( "gap> List( LB, g -> Index( B, g ) );\n" );
Display( List( LB, g -> Index( B, g ) ) );

Print( "\n# Auf gut Glueck betrachten wir die Untergruppen von Index 55.\n\n" );
Print( "gap> IsomorphismGroups( LB[11], LB[12] );\n" );
Display( IsomorphismGroups( LB[11], LB[12] ) );

Print( "\n# Tatsaechlich handelt es sich um eine D12 und eine A4:\n\n" );
Print( "gap> StructureDescription( LB[11] );\n" );
Display( StructureDescription( LB[11] ) );
Print( "gap> StructureDescription( LB[12] );\n" );
Display( StructureDescription( LB[12] ) );
