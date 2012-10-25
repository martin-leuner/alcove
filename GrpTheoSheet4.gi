Print( "\n\n------------------------------------------------------------\n                        Aufgabe 1                           \n------------------------------------------------------------\n\n" );

D8 := Group( [ [[-1,0],[0,1]], [[0,1],[1,0]] ] );
Q8 := Group( [ [[0,E(4)],[E(4),0]], [[E(4),0],[0,-E(4)]] ] );
C4 := Group( [ [E(4)] ] );

Print( "# Waehle Darstellung von D8, Q8 in GL(2,C) und von C4 in GL(1,C) (vgl. Quellcode). Dann\n",
       "# erhaelt man die zentralen Produkte der Gruppen ueber C2 aus den Kroneckerprodukten mit\n",
       "# der Identitaetsmatrix (passend), da die Bilder der Quadrate der jeweiligen Elemente von\n",
       "# Ordnung 4 durch kroneckern jeweils auf -I_4 gehen.\n\n" );

D8oD8 := CentralProductOfMatrixGroups( D8, D8 );
D8oQ8 := CentralProductOfMatrixGroups( D8, Q8 );
D8oQ8oC4 :=  CentralProductOfMatrixGroups( CentralProductOfMatrixGroups( D8, Q8 ), C4 );

Print( "gap> D8oD8 := CentralProductOfMatrixGroups( D8, D8 );\n" );
Print( "gap> D8oQ8 := CentralProductOfMatrixGroups( D8, Q8 );\n" );
Print( "gap> D8oQ8oC4 :=  CentralProductOfMatrixGroups( CentralProductOfMatrixGroups( D8, Q8 ), C4 );\n" );
Print( "\n# CentralProductOfMatrixGroups benutzt genau die Konstruktion ueber Kroneckerprodukte. Ist\n",
       "# undokumentiert, aber der Quellcode liegt in gap4r4/grp/classic.gi\n\n" );

Print( "gap> IsSubgroup( D8oQ8oC4, D8oD8 );\n" );
Display( IsSubgroup( D8oQ8oC4, D8oD8 ) );
Print( "gap> IsSubgroup( D8oQ8oC4, D8oQ8 );\n" );
Display( IsSubgroup( D8oQ8oC4, D8oQ8 ) );

PermRep1 := Image( SmallerDegreePermutationRepresentation( Image( RegularActionHomomorphism( D8oD8 ) ) ) );
PermRep2 := Image( SmallerDegreePermutationRepresentation( Image( RegularActionHomomorphism( D8oQ8 ) ) ) );
PermRep3 := Image( SmallerDegreePermutationRepresentation( Image( RegularActionHomomorphism( D8oQ8oC4 ) ) ) );

Print( "\n# Nun zu den Automorphismengruppen.\n" );
Print( "gap> StructureDescription( AutomorphismGroup( D8oD8 ) );\n" );
Display( StructureDescription( PermRep1 ) );
Print( "gap> StructureDescription( AutomorphismGroup( D8oQ8 ) );\n" );
Display( StructureDescription( PermRep2 ) );
Print( "gap> StructureDescription( AutomorphismGroup( D8oQ8oC4 ) );\n" );
Display( StructureDescription( PermRep3 ) );
