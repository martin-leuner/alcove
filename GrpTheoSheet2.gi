Print( "\n\n------------------------------------------------------------\n                        Aufgabe 1                           \n------------------------------------------------------------\n\n" );

D6xD6 := Group( [ (1,2), (2,3), (4,5), (5,6) ] );;
D8xD8 := Group( [ (1,2)(3,4), (1,3), (5,6)(7,8), (5,7) ] );;
C2wS3 := Group( [ (1,2), (3,4), (5,6), (1,3)(2,4), (1,3,5)(2,4,6) ] );;

ND6 := NormalSubgroups( D6xD6 );;
ND8 := NormalSubgroups( D8xD8 );;
NWP := NormalSubgroups( C2wS3 );;

AllTwoFactorDirectDecompositions := function( G )
 local NList, Result, i, j, n;
 NList := NormalSubgroups( G );
 Result := [];
 n := Size( NList );
 for i in [1..n-1] do
  for j in [i+1..n] do
   if( Order( Intersection( NList[i], NList[j] ) ) = 1 and Group( Union2( GeneratorsOfGroup( NList[i] ), GeneratorsOfGroup( NList[j] ) ) ) = G ) then
    Add( Result, [NList[i],NList[j]] );
   fi;
  od;
 od;
 Remove( Result, 1 );
 return Result;
end;;

Print( "# AllTwoFactorDirectDecompositions berechnet das, was sein Name sagt. Ist keine GAP-interne Funktion, sondern selbstgeschrieben (und das ziemlich primitiv).\n" );

Print( "\ngap> AllTwoFactorDirectDecompositions( D6xD6 );\n" );
Display( AllTwoFactorDirectDecompositions( D6xD6 ) );
Print( "# Hier finden wir also D6xD6 als einzige Zerlegung.\n# Nun die Automorphismengruppe:\n" );
Display( StructureDescription( AutomorphismGroup( D6xD6 ) ) );
Print( "# Somit Aut( D6xD6 ) \\cong D6 \\wreath C2.\n" );

Print( "\ngap> AllTwoFactorDirectDecompositions( D8xD8 );\n" );
tmp := AllTwoFactorDirectDecompositions( D8xD8 );;
Display( tmp );
Print( "# Fuer D8xD8 erhalten wir 16 verschiedene Zerlegungen in direkte Produkte von zwei Faktoren.\n" );
Print( "# Teste diese Faktoren auf Unzerlegbarkeit.\n" );
Print( "gap> ForAll( tmp, facts -> IsEmpty( AllTwoFactorDirectDecompositions( facts[1] ) ) and IsEmpty( AllTwoFactorDirectDecompositions( facts[2] ) ) )\n" );
Display( ForAll( tmp, facts -> IsEmpty( AllTwoFactorDirectDecompositions( facts[1] ) ) and IsEmpty( AllTwoFactorDirectDecompositions( facts[2] ) ) ) );
A := AutomorphismGroup( D8xD8 );;
I := InnerAutomorphismsAutomorphismGroup( A );;
F := FactorGroup( A, I );;
Print( "# Zur Automorphismengruppe:\ngap> Size( AutomorphismGroup( D8xD8 ) );\n" );
Display( Size(A) );
Print( "# Bestimme die innere und aeussere Automorphismengruppe:\n# Innere:\n" );
Display( StructureDescription( I ) );
Print( "# Auessere:\n" );
Display( StructureDescription( F ) );

Print( "\ngap> AllTwoFactorDirectDecompositions( C2wS3 );\n" );
tmp := AllTwoFactorDirectDecompositions( C2wS3 );;
Display( tmp );
Print( "# Fuer C2 \\wreath S3 finden wir zwei verschiedene Zerlegungen.\n# Teste diese Faktoren auf Unzerlegbarkeit.\n" );
Print( "gap> ForAll( tmp, facts -> IsEmpty( AllTwoFactorDirectDecompositions( facts[1] ) ) and IsEmpty( AllTwoFactorDirectDecompositions( facts[2] ) ) )\n" );
Display( ForAll( tmp, facts -> IsEmpty( AllTwoFactorDirectDecompositions( facts[1] ) ) and IsEmpty( AllTwoFactorDirectDecompositions( facts[2] ) ) ) );
Print( "# Zur Automorphismengruppe:\n" );
Display( StructureDescription( AutomorphismGroup( C2wS3 ) ) );
Print( "# Somit Aut( C2 \\wreath S3 ) \\cong C2 \\wreath S3.\n" );


Print( "\n\n------------------------------------------------------------\n                        Aufgabe 4                           \n------------------------------------------------------------\n\n" );

Print( "gap> IsomorphismGroups( GL(4,2), AlternatingGroup(8) );\n" );
Display( IsomorphismGroups( GL(4,2), AlternatingGroup(8) ) );
Print( "# Somit sind beide isomorph. WARUM das funktioniert und wie man das anhand von passenden Operationen sieht, ueberlege ich mir morgen.\n" );
