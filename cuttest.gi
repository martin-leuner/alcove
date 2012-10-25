G := Group(());
C := 0;
Res := [];

TwoPower := function( n )
 local exp;
 exp := 0;
 while( n mod 2 = 0 ) do
  exp := exp + 1;
  n := n/2;
 od;
 return exp;
end;

TestInit := function( n )
 local i;
 G := SymmetricGroup( n );;
 Print( "# Wrote down generators. Initializing product replacement...\n" );
 PseudoRandom(G);
 Print( "# Initialized product replacement. Scrambling...\n" );
 for i in [ 1 .. 100 ] do PseudoRandom(G); od;
 Print( "# Scrambled product replacement's generating set. Setting global variables...\n" );
 C := 4*Int( RootInt( n )/3 );
 Res := List( [ 1 .. Log2Int( C ) ], i -> 0 );
end;

Test := function( num )
 local i, g, o, p;
 for i in [ 1 .. num ] do
  g := PseudoRandom(G);
  o := Order(g);
  p := TwoPower(o);
  if( p > 0 and NrMovedPoints( g^(o/2) ) <= C ) then
   Res[p] := Res[p] + 1;
  fi;
 od;
end;

ResultOfTest := function( n, t )
 Print( "# Testing n=", n, ", t=", t, ".\n" );
 TestInit( n );
 Test( t );
 return Res;
end;
