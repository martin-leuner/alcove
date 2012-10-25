IsTransposition := function( t )
 return NrMovedPoints( t ) = 2;
end;;

RandomTranspositionCandidate := function( G )
 local v;
 v := PseudoRandom( G );
 if( not IsEvenInt( Order(v) ) ) then
  return ();
 fi;
 v := v ^ ( Order(v)/2 );
 v := v * ( v ^ PseudoRandom( G ) );
 if( not IsEvenInt( Order(v) ) ) then
  return ();
 fi;
 v := v ^ ( Order(v)/2 );
 return v;
end;;

TestInvolStats := function( n, Tests )
 local S, i, Hits;
 S := SymmetricGroup( n );
 for i in [ 1 .. 100 ] do
  Hits := PseudoRandom( S );
 od;
 Hits := 0;
 for i in [ 1 .. Tests ] do
  if( IsTransposition( RandomTranspositionCandidate( S ) ) ) then
   Hits := Hits + 1;
  fi;
 od;
 return Hits/Tests;
end;;

TestTwoCentGens := function( G )
 local inv, gen1, gens;
 gens := [];

 inv := PseudoRandom(G);
 while not IsEvenInt( Order( inv ) ) do
  inv := PseudoRandom(G);
 od;
 inv := inv ^ ( Order(inv)/2 );

 if IsEvenInt( NrMovedPoints(inv)/2 ) then
  Print( "Involution in A_n.\n" );
  return;
 fi;

 Print( "Involution: ", inv, "\n" );

 Add( gens, inv );

 while( Group( gens ) <> Centralizer( G, inv ) ) do

  gen1 := Comm( inv, PseudoRandom(G) );
  while not IsEvenInt( Order( gen1 ) ) do
   gen1 := Comm( inv, PseudoRandom(G) );
  od;
 
  Add( gens, gen1 ^ ( Order(gen1)/2 ) );

 od;

# Print( "First generator: ", gen1, "\n" );
#
# gen1 := Comm( inv, PseudoRandom(G) );
# while not IsEvenInt( Order( gen1 ) ) do
#  gen1 := Comm( inv, PseudoRandom(G) );
# od;
#
# gen1 := gen1 ^ ( Order(gen1)/2 );
# Print( "Second generator: ", gen2, "\n" );

 return( Size( gens ) );
end;


Mean := function(L)
 return Sum(L)/Size(L);
end;;


InvCommOrder := function( G, inv, Tests )
 local L;
 L := List( [ 1 .. Tests ], i -> Order( Comm( inv, PseudoRandom( G ) ) ) );
 return Mean(L);
end;;
 

IsPretransposition := function( cand, n )
 return ForAll( CycleLengths( cand, [1..n] ), l -> not IsEvenInt(l) or l = 2 );
end;;


IsRecognizableBelowLog := function( cand, n )
 return Maximum( CycleLengths( cand, [1..n] ) ) <= Log2Int(n) and IsPretransposition(cand,n);
end;;


IsRecognizableBelowSqrt := function( cand, n )
 return Maximum( CycleLengths( cand, [1..n] ) ) <= RootInt(n) and IsPretransposition(cand,n);
end;;


TestProportionSn := function( n, NumberOfTests, TestFunction )
 local S, i, Hits;
 S := SymmetricGroup( n );
 for i in [ 1 .. 100 ] do
  Hits := PseudoRandom( S );
 od;
 Hits := 0;
 for i in [ 1 .. NumberOfTests ] do
  if( TestFunction( PseudoRandom( S ), n ) ) then
   Hits := Hits + 1;
  fi;
 od;
 return Hits/NumberOfTests;
end;;
