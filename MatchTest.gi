CheckProportionOfElementsMatchingTransposition := function( n, NumberOfTests )
 local Hits, G, Rnd, i;
 Hits := 0;
 G := SymmetricGroup(n);
 for i in [1..NumberOfTests] do
  if( Order( Comm((1,2),PseudoRandom(G)) ) = 3 ) then
   Hits := Hits + 1;
  fi;
 od;
 return Hits/NumberOfTests;
end;;
