CoverTest := function( n, k )
 local Sn, kCycle, SuppList, Steps, Rnd;
 Sn := SymmetricGroup(n);
 kCycle := PermListList( [1..k], List( [1..k], i->(i mod k)+1 ) );
 SuppList := Set([1..k]);
 Steps := 1;
 while( Size(SuppList) < n ) do
  Rnd := kCycle^PseudoRandom(Sn);
  SuppList := Union2( SuppList, MovedPoints(Rnd) );
  Steps := Steps + 1;
 od;
 return Steps;
end;;
