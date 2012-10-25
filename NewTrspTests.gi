Steps := 0;;
S10t := [];;
S100t := [];;
S1000t := [];;
S1024t := [];;
S5000t := [];;
S10d := [];;
S100d := [];;
S1000d := [];;
S1024d := [];;
S5000d := [];;
S10 := SymmetricGroup(10);;
S100 := SymmetricGroup(100);;
S1000 := SymmetricGroup(1000);;
S1024 := SymmetricGroup(1024);;
S5000 := SymmetricGroup(5000);;
for i in [1..100] do
 PseudoRandom(S10);;
od;;
for i in [1..100] do
 PseudoRandom(S100);;
od;;
for i in [1..100] do
 PseudoRandom(S1000);;
od;;
for i in [1..100] do
 PseudoRandom(S1024);;
od;;
for i in [1..100] do
 PseudoRandom(S5000);;
od;;

Gustav := function( G, L, t )
 local tmp, k;
 k := 1;
 tmp := t^PseudoRandom(G);
 while( tmp*t = t*tmp and t <> One(G) ) do
  t := t*tmp;
  tmp := t^PseudoRandom(G);
  k := k+1;
 od;
 if( IsBound(L[k]) ) then
  L[k] := L[k] + 1;
 else
  L[k] := 1;
 fi;
end;;

DisplayLists := function()
 Print( "Distribution after ", Steps, " steps:\n" );
 Print( "--- S10 ---\nTransposition:        ", S10t, "\nDouble transposition: ", S10d, "\n" );
 Print( "--- S100 ---\nTransposition:        ", S100t, "\nDouble transposition: ", S100d, "\n" );
 Print( "--- S1000 ---\nTransposition:        ", S1000t, "\nDouble transposition: ", S1000d, "\n" );
 Print( "--- S1024 ---\nTransposition:        ", S1024t, "\nDouble transposition: ", S1024d, "\n" );
 Print( "--- S5000 ---\nTransposition:        ", S5000t, "\nDouble transposition: ", S5000d, "\n" );
end;;

while( true ) do
 Steps := Steps + 1;
 Gustav( S10, S10t, (1,2) );
 Gustav( S10, S10d, (1,2)(3,4) );
 Gustav( S100, S100t, (1,2) );
 Gustav( S100, S100d, (1,2)(3,4) );
 Gustav( S1000, S1000t, (1,2) );
 Gustav( S1000, S1000d, (1,2)(3,4) );
 Gustav( S1024, S1024t, (1,2) );
 Gustav( S1024, S1024d, (1,2)(3,4) );
 Gustav( S5000, S5000t, (1,2) );
 Gustav( S5000, S5000d, (1,2)(3,4) );
od;
