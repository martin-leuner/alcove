MeanSecondEntry := function( L )
 local MeanList, sl, entry;
 MeanList := List( [1..Size(L[1])], ent -> 0 );
 for sl in L do
  for entry in sl do
   MeanList[ entry[1] ] := MeanList[ entry[1] ] + entry[2];
  od;
 od;
 MeanList := List( MeanList, k -> k/Size(L) );
 return MeanList;
end;;
