FindFixedAndMovedPoints := function()
 local Pts, i;
 Pts := [,];

 for i in [0..CurrentNumberOfPoints-1] do
  if( not CurrentNumberOfPoints-i in MovedPoints( RandomLongCycle ) ) then
   Pts[1] := CurrentNumberOfPoints-i;
   break;
  fi;
 od;
 if( not IsBound( Pts[1] ) ) then
  return fail;
 fi;

 for i in List( [1..CurrentNumberOfPoints], k -> k mod CurrentNumberOfPoints ) do
  if( CurrentNumberOfPoints-i in MovedPoints( RandomLongCycle ) ) then
   Pts[2] := CurrentNumberOfPoints-i;
   break;
  fi;
 od;
 if( not IsBound( Pts[2] ) ) then
  return fail;
 fi;

 return Pts;
end;;
