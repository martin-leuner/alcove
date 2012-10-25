ZGEndGenerators := function( grp, elt, act )
 local dom, stab, staborbs, reps, orb, mats, mat, tmporb, i, indicator;

 stab := OrbitStabiliser( grp, elt, act );
 dom := stab.orbit;
 stab := stab.stabilizer;

 staborbs := OrbitsDomain( stab, dom, act );

 reps := [];
 for i in [ 1 .. Size(dom) ] do reps[i] := RepresentativeAction( grp, elt, dom[i], act ); od;

 mats := [];
 for orb in staborbs do
  mat := [];
  for i in [ 1 .. Size(dom) ] do
   tmporb := List( orb, x -> act(x,reps[i]) );
   Add( mat, List( [ 1 .. Size(dom) ], function(j) if dom[j] in tmporb then return 1; else return 0; fi; end ) );
  od;
  Add( mats, mat );
 od;

 return mats;
end;


Submatrix := function( mat, rows, cols )
 local i, j, res;
 res := List( [1..Size(rows)], x -> [] );
 for i in [ 1 .. Size(rows) ] do
  for j in [ 1 .. Size(cols) ] do
   res[i][j] := mat[rows[i]][cols[j]];
  od;
 od;
 return res;
end;


InducesUniformMatroid := function( mat )
 local rk, rdim;
 rk := Rank(mat);
 rdim := DimensionsMat(mat)[1];
 return ForAll( Combinations( [1..DimensionsMat(mat)[2]], rk ), cols -> Rank( Submatrix( mat, [1..rdim], cols ) ) = rk );
end;
