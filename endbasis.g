SplitOverQ := function( list )
 local irreds, F;
 F := Rationals;
 irreds := Filtered( Union( List( list, m -> Factors( MinimalPolynomial( F, m ) ) ) ), p -> Degree(p) > 1 );
 while not IsEmpty( irreds ) do
  F := FieldExtension( F, irreds[1] );
  irreds := Filtered( Union( List( irreds, f -> Factors( PolynomialRing(F,1), f ) ) ), p -> Degree(p) > 1 );
 od;
 return F;
end;


DisplayList := function( list )
 local ent;
 for ent in list do Display( ent ); Print("\n"); od;
end;


AdjacencyMatrices := function( grp )
 local conj, class;

 conj := ConjugacyClasses(grp);

 return List( conj, class -> List( grp, i -> List( grp, function(j) if j^-1*i in class then return 1; else return 0; fi; end ) ) );
end;


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


SimultaneousEigenspaces := function( matlist )
 local F, ESlist, sections, currlist;

 F := SplitOverQ( matlist );
 ESlist := List( matlist, m -> Eigenspaces( F, m ) );

 sections := [ VectorSpace(F,IdentityMat(DimensionsMat(matlist[1])[1])) ];
 for currlist in ESlist do
  sections := Filtered( Concatenation( List( sections, sec -> List( currlist, space -> Intersection( space, sec ) ) ) ), cand -> Dimension(cand) > 0 );
 od;

 return sections;
end;


OrthogonalProjections := function( vslist )
 local n, genmat, offset, vsrowinds, i, inv;

 vsrowinds := [];
 offset := 0;
 for i in [1..Size(vslist)] do
  vsrowinds[i] := List( [1..Dimension(vslist[i])], j -> j + offset );
  offset := offset + Dimension(vslist[i]);
 od;
 n := Length( GeneratorsOfVectorSpace(vslist[1])[1] );
 genmat := Concatenation( List( vslist, GeneratorsOfVectorSpace ) );
 inv := genmat^-1;

 return List( [1..Size(vslist)], nr ->
	inv * List( [1..n], function(j) if j in vsrowinds[nr] then return genmat[j]; else return Zero(vslist[1]); fi; end ) );
end;


EigenMatrices := function( matlist, eslist )
 local first, ev;

 ev := function( mat, vs )
  local gen, im, i;
  gen := GeneratorsOfVectorSpace(vs)[1];
  im := gen*mat;
  for i in [1..Length(gen)] do
   if not IsZero(gen[i]) then return im[i]/gen[i]; fi;
  od;
 end;

 first := List( [ 1 .. Size(matlist) ], i -> List( [ 1 .. Size(matlist) ], j -> ev( matlist[j], eslist[i] ) ) );

 return [ first, DimensionsMat(matlist[1])[1]*first^-1 ];
end;


