InvRingQx := function( gens )		# gens a list of generators
 local 	InvActOnMonomials, MonomialsByDegree, Reynolds, Independent,
	dim, G, vars, invgens, currdeginvgens, currdeg, currmon, potentialinv;
 dim := DimensionsMat( gens[1] )[1];
 G := Group( gens );
 vars := List( [ 1 .. dim ], i -> Indeterminate( Rationals, i ) );
 invgens := [];

 # local function InvActOnMonomials computes image of monomial under g^(-1)
 InvActOnMonomials := function( g, mon )	# g a matrix, mon a list of
 						# integers containing the exponents of the monomial
  return
  Product( List( [ 1 .. dim ],
	i -> Sum( List( [ 1 .. dim ],	# compute image of each variable under g
		k -> g[k][i] * vars[k]
	) ) ^ mon[i]
  ) );
 end;

 # MonomialsByDegree ignores sensible programming and writes down every monomial of given degree
 MonomialsByDegree := function( deg )
  return Filtered( Tuples( [ 0 .. deg ], dim ), s -> Sum(s) = deg );
 end;
 
 # Reynolds applies the Reynolds operator to a monomial
 Reynolds := function( mon )
  return Sum( List( G, g -> InvActOnMonomials( g, mon ) ) )/Order(G);
	# |G| is irrelevant, but it's computed anyway, so why not use it
 end;

 # Independent tests for linear independence between known invariants of current degree
 Independent := function( pol )
  if( IsEmpty( currdeginvgens ) ) then
   return pol <> Zero(pol);
  else
   return
   Dimension( VectorSpace( Rationals, Union( currdeginvgens, [ pol ] ) ) ) >
   Dimension( VectorSpace( Rationals, currdeginvgens ) );
  fi;
 end;

 for currdeg in [ 1 .. Order(G) ] do	# look at each degree separately
					# degrees up to |G| suffice by Noether's bound
  currdeginvgens := [];
  for currmon in MonomialsByDegree( currdeg ) do
   potentialinv := Reynolds( currmon );
   if( Independent( potentialinv ) ) then	# invariant added only if linearly independent to 
						# existing ones
    Add( currdeginvgens, potentialinv );
   fi;
  od;
  invgens := Concatenation( invgens, currdeginvgens );
 od;

 return invgens;
end;


G1 := [ [[0,1,0],[0,0,1],[1,0,0]], [[-1,0,0],[0,1,0],[0,0,-1]] ];;
G2 := [	[[0,0,1,0],[0,0,0,1],[1,0,0,0],[0,1,0,0]],
	[[0,0,-1,-1],[0,0,1,0],[0,1,0,0],[-1,-1,0,0]] ];;
I1 := InvRingQx( G1 );;
I2 := InvRingQx( G2 );;

Print( "# Found ", Size(I1), " generators of Q[x]^G1:\n", I1, "\n" );
Print( "# Found ", Size(I2), " generators of Q[x]^G2:\n", I2, "\n" );

# Output is UGLY. Program finds 42 invariants of G1 and 40 invariants of G2.
