#############################################################################
##
##  Matroid.gi                  alcove package                  Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  Matroid methods for alcove.
##
#############################################################################

####################################
##
## Representations
##
####################################

DeclareRepresentation( "IsAbstractMatroidRep",
	IsMatroid and IsAttributeStoringRep,
	[]
);

DeclareRepresentation( "IsVectorMatroidRep",
	IsMatroid and IsAttributeStoringRep,
	[ "generatingMatrix" ]
);

DeclareRepresentation( "IsRepresentationMatroidRep",
	IsMatroid and IsAttributeStoringRep,
	[ "representation", "baseColumn" ]
);

#DeclareRepresentation( "IsGraphicMatroidRep",
#	IsMatroid and IsAttributeStoringRep,
#	[ "incidenceMatrix" ]
#);


####################################
##
## Types and Families
##
####################################

BindGlobal( "TheFamilyOfMatroids",
	NewFamily( "TheFamilyOfMatroids" , IsMatroid )
);

BindGlobal( "TheTypeAbstractMatroid",
	NewType( TheFamilyOfMatroids,
		IsAbstractMatroidRep )
);

BindGlobal( "TheTypeMinorOfAbstractMatroid",
	NewType( TheFamilyOfMatroids,
		IsAbstractMatroidRep and IsMinorOfMatroid )
);

BindGlobal( "TheTypeVectorMatroid",
	NewType( TheFamilyOfMatroids,
		IsVectorMatroidRep )
);

BindGlobal( "TheTypeMinorOfVectorMatroid",
	NewType( TheFamilyOfMatroids,
		IsVectorMatroidRep and IsMinorOfMatroid )
);

BindGlobal( "TheTypeRepresentationMatroid",
	NewType( TheFamilyOfMatroids,
		IsRepresentationMatroidRep )
);

BindGlobal( "TheTypeMinorOfRepresentationMatroid",
	NewType( TheFamilyOfMatroids,
		IsRepresentationMatroidRep and IsMinorOfMatroid )
);

#BindGlobal( "TheTypeGraphicMatroid",
#	NewType( TheFamilyOfMatroids,
#		IsGraphicMatroidRep )
#);

#BindGlobal( "TheTypeMinorOfGraphicMatroid",
#	NewType( TheFamilyOfMatroids,
#		IsGraphicMatroidRep and IsMinorOfMatroid )
#);


####################################
##
## Attributes
##
####################################


##############
## DualMatroid

##
InstallMethod( DualMatroid,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )
  local dual;

  dual := UniformMatroid( SizeOfGroundSet(matroid) - RankOfMatroid(matroid), SizeOfGroundSet(matroid) );

  SetDualMatroid( dual, matroid );

  return dual;
 end

);

##
InstallMethod( DualMatroid,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],
		30,

 function( matroid )
  local dualbases, dual;

  dualbases := Set( List( Bases( matroid ), b -> Difference( GroundSet( matroid ), b ) ) );

  dual := MatroidByBasesNCL( GroundSet( matroid ), dualbases );
  SetDualMatroid( dual, matroid );
  _alcove_MatroidStandardImplications( dual );

  return dual;
 end

);

##
InstallMethod( DualMatroid,
		"for matroids with a rank function",
		[ IsAbstractMatroidRep and HasRankFunction ],
		20,

 function( matroid )
  local dualRkFunc, dual, corank, rkFunc, gset;

  gset := GroundSet( matroid );
  rkFunc := RankFunction( matroid );
  corank := Size( gset ) - RankOfMatroid( matroid );

  dualRkFunc :=
	function( x )
	 local compl;

	 compl := Difference( gset, x );

	 return corank + rkFunc( compl ) - Size( compl );
	end;

  dual := MatroidByRankFunctionNCL( gset, dualRkFunc );
  SetDualMatroid( dual, matroid );
  _alcove_MatroidStandardImplications( dual );

  return dual;
 end

);

##
InstallMethod( DualMatroid,
		"for connected vector matroids",
		[ IsVectorMatroidRep and IsConnected ],
		30,

 function( matroid )
  local dualmatrix, dual, mat;
  mat := MatrixOfVectorMatroid( matroid );

  dualmatrix := SyzygiesOfRows( Involution( mat ) );

  dual := Matroid( dualmatrix );
  SetDualMatroid( dual, matroid );

  return dual;
 end

);

##
InstallMethod( DualMatroid,
		"for disconnected matroids",
		[ IsMatroid and HasDirectSumDecomposition ],
		0,

 function( matroid )
  local dual;

  if IsConnected( matroid ) then
   TryNextMethod();
  fi;

  dual := rec();
  ObjectifyWithAttributes( dual, TheTypeAbstractMatroid,
			SizeOfGroundSet, SizeOfGroundSet( matroid ),
			DirectSumDecomposition, List( DirectSumDecomposition(matroid), s -> [ s[1], DualMatroid(s[2]) ] ),
			DualMatroid, matroid );
  _alcove_MatroidStandardImplications( dual );

  return dual;
 end

);

##
InstallMethod( DualMatroid,
		"fallback method for connected matroids",
		[ IsMatroid and IsConnected ],
		0,

 function( matroid )

  RankFunction( matroid );

  return DualMatroid( matroid );

 end

);

##
InstallMethod( DualMatroid,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )

  DirectSumDecomposition( matroid );

  return DualMatroid( matroid );

 end

);


####################
## SimplifiedMatroid

##
InstallMethod( SimplifiedMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local del, checkset, currParClass;

  checkset := Difference( GroundSet( matroid ), Loops( matroid ) );
  del := Loops( matroid );

  while not IsEmpty( checkset ) do
   currParClass := ClosureFunction(matroid)( checkset[1] );
   checkset := Difference( checkset, currParClass );
   Remove( currParClass );
   del := Union2( del, currParClass );
  od;

  return Deletion( matroid, del );
 end

);


############################
## NormalFormOfVectorMatroid

##
InstallMethod( NormalFormOfVectorMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  local nf, posOfNonUnitCols;

  nf := RowReducedEchelonForm( MatrixOfVectorMatroid( matroid ) );
  posOfNonUnitCols := Difference( GroundSet( matroid ), PositionOfFirstNonZeroEntryPerRow( nf ) );

  return [ CertainColumns( nf, posOfNonUnitCols ), posOfNonUnitCols ];
 end

);


############
## GroundSet

##
InstallMethod( GroundSet,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return [ 1 .. SizeOfGroundSet( matroid ) ];
 end

);


##################
## SizeOfGroundSet

##
InstallMethod( SizeOfGroundSet,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  return Size( GroundSet( matroid ) );
 end

);

##
InstallMethod( SizeOfGroundSet,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
   return NrColumns( MatrixOfVectorMatroid( matroid ) );
 end

);


################
## RankOfMatroid

##
InstallMethod( RankOfMatroid,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],
		40,

 function( matroid )

  return Size( Bases(matroid)[1] );

 end

);

##
InstallMethod( RankOfMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		20,

 function( matroid )

  return RowRankOfMatrix( MatrixOfVectorMatroid(matroid) );

 end

);

##
InstallMethod( RankOfMatroid,
		"for disconnected matroids",
		[ IsMatroid ],
		0,

 function( matroid )

  return Sum( DirectSumDecomposition(matroid), s -> RankOfMatroid(s[2]) );

 end

);

##
InstallMethod( RankOfMatroid,
		"fallback method for connected matroids",
		[ IsMatroid and IsConnected ],
		10,

 function( matroid )

  return Size( SomeBasis( matroid ) );

 end

);

##
InstallMethod( Rank,
		"alias for Rank for matroids",
		[ IsMatroid ],

 RankOfMatroid

);


###############
## RankFunction

##
InstallMethod( RankFunction,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],
		10,

 function( matroid )
  return
	function( x )
	 local b, max, s;

	 max := 0;

	 for b in Bases( matroid ) do

	  s := Size( Intersection2( b, x ) );
	  if s > max then

	   max := s;
	   if max = Size( x ) then return max; fi;

	  fi;

	 od;

	 return max;
	end;
 end

);

##
InstallMethod( RankFunction,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		30,

 function( matroid )
  return function( x ) return RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid( matroid ), x ) ); end;
 end

);

##
InstallMethod( RankFunction,
		"for disconnected matroids",
		[ IsMatroid and HasDirectSumDecomposition ],
		30,

 function( matroid )

  if IsConnected( matroid ) then TryNextMethod(); fi;

  return function( x ) return Sum( DirectSumDecomposition( matroid ), s -> RankFunction(s[2])( Intersection2( s[1], x ) ) ); end;

 end

);

##
InstallMethod( RankFunction,
		"for 2-sums of matroids",
		[ IsMatroid and HasTwoSumDecomposition ],
		30,

 function( matroid )

  if Is3Connected( matroid ) then TryNextMethod(); fi;

  return function( x )
	 local two, res1, res2, rk1, rk2, r1x, r2x;

	 two := TwoSumDecomposition( matroid );
	 res1 := Intersection2( two[3], x );
	 res2 := Intersection2( two[6], x );
	 rk1 := RankFunction( two[1] );
	 rk2 := RankFunction( two[4] );

	 r1x := rk1( res1 );
	 r2x := rk2( res2 );

	 if r1x = rk1( Union2( res1, [two[2]] ) ) and
	    r2x = rk2( Union2( res2, [two[5]] ) ) then

	  return r1x + r2x - 1;

	 else

	  return r1x + r2x;

	 fi;
	end;

 end

);

##
InstallMethod( RankFunction,
		"fallback method for matroids with known rank",
		[ IsMatroid and HasRankOfMatroid ],

 function( matroid )
  local isIndep, bound;

  isIndep := IndependenceOracle( matroid );

  bound := RankOfMatroid( matroid );

  return
	function( x )
	 local maxIndep, i, tmp, ctr;

	 maxIndep := [];
	 ctr := 0;
	 for i in x do

	  tmp := Union2( maxIndep, [i] );

	  if isIndep( tmp ) then
	   maxIndep := tmp;
	   ctr := ctr + 1;
	  fi;

	  if ctr = bound then break; fi;

	 od;

	 return ctr;
	end;
 end

);

##
InstallMethod( RankFunction,
		"fallback method",
		[ IsMatroid ],

 function( matroid )
  local isIndep, bound;

  isIndep := IndependenceOracle( matroid );

  return
	function( x )
	 local maxIndep, i, tmp;

	 maxIndep := [];
	 for i in x do

	  tmp := Union2( maxIndep, [i] );

	  if isIndep( tmp ) then
	   maxIndep := tmp;
	  fi;

	 od;

	 return Size( maxIndep );
	end;
 end

);


##################
## ClosureFunction

##
InstallMethod( ClosureFunction,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return
	function( x )
	 local loopsOfMinor, minor;

	 minor := MinorNL( matroid, [], x );

	 loopsOfMinor := List( Loops( minor ), l -> ParentAttr(minor)[2][l] );

	 return Union2( x, loopsOfMinor );
	end;
 end

);


#######################
## IndependenceOracle

##
InstallMethod( IndependenceOracle,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		40,

 function( matroid )
  return
	function( x )
	 local nf, unitVecLabels, otherLabels, checkMat, unitVecsInX, nrCols;

	 nf := NormalFormOfVectorMatroid( matroid );
	 otherLabels := nf[2];
	 unitVecLabels := Difference( GroundSet( matroid ), otherLabels );

	 checkMat := CertainColumns( nf[1], List( Intersection2( x, otherLabels ), col -> Position( otherLabels, col ) ) );

         nrCols := NrColumns( checkMat );
         if nrCols = 0 then return true; fi;

         unitVecsInX := Intersection2( x, unitVecLabels );
         checkMat := CertainRows( checkMat, Difference( [ 1 .. NrRows( checkMat ) ], List( unitVecsInX, row -> Position( unitVecLabels, row ) ) ) );

         if NrRows( checkMat ) < nrCols then return false; fi;

         return ColumnRankOfMatrix( checkMat ) = nrCols;
	end;
 end

);

##
InstallMethod( IndependenceOracle,
		"for matroids with bases",
		[ IsMatroid and HasBases ],
		0,

 function( matroid )
  return
	function( x )
	 return ForAny( Bases( matroid ), b -> IsSubset( b, x ) );
	end;
 end

);

##
InstallMethod( IndependenceOracle,
		"for matroids with circuits",
		[ IsMatroid and HasCircuits ],
		10,

 function( matroid )
  return
	function( x )
	 return ForAll( Circuits( matroid ), c -> not IsSubset( x, c ) );
	end;
 end

);

##
InstallMethod( IndependenceOracle,
		"for matroids with a rank function",
		[ IsMatroid and HasRankFunction ],
		30,

 function( matroid )
  return
	function( x )
	 return RankFunction(matroid)(x) = Size(x);
	end;
 end

);

##
InstallMethod( IndependenceOracle,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )
  return
	function( x )
	 return Size(x) <= RankOfMatroid( matroid );
	end;
 end

);

##
InstallMethod( IndependenceOracle,
		"for disconnected matroids",
		[ IsMatroid and HasDirectSumDecomposition ],
		20,

 function( matroid )

  if IsConnected( matroid ) then TryNextMethod(); fi;

  return
	function( x )
	 return ForAll( DirectSumDecomposition( matroid ), s -> IndependenceOracle(s[2])( List( Intersection2(s[1],x), i -> Position(s[1],i) ) ) );
	end;

 end

);

##
InstallMethod( IndependenceOracle,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )
  RankFunction( matroid );
  return IndependenceOracle( matroid );
 end

);


################
## CircuitOracle

##
InstallMethod( CircuitOracle,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )
  return function(x)
		return Size(x) = RankOfMatroid(matroid)+1;
	end;
 end

);

##
InstallMethod( CircuitOracle,
		"for matroids with known circuits",
		[ IsMatroid and HasCircuits ],
		40,

 function( matroid )
  return function(x)
		return x in Circuits( matroid );
	end;
 end

);

##
InstallMethod( CircuitOracle,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )
  return function(x)
		local isIndep;

		isIndep := IndependenceOracle( matroid );

		return not isIndep(x) and ForAll( x, i -> isIndep( Difference( x, [i] ) ) );
	end;
 end

);


########
## Bases

##
InstallMethod( Bases,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )

  return Combinations( GroundSet( matroid ), RankOfMatroid( matroid ) );

 end

);

##
InstallMethod( Bases,				# THIS IS AN EXTREMELY NAIVE APPROACH
		"for vector matroids",
		[ IsVectorMatroidRep and IsConnected ],

 function( matroid )

  return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], RankOfMatroid( matroid ) ),
		b -> RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid(matroid), b ) ) = RankOfMatroid( matroid ) );

 end

);

##
InstallMethod( Bases,
		"for disconnected matroids",
		[ IsMatroid ],
		0,

 function( matroid )

  if IsConnected( matroid ) then TryNextMethod(); fi;

  return List( Cartesian( List( DirectSumDecomposition(matroid), s -> List( Bases(s[2]), b -> List( b, i -> s[1][i] ) ) ) ), Union );

 end

);

##
InstallMethod( Bases,
		"for 2-sums of matroids",
		[ IsMatroid and IsConnected ],
		0,

 function( matroid )
  local twosum;

  if Is3Connected( matroid ) then TryNextMethod(); fi;

  twosum := TwoSumDecomposition( matroid );

  return Union2(
		List(	Cartesian(
				List(
					Filtered( Bases( twosum[1] ), b -> not twosum[2] in b ),
					b -> List( b, i -> twosum[3][i] )
				),
				List(
					Filtered( Bases( twosum[4] ), b -> twosum[5] in b ),
					b -> List( Difference(b,[twosum[5]]), i -> twosum[6][i] )
				)
			), Union ),

		List(	Cartesian(
				List(
					Filtered( Bases( twosum[1] ), b -> twosum[2] in b ),
					b -> List( Difference(b,[twosum[2]]), i -> twosum[3][i] )
				),
				List(
					Filtered( Bases( twosum[4] ), b -> not twosum[5] in b ),
					b -> List( b, i -> twosum[6][i] )
				)
			), Union )
	);

 end

);


#############
## KnownBases

##
InstallMethod( KnownBases,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return [];
 end

);


###########
## Circuits

##
InstallMethod( Circuits,		## recursive exponential time method
		"for connected matroids",
		[ IsMatroid and IsConnected ],
		10,

 function( matroid )
  local loopsColoops, loopColoopFree, delCircs, conCircs, t, h, l, circs;

# Check all trivial cases:

  if SizeOfGroundSet( matroid ) = 0 then return []; fi;
  if SizeOfGroundSet( matroid ) = 1 then return List( Loops( matroid ), i -> [i] ); fi;

  if RankOfMatroid( matroid ) = 0 then return List( GroundSet( matroid ), i -> [i] ); fi;

  loopsColoops := Union2( Loops( matroid ), Coloops( matroid ) );

  if Size( loopsColoops ) = SizeOfGroundSet( matroid ) then return List( Loops( matroid ), i -> [i] ); fi;

# Delete loops and coloops and start recursion:

  loopColoopFree := MinorNL( matroid, loopsColoops, [] );
  t := SizeOfGroundSet( loopColoopFree );

  delCircs := Circuits( MinorNL( loopColoopFree, [t], [] ) );
  conCircs := Circuits( MinorNL( loopColoopFree, [], [t] ) );

# Combine results:

  circs := Union2( 	List( delCircs, h -> ShallowCopy(h) ),		# this line ensures that the lists in circs are mutable
			List( Difference( conCircs, delCircs ), h -> Union2( h, [t] ) ) );

# Shift labels according to deletion:

  for l in loopsColoops do			## FIXME: replace this procedure by i -> ParentAttr[2][i] after having thought about it for a while
   for h in circs do
    for t in [ 1 .. Size( h ) ] do
     if h[t] >= l then h[t] := h[t] + 1; fi;
    od;
   od;
  od;

  return Union2( circs, List( Loops( matroid ), l -> [l] ) );
 end

);

##
InstallMethod( Circuits,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )

  return Combinations( GroundSet( matroid ), RankOfMatroid( matroid ) + 1 );

 end

);

##
InstallMethod( Circuits,		## incremental polynomial time method for vector matroids
		"for vector matroids",
		[ IsVectorMatroidRep and IsConnected ],
		20,

 function( matroid )
  local	corank, rank, i, j, isIndependent, superSet, ReduceDependentSetToCircuit,
	newCircuits, oldCircuits, union, intersection, currentCircuit, otherCircuit;

# If matroid is uniform, call method for uniform matroids:

  if IsUniform( matroid ) then
   return Circuits( UniformMatroidNL( RankOfMatroid( matroid ), SizeOfGroundSet( matroid ) ) );
  fi;

# Local function to find a circuit contained in a given set:

  ReduceDependentSetToCircuit := function( dependentSet )
   local element, furtherReduction, reducedSet;

   repeat

    furtherReduction := false;
    for element in dependentSet do
 
     reducedSet := Difference( dependentSet, [ element ] );
 
     if not isIndependent( reducedSet ) then			# smaller dependent set found, start over
      dependentSet := reducedSet;
      furtherReduction := true;
      break;
     fi;

    od;	# for element in dependentSet

   until not furtherReduction;

   return dependentSet;
  end;

# Initialise variables:

  rank := RankOfMatroid( matroid );
  corank := SizeOfGroundSet( matroid ) - rank;

  isIndependent := IndependenceOracle( matroid );

  newCircuits := FundamentalCircuitsWithBasis( matroid )[1];
  oldCircuits := [];

# Treat loops separately:

  newCircuits := Filtered( newCircuits, circ -> Size(circ) > 1 );

# Check circuit axiom on new circuits until no more new circuits are found:

  while not IsEmpty( newCircuits ) do

   currentCircuit := Remove( newCircuits );

   for otherCircuit in oldCircuits do

    union := Union2( currentCircuit, otherCircuit );
    intersection := Intersection2( currentCircuit, otherCircuit );

    for i in intersection do

     superSet := Difference( union, [ i ] );

     if not ForAny( newCircuits, circ -> IsSubset( superSet, circ ) ) and not ForAny( oldCircuits, circ -> IsSubset( superSet, circ ) ) then
      Add( newCircuits, ReduceDependentSetToCircuit( superSet ) );
     fi;

    od; # for i in intersection

   od; # for otherCircuit in oldCircuits

   Add( oldCircuits, currentCircuit );

  od; # while not IsEmpty( newCircuits )

  return Union2( oldCircuits, List( Loops( matroid ), loop -> [ loop ] ) );
 end

);

##
InstallMethod( Circuits,
		"for disconnected matroids",
		[ IsMatroid ],
		0,

 function( matroid )

  return Union( List( DirectSumDecomposition(matroid), s -> List( Circuits(s[2]), c -> List( c, i -> s[1][i] ) ) ) );

 end

);

##
InstallMethod( Circuits,
		"for 2-sums of matroids",
		[ IsMatroid and IsConnected ],
		0,

 function( matroid )
  local two, c1base, c1nobase, c2base, c2nobase, i;

  two := TwoSumDecomposition( matroid );
  c1nobase := [];
  c1base := Circuits( two[1] );
  c2nobase := [];
  c2base := Circuits( two[4] );

  for i in Reversed( [ 1 .. Size(c1base) ] ) do
   if not two[2] in c1base[i] then

    AddSet( c1nobase, Remove( c1base, i ) );

   fi;
  od;

  for i in Reversed( [ 1 .. Size(c2base) ] ) do
   if not two[5] in c2base[i] then

    AddSet( c2nobase, Remove( c2base, i ) );

   fi;
  od;

  c1base := List( c1base, c -> Difference( c, two[2] ) );
  c2base := List( c2base, c -> Difference( c, two[5] ) );

  return Union(
		List( c1nobase, c -> List( c, i -> two[3][i] ) ),
		List( c2nobase, c -> List( c, i -> two[6][i] ) ),
		List( Cartesian( c1base, c2base ), Union )
	);

 end

);


###############################
## FundamentalCircuitsWithBasis

##
InstallMethod( FundamentalCircuitsWithBasis,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  local basis, reduceDependentSetToCircuit, isIndependent, circs, known;

  reduceDependentSetToCircuit := function( dependentSet )
   local element, furtherReduction, reducedSet;

   repeat

    furtherReduction := false;
    for element in dependentSet do
 
     reducedSet := Difference( dependentSet, [ element ] );
 
     if not isIndependent( reducedSet ) then			# smaller dependent set found, start over
      dependentSet := reducedSet;
      furtherReduction := true;
      break;
     fi;

    od;	# for element in dependentSet

   until not furtherReduction;

   return dependentSet;
  end;

  basis := SomeBasis( matroid );
  isIndependent := IndependenceOracle( matroid );

  circs := List( Difference( GroundSet( matroid ), basis ), i -> reduceDependentSetToCircuit( Union2( basis, [i] ) ) );

  SetKnownCircuits( matroid, Union2( KnownCircuits( matroid ), circs ) );

  return [ circs, basis ];
 end

);

##
InstallMethod( FundamentalCircuitsWithBasis,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  local nf, otherLabels, unitVecLabels, rank, corank, circs, currentCircuit, i, j;

  nf := NormalFormOfVectorMatroid( matroid )[1];
  otherLabels := NormalFormOfVectorMatroid( matroid )[2];
  unitVecLabels := Difference( GroundSet( matroid ), otherLabels );

  rank := RankOfMatroid( matroid );
  corank := SizeOfGroundSet( matroid ) - rank;

  circs := [];

  for j in [ 1 .. corank ] do

   currentCircuit := [];
   for i in [ 1 .. rank ] do

    if not IsZero( MatElm( nf, i, j ) ) then Add( currentCircuit, unitVecLabels[i] ); fi;

   od;
   AddSet( currentCircuit, otherLabels[j] );

   circs[j] := currentCircuit;

  od;

  SetKnownCircuits( matroid, Union2( KnownCircuits( matroid ), circs ) );

  return [ circs, unitVecLabels ];
 end

);


################
## KnownCircuits

##
InstallMethod( KnownCircuits,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return [];
 end

);


#############
## Cocircuits

##
InstallMethod( Cocircuits,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Circuits( DualMatroid( matroid ) );
 end

);


##############
## Hyperplanes

##
InstallMethod( Hyperplanes,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Set( List( Cocircuits( matroid ), c -> Difference( GroundSet( matroid ), c ) ) );
 end

);


##################
## TuttePolynomial

##
InstallMethod( TuttePolynomial,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		30,

 function( matroid )
  local x, y, k, n;

  n := SizeOfGroundSet( matroid );
  k := RankOfMatroid( matroid );

  x := Indeterminate( Integers, 1 );
  y := Indeterminate( Integers, 2 );
  if not HasIndeterminateName( FamilyObj(x), 1 ) and not HasIndeterminateName( FamilyObj(x), 2 ) then
   SetIndeterminateName( FamilyObj(x), 1, "x" );
   SetIndeterminateName( FamilyObj(x), 2, "y" );
  fi;

  return Sum( [ 0 .. k ], i -> Binomial( n, i ) * (x-1)^(k-i) ) + Sum( [ k+1 .. n ], i -> Binomial( n, i ) * (y-1)^(i-k) );
 end

);

##
InstallMethod( TuttePolynomial,
		"for disconnected matroids",
		[ IsMatroid ],
		0,

 function( matroid )

  return Product( DirectSumDecomposition( matroid ), comp -> TuttePolynomial( comp[2] ) );

 end

);

##
InstallMethod( TuttePolynomial,
		"generic method for connected matroids",
		[ IsMatroid and IsConnected ],
		10,

 function( matroid )
  local loopNum, coloopNum, loopsColoops, x, y, p, min, n;

  x := Indeterminate( Integers, 1 );
  y := Indeterminate( Integers, 2 );

  loopNum := Size( Loops( matroid ) );
  coloopNum := Size( Coloops( matroid ) );

  p := x^coloopNum * y^loopNum;

  n := SizeOfGroundSet( matroid );

# Termination case:

  if loopNum + coloopNum = n then
   return p;
  fi;

# Recursion:

  loopsColoops := Union2( Loops( matroid ), Coloops( matroid ) );

  min := MinorNL( matroid, loopsColoops, [] );

  n := GroundSet( min )[1];

  return p * ( TuttePolynomial( MinorNL( min, [n], [] ) ) + TuttePolynomial( MinorNL( min, [], [n] ) ) );
 end

);

##
InstallMethod( TuttePolynomial,
		"for connected vector matroids",
		[ IsVectorMatroidRep and IsConnected ],
		20,

 function( matroid )
  local x, y, recursiveTutteCon, recursiveTutteDel, recursionStep, loopsColoops, minorMat, k, n;

  x := Indeterminate( Integers, 1 );
  y := Indeterminate( Integers, 2 );
  if not HasIndeterminateName( FamilyObj(x), 1 ) and not HasIndeterminateName( FamilyObj(x), 2 ) then
   SetIndeterminateName( FamilyObj(x), 1, "x" );
   SetIndeterminateName( FamilyObj(x), 2, "y" );
  fi;

##
# Check after contraction:

  recursiveTutteCon := function( minorMatrix )
   local nonLoops;

# Contraction may create new loops, check for those:
   nonLoops := NonZeroColumns( minorMatrix );

   if Size( nonLoops ) < NrColumns( minorMatrix ) then
    return y^( NrColumns(minorMatrix) - Size(nonLoops) ) * recursionStep( CertainColumns( minorMatrix, nonLoops ) );
   fi;

   return recursionStep( minorMatrix );
  end;		# recursiveTutteCon
##

##
# Check after deletion:

  recursiveTutteDel := function( minorMatrix )
   local nonColoops;

# Contraction may create new coloops, check for those:
   nonColoops := NonZeroRows( minorMatrix );

   if Size( nonColoops ) < NrRows( minorMatrix ) then
    return x^( NrRows(minorMatrix) - Size(nonColoops) ) * recursionStep( CertainRows( minorMatrix, nonColoops ) );
   fi;

   return recursionStep( minorMatrix );
  end;		# recursiveTutteDel
##

##
# Basic recursion step:

  recursionStep := function( mat )
   local i, j, c, nz, rdim, cdim, col, delMat;

# Termination:
   rdim := NrRows( mat );
   cdim := NrColumns( mat );

   if rdim = 1 then
    return x - 1 + Sum( [ 1 .. cdim + 1 ], j -> Binomial(cdim+1,j) * (y-1)^(j-1) );
   elif cdim = 1 then
    return y - 1 + Sum( [ 0 .. rdim ], j -> Binomial(rdim+1,j) * (x-1)^(rdim-j) );
   elif rdim = 0 then
    return y^cdim;
   elif cdim = 0 then
    return x^rdim;
   fi;

# Find first non-zero entry in first row:
   for i in [ 1 .. NrColumns(mat) ] do
    nz := MatElm( mat, 1, i );
    if not IsZero( nz ) then
     col := i;
     break;
    fi;
   od;

# Compute matrix for deletion minor:
   delMat := EntriesOfHomalgMatrixAsListList( CertainColumns( mat, Difference( [1..NrColumns(mat)], [col] ) ) );
   cdim := cdim - 1;

   for i in [ 2 .. rdim ] do
    c := MatElm( mat, i, col );
    if not IsZero( c ) then
     c := -c/nz;
     for j in [ 1 .. cdim ] do
      delMat[i][j] := delMat[i][j] + c*delMat[1][j];
     od;
    fi;
   od;

   delMat := HomalgMatrix( delMat, HomalgRing(mat) );

   return recursiveTutteCon( CertainRows( mat, [ 2 .. NrRows(mat) ] ) )
	+ recursiveTutteDel( delMat );
  end;
##

# Prepare for recursion:

  minorMat := NormalFormOfVectorMatroid( matroid )[1];
  loopsColoops := Union2( Loops( matroid ), Coloops( matroid ) );
  minorMat := CertainRows( CertainColumns( minorMat, NonZeroColumns( minorMat ) ), NonZeroRows( minorMat ) );

  return x^Size( Coloops( matroid ) ) * y^Size( Loops( matroid ) ) * recursionStep( minorMat );
 end

);


###########################
## RankGeneratingPolynomial

##
InstallMethod( RankGeneratingPolynomial,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local x, y;
  x := Indeterminate( Integers, 1 );
  y := Indeterminate( Integers, 2 );
  return Value( TuttePolynomial( matroid ), [ x, y ], [ x+1, y+1 ] );
 end

);


########
## Loops

##
InstallMethod( Loops,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],

 function( matroid )

  return Coloops( DualMatroid( matroid ) );

 end

);

##
InstallMethod( Loops,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )

  return ZeroColumns( MatrixOfVectorMatroid( matroid ) );

 end

);

##
InstallMethod( Loops,
		"fallback method",
		[ IsMatroid ],

 function( matroid )
  local isIndep;

  isIndep := IndependenceOracle( matroid );

  return Filtered( GroundSet( matroid ), l -> not isIndep( [l] ) );
 end

);


##########
## Coloops

##
InstallMethod( Coloops,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],
		30,

 function( matroid )
  local is, b;

  is := GroundSet( matroid );
  for b in Bases( matroid ) do
   is := Intersection2( is, b );
   if IsEmpty( is ) then break; fi;
  od;

  return is;
 end

);

##
InstallMethod( Coloops,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		40,

 function( matroid )

  if HasNormalFormOfVectorMatroid( matroid ) then

   if IsEmpty( NormalFormOfVectorMatroid( matroid )[2] ) then

    return GroundSet( matroid );

   else

    return List( ZeroRows( NormalFormOfVectorMatroid( matroid )[1] ), i -> Difference( GroundSet( matroid ), NormalFormOfVectorMatroid( matroid )[2] )[i] );

   fi;

  else

   return Loops( DualMatroid( matroid ) );

  fi;

 end

);

##
InstallMethod( Coloops,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		50,

 function( matroid )

  if RankOfMatroid( matroid ) = SizeOfGroundSet( matroid ) then

   return GroundSet( matroid );

  else

   return [];

  fi;

 end

);

##
InstallMethod( Coloops,
		"for matroids with known duals",
		[ IsMatroid and HasDualMatroid ],
		10,

 function( matroid )

  return Loops( DualMatroid( matroid ) );

 end

);

##
InstallMethod( Coloops,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )

  return Difference( GroundSet( matroid ), Union( FundamentalCircuitsWithBasis( matroid )[1] ) );

 end

);


####################
## AutomorphismGroup

##
InstallMethod( AutomorphismGroup,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		30,

 function( matroid )
  return SymmetricGroup( SizeOfGroundSet( matroid ) );
 end

);

##
InstallMethod( AutomorphismGroup,		# this is a HORRIBLE method
		"fallback method",
		[ IsMatroid ],

 function( matroid )

  return Stabiliser( SymmetricGroup( SizeOfGroundSet(matroid) ), Bases(matroid), OnSetsSets );

 end

);


#########################
## DirectSumDecomposition

##
InstallMethod( DirectSumDecomposition,
		"for connected matroids",
		[ IsMatroid and IsConnected ],
		30,

 function( matroid )
  return [ [ GroundSet(matroid), matroid ] ];
 end

);

##
InstallMethod( DirectSumDecomposition,
		"for matroids",
		[ IsMatroid ],
		0,

 function( matroid )
  local fundcircs, circ, i, j, currentComponent, components, section;

  fundcircs := ShallowCopy( FundamentalCircuitsWithBasis( matroid )[1] );
  components := [];

# Determine partition of ground set by fundamental circuits:

  while not IsEmpty( fundcircs ) do

   circ := Remove( fundcircs );
   i := 1;
   currentComponent := [ circ ];

   while i <= Size( currentComponent ) do

    circ := currentComponent[i];
    j := 1;

    while j <= Size( fundcircs ) do

     section := Intersection2( fundcircs[j], circ );

     if not IsEmpty( section ) then
 
      Add( currentComponent, Remove(fundcircs,j) );
 
     else

      j := j + 1;

     fi;

    od;
 
    i := i+1;

   od;

   AddSet( components, Union( currentComponent ) );

  od;

# Add components consisting of coloops:

  components := Union2( components, List( Coloops(matroid), l -> [l] ) );

  components := List( components, comp -> [ comp, Minor( matroid, Difference(GroundSet(matroid),comp), [] ) ] );

  for currentComponent in components do
   SetIsConnected( currentComponent[2], true );
  od;

  return components;
 end

);


######################
## TwoSumDecomposition

##
#InstallMethod( TwoSumDecomposition,
#		"for matroids",
#		[ IsMatroid ],
#
# function( matroid )
# end
#
#);


####################################
##
## Properties
##
####################################

############
## IsUniform

##
InstallMethod( IsUniform,
		"for matroids with bases",
		[ IsMatroid and HasBases ],
		30,

 function( matroid )
  return Size( Bases( matroid ) ) = Binomial( SizeOfGroundSet( matroid ), RankOfMatroid( matroid ) );
 end

);

##
InstallMethod( IsUniform,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )
  local k, isIndep, n, potIter, x;

  n := SizeOfGroundSet( matroid );
  k := RankOfMatroid( matroid );

  if k = 0 or k = n then return true; fi;

  isIndep := IndependenceOracle( matroid );

  potIter := IteratorOfCombinations( [ 1 .. n ], k );

  for x in potIter do
   if not isIndep(x) then return false; fi;
  od;

  return true;
 end

);


##################
## IsSimpleMatroid

##
InstallMethod( IsSimpleMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return SimplifiedMatroid( matroid ) = matroid;
 end

);

##
InstallMethod( IsSimple, "for matroids", [ IsMatroid ], IsSimpleMatroid );


############
## IsGraphic

##
InstallMethod( IsGraphic,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


############
## IsRegular

##
InstallMethod( IsRegular,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


##############
## IsConnected

##
InstallMethod( IsConnected,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

  return Size( DirectSumDecomposition( matroid ) ) = 1;

 end

); 


###############
## Is3Connected

##
#InstallMethod( Is3Connected,
#		"for matroids",
#		[ IsMatroid ],
#
# function( matroid )
# end
#
#);


####################################
##
## Methods
##
####################################

############
## SomeBasis

##
InstallMethod( SomeBasis,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		30,

 function( matroid )

  return [1..RankOfMatroid(matroid)];

 end

);

##
InstallMethod( SomeBasis,
		"for matroids with bases",
		[ IsMatroid and HasBases ],
		30,

 function( matroid )

  return Bases( matroid )[1];

 end

);

##
InstallMethod( SomeBasis,
		"for matroids with known bases",
		[ IsMatroid and HasKnownBases ],
		20,

 function( matroid )

  if not IsEmpty( KnownBases( matroid ) ) then
   return KnownBases( matroid )[1];
  else
   TryNextMethod();
  fi;

 end

);

##
InstallMethod( SomeBasis,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		10, 

 function( matroid )
  local basis;

  basis := Difference( GroundSet( matroid ), NormalFormOfVectorMatroid( matroid )[2] );
  AddSet( KnownBases( matroid ), basis );

  return basis;
 end

);

##
InstallMethod( SomeBasis,
		"fallback method",
		[ IsMatroid ],
		0,

 function( matroid )
  local indep, tmp, i, isIndep;

  isIndep := IndependenceOracle( matroid );
  indep := [];
  i := 1;

  while i <= SizeOfGroundSet( matroid ) do

   tmp := Union2( indep, [i] );

   if isIndep( tmp ) then indep := tmp; fi;

   i := i + 1;

  od;

  AddSet( KnownBases( matroid ), indep );

  return indep;
 end

);


########################
## MatrixOfVectorMatroid

##
InstallMethod( MatrixOfVectorMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
 
  if IsBound( matroid!.generatingMatrix ) then
   return matroid!.generatingMatrix;
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;

 end

);


#############
## HomalgRing

##
InstallMethod( HomalgRing,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )

  return HomalgRing( MatrixOfVectorMatroid( matroid ) );

 end

);


##########
## MinorNL

##
InstallMethod( MinorNL,
		"for empty arguments",
		[ IsMatroid, IsList and IsEmpty, IsList and IsEmpty ],
		50,

 function( matroid, del, contr )
  return matroid;
 end

);

##
InstallMethod( MinorNL,
		"fallback method for connected matroids",
		[ IsMatroid and IsConnected, IsList, IsList ],
		0,

 function( matroid, del, contr )

  Bases( matroid );

  return MinorNL( matroid, del, contr );

 end

);

##
InstallMethod( MinorNL,
		"for disconnected matroids",
		[ IsMatroid and HasDirectSumDecomposition, IsList, IsList ],
		30,

 function( matroid, del, contr )
  local minor;

  if IsConnected( matroid ) then TryNextMethod(); fi;		# prevent this method from calling itself on the same arguments

  minor := Iterated( List( DirectSumDecomposition(matroid), s -> MinorNL( s[2],
							List( Intersection2(del,s[1]), i -> Position(s[1],i) ),
							List( Intersection2(contr,s[1]), i -> Position(s[1],i) ) ) ),
		DirectSumOfMatroidsNL );

  ObjectifyWithAttributes( minor, TheTypeMinorOfAbstractMatroid,
			ParentAttr, [ matroid, Difference( GroundSet(matroid), Union2(del,contr) ) ] );

  return minor;
 end

);

##
InstallMethod( MinorNL,
		"for uniform matroids",
		[ IsMatroid and IsUniform, IsList, IsList ],
		30,

 function( matroid, del, contr )
  local minor, minSize, minRank;

  minSize := SizeOfGroundSet(matroid) - Size(del) - Size(contr);
  minRank := Maximum( Minimum( RankOfMatroid(matroid) - Size(contr), minSize ), 0 );

  minor := UniformMatroid( minRank, minSize );

  ObjectifyWithAttributes( minor, TheTypeMinorOfAbstractMatroid,
			ParentAttr, [ matroid, Difference( GroundSet(matroid), Union2(del,contr) ) ] );

  return minor;
 end

);

##
InstallMethod( MinorNL,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases, IsList, IsList ],
		10,

 function( matroid, del, contr )
  local minorBases, t, sdel, scontr, minor, loopsColoops, groundSetInParent;

  sdel := Set( del );
  scontr := Set( contr );
  if not IsEmpty( Intersection2( sdel, scontr ) ) then Error( "<del> and <contr> must not meet" ); fi;

# If loops or coloops will be deleted or contracted, delete rather than contract:

  loopsColoops := Intersection2( Union2( Loops( matroid ), Coloops( matroid ) ), scontr );
  scontr := Difference( scontr, loopsColoops );
  sdel := Union2( sdel, loopsColoops );

  minorBases := ShallowCopy( Bases( matroid ) );

# Deletion:

  for t in sdel do
   if ForAll( minorBases, b -> t in b ) then		# t is a coloop in the current minor
    minorBases := List( minorBases, b -> Difference(b,[t]) );
   else
    minorBases := Filtered( minorBases, b -> not t in b );
   fi;
  od;

# Contraction:

  for t in scontr do
   if ForAny( minorBases, b -> t in b ) then		# t is not a loop in the current minor
    minorBases := List( Filtered( minorBases, b -> t in b ), b -> Difference(b,[t]) );
   fi;
  od;

# Map bases to canonical ground set:

  groundSetInParent := Difference( GroundSet( matroid ), Union2( sdel, scontr ) );
  minorBases := List( minorBases, b -> List( b, i -> Position( groundSetInParent, i ) ) );

# Construct minor:

  minor := Objectify( TheTypeMinorOfAbstractMatroid, rec() );

  SetSizeOfGroundSet( minor, Size( groundSetInParent ) );
  SetBases( minor, minorBases );

  SetParentAttr( minor, [
	matroid,
	groundSetInParent
  ] );

  return minor;
 end

);

##
InstallMethod( MinorNL,
		"for vector matroids",
		[ IsVectorMatroidRep, IsList, IsList ],
		12,

 function( matroid, del, contr )
  local loopsColoops, sdel, scontr, minorMat, minor, col, row, actRows, actCols, foundRow, foundCoeff, rowCoeff, calcCol, t, mat;

  sdel := Set( del );
  scontr := Set( contr );

  if not IsEmpty( Intersection2( sdel, scontr ) ) then Error( "<del> and <contr> must not meet" ); fi;
  if not IsSubset( [ 1 .. SizeOfGroundSet( matroid ) ], Union2( sdel, scontr ) ) then Error( "<del> and <contr> must be subsets of the column labels of <matroid>" ); fi;

# If loops or coloops will be deleted or contracted, delete rather than contract:

  loopsColoops := Intersection2( Union2( Loops( matroid ), Coloops( matroid ) ), scontr );
  scontr := Difference( scontr, loopsColoops );
  sdel := Union2( sdel, loopsColoops );

# Delete columns and prepare matrix for contraction:

  mat := MatrixOfVectorMatroid( matroid );
  minorMat := EntriesOfHomalgMatrixAsListList( mat );
  actCols := Difference( GroundSet( matroid ), sdel );

# Contraction:

  if NrRows(mat) > 0 then

   actRows := [ 1 .. DimensionsMat( minorMat )[1] ];
   for col in scontr do
 
    actCols := Difference( actCols, [ col ] );
    foundRow := 0;
    for row in actRows do
 
     rowCoeff := minorMat[row][col];
     if not IsZero( rowCoeff ) then
 
      if foundRow = 0 then
 
       foundRow := row;
       foundCoeff := rowCoeff;
 
      else
 
       rowCoeff := rowCoeff/foundCoeff;
       for calcCol in actCols do
        minorMat[row][calcCol] := minorMat[row][calcCol] - rowCoeff * minorMat[foundRow][calcCol];
       od;
 
      fi;
 
     fi;
 
    od;
    actRows := Difference( actRows, [ foundRow ] );
 
   od;

  else

   actRows := [];

  fi;

  if IsEmpty( actRows ) then
   minorMat := HomalgMatrix( [], 0, Size( actCols ), HomalgRing( mat ) );
  else
   minorMat := CertainColumns( CertainRows( HomalgMatrix( minorMat, HomalgRing( mat ) ), actRows ), actCols );
  fi;

  minor := rec( generatingMatrix := Immutable( minorMat ) );
  ObjectifyWithAttributes( minor, TheTypeMinorOfVectorMatroid,
  			ParentAttr, [ matroid, Difference( GroundSet( matroid ), Union2( sdel, scontr ) ) ]
		);

  return minor;
 end

);


########
## Minor

##
InstallMethod( Minor,
		"for abstract matroids",
		[ IsMatroid, IsList, IsList ],

 function( mat, del, con )
  local min;

  min := MinorNL( mat, del, con );
  _alcove_MatroidStandardImplications( min );

  return min;
 end

);

##
InstallMethod( Minor,
		"for vector matroids",
		[ IsVectorMatroidRep, IsList, IsList ],
		10,

 function( mat, del, con )
  local min;

  min := MinorNL( mat, del, con );
  _alcove_MatroidStandardImplications( min );
  _alcove_VectorMatroidImplications( min );

  return min;
 end

);


###########
## Deletion

##
InstallMethod( Deletion,
		"for matroids",
		[ IsMatroid, IsList ],

 function( matroid, del )
  return Minor( matroid, del, [] );
 end

);


##############
## Contraction

##
InstallMethod( Contraction,
		"for matroids",
		[ IsMatroid, IsList ],

 function( matroid, contr )
  return Minor( matroid, [], contr );
 end

);


##########
## IsMinor

##
InstallMethod( IsMinor,
		"for matroids",
		[ IsMatroid, IsMinorOfMatroid ],

 function( matroid, minor )
  local parent;
  parent := ParentAttr( minor )[1];
  if IsMinorOfMatroid( parent ) then
   return IsMinor( matroid, parent );
  else
   return IsIdenticalObj( matroid, parent );
  fi;
 end

);


######################
## DirectSumOfMatroids

##
InstallMethod( DirectSumOfMatroidsNL,
		"for matroids",
		[ IsMatroid, IsMatroid ],

 function( m1, m2 )
  local sum, size1, subs;

  size1 := SizeOfGroundSet(m1);
  subs := List( GroundSet(m2), i -> i+size1 );

  sum := rec();
  ObjectifyWithAttributes( sum, TheTypeAbstractMatroid,
			SizeOfGroundSet, size1 + SizeOfGroundSet(m2),
			RankOfMatroid, RankOfMatroid(m1) + RankOfMatroid(m2),
			DirectSumDecomposition, Concatenation( DirectSumDecomposition(m1),
								List( DirectSumDecomposition(m2), tup -> [ List(tup[1],j->subs[j]), tup[2] ] ) ),
			IsConnected, false );

  return sum;
 end

);

##
InstallMethod( DirectSumOfMatroids,
		"for matroids",
		[ IsMatroid, IsMatroid ],

 function( m1, m2 )
  local sum;

  sum := DirectSumOfMatroidsNL(m1,m2);

  _alcove_MatroidStandardImplications( sum );

  return sum;
 end

);


###################
## TwoSumOfMatroids

##
InstallMethod( TwoSumOfMatroidsNL,
		"for matroids",
		[ IsMatroid, IsInt, IsMatroid, IsInt ],

 function( m1, p1, m2, p2 )
  local sum, size1, size2;

  size1 := SizeOfGroundSet( m1 );
  size2 := SizeOfGroundSet( m2 );

  if size1 < 3 or size2 < 3 then
   Error( "both matroids must have at least 3 elements" );
  fi;

  if p1 in Loops( m1 ) or p1 in Coloops( m1 ) or p2 in Loops( m2 ) or p2 in Coloops( m2 ) then
   Error( "base points must be neither loops nor coloops in their respective matroids" );
  fi;

  sum := rec();
  ObjectifyWithAttributes( sum, TheTypeAbstractMatroid,
			SizeOfGroundSet, size1 + size2 - 2,
			RankOfMatroid, RankOfMatroid(m1) + RankOfMatroid(m2) - 1,
			TwoSumDecomposition, [ 	m1, p1, Concatenation( [ 1 .. p1-1 ], [0], [ p1 .. size1-1 ] ),
						m2, p2, Concatenation( [ size1 .. size1+p2-2 ], [0], [ size1+p2-1 .. size1+size2-2 ] ) ],
			Is3Connected, false );

  return sum;
 end

);

##
InstallMethod( TwoSumOfMatroidsNL,
		"for vector matroids",
		[ IsVectorMatroidRep, IsInt, IsVectorMatroidRep, IsInt ],

 function( m1, p1, m2, p2 )
  local sum, size1, size2, ring, mat1, mat2, source, r0, r, c;

  ring := HomalgRing( m1 );
  if not IsIdenticalObj( ring, HomalgRing( m2 ) ) then					## FIGURE OUT HOW TO SOLVE THIS PROPERLY
   Error( "please make sure that the underlying rings of the matroids' matrices are identical" );
#   TryNextMethod();
  fi;

  size1 := SizeOfGroundSet( m1 );
  size2 := SizeOfGroundSet( m2 );

  if size1 < 3 or size2 < 3 then
   Error( "both matroids must have at least 3 elements" );
  fi;

  if p1 in Loops( m1 ) or p1 in Coloops( m1 ) or p2 in Loops( m2 ) or p2 in Coloops( m2 ) then
   Error( "base points must be neither loops nor coloops in their respective matroids" );
  fi;

# Compute equivalent matrix for m1 having p1 as unit vector:

  source := MatrixOfVectorMatroid( m1 );
  mat1 := EntriesOfHomalgMatrixAsListList( CertainColumns( source, Difference( [1..size1], [p1] ) ) );

  r0 := 0;
  for r in Reversed( [ 1 .. Size( mat1 ) ] ) do

   c := MatElm( source, r, p1 );

   if not IsZero( c ) then

    if r0 = 0 then
     r0 := r;
     mat1[r] := 1/c * mat1[r];
    else
     mat1[r] := mat1[r] - c * mat1[r0];
    fi;
   
   fi;

  od;

# Swap rows:

  if r0 <> Size(mat1) then
   r := mat1[Size(mat1)];
   mat1[Size(mat1)] := mat1[r0];
   mat1[r0] := r;
  fi;

  mat1 := HomalgMatrix( mat1, ring );
  
# Compute equivalent matrix for m2 having p2 as unit vector:

  source := MatrixOfVectorMatroid( m2 );
  mat2 := EntriesOfHomalgMatrixAsListList( CertainColumns( source, Difference( [1..size2], [p2] ) ) );

  r0 := 0;
  for r in [ 1 .. Size( mat2 ) ] do

   c := MatElm( source, r, p2 );

   if not IsZero( c ) then

    if r0 = 0 then
     r0 := r;
     mat2[r] := 1/c * mat2[r];
    else
     mat2[r] := mat2[r] - c * mat2[r0];
    fi;
   
   fi;

  od;

# Swap rows:

  if r0 <> 1 then
   r := mat2[1];
   mat2[1] := mat2[r0];
   mat2[r0] := r;
  fi;

  mat2 := HomalgMatrix( mat2, ring );
 
# Build new matrix and create object:

  sum := rec( generatingMatrix := UnionOfColumns(
		UnionOfRows( mat1, HomalgZeroMatrix( NrRows(mat2)-1, NrColumns(mat1), ring ) ),
		UnionOfRows( HomalgZeroMatrix( NrRows(mat1)-1, NrColumns(mat2), ring ), mat2 ) )
	 );

  ObjectifyWithAttributes( sum, TheTypeVectorMatroid,
			SizeOfGroundSet, size1 + size2 - 2,
			RankOfMatroid, RankOfMatroid(m1) + RankOfMatroid(m2) - 1,
			TwoSumDecomposition, [ 	m1, p1, Concatenation( [ 1 .. p1-1 ], [0], [ p1 .. size1-1 ] ),
						m2, p2, Concatenation( [ size1 .. size1+p2-2 ], [0], [ size1+p2-1 .. size1+size2-2 ] ) ],
			Is3Connected, false );

  return sum;
 end

);

##
InstallMethod( TwoSumOfMatroids,
		"for matroids",
		[ IsMatroid, IsInt, IsMatroid, IsInt ],

 function( m1, p1, m2, p2 )
  local sum;

  sum := TwoSumOfMatroidsNL( m1, p1, m2, p2 );

  _alcove_MatroidStandardImplications( sum );

  return sum;
 end

);

##
InstallMethod( TwoSum,
		"for matroids",
		[ IsMatroid, IsInt, IsMatroid, IsInt ],

 TwoSumOfMatroids

);

##
InstallMethod( TwoSumNL,
		"for matroids",
		[ IsMatroid, IsInt, IsMatroid, IsInt ],

 TwoSumOfMatroidsNL

);


####################################
##
## Operators
##
####################################

############
## Addition:

##
InstallMethod( \+,
		"direct sum of matroids",
		[ IsMatroid, IsMatroid ],

 DirectSumOfMatroids

);


#########
## Equal:

##
InstallMethod( \=,
		"for matroids with bases",
		[ IsMatroid and IsConnected and HasBases, IsMatroid and IsConnected and HasBases ],
		20,

 function( m1, m2 )

  return SizeOfGroundSet(m1) = SizeOfGroundSet(m2) and Bases(m1) = Bases(m2);

 end

);

##
InstallMethod( \=,
		"for matroids with circuits",
		[ IsMatroid and IsConnected and HasCircuits, IsMatroid and IsConnected and HasCircuits ],
		30,

 function( m1, m2 )

  return SizeOfGroundSet(m1) = SizeOfGroundSet(m2) and Circuits(m1) = Circuits(m2);

 end

);

##													## NAIVE METHOD
InstallMethod( \=,
		"fallback method",
		[ IsMatroid and IsConnected, IsMatroid and IsConnected ],
		10,

 function( m1, m2 )

  if SizeOfGroundSet(m1) <> SizeOfGroundSet(m2) then return false; fi;
  if RankOfMatroid(m1) <> RankOfMatroid(m2) then return false; fi;

  return Circuits(m1) = Circuits(m2);

 end

);

##
InstallMethod( \=,
		"for disconnected matroids",
		[ IsMatroid, IsMatroid ],
		0,

 function( m1, m2 )

  return DirectSumDecomposition( m1 ) = DirectSumDecomposition( m2 );

 end

);


####################################
##
## Constructors
##
####################################

########
## Copy:

##
InstallMethod( Matroid,
		"copy constructor",
		[ IsMatroid ],

 IdFunc

);


###################
## Vector matroids:

##
InstallMethod( Matroid,
		"by empty matrix",
		[ IsGeneralizedRowVector and IsNearAdditiveElementWithInverse and IsAdditiveElement ],

 function( mat )
  local matroid;

  if not IsEmpty( mat[1] ) then Error( "constructor for empty vector matroids called on non-empty matrix" ); fi;

  matroid := rec( generatingMatrix := Immutable( HomalgMatrix(mat,HomalgRingOfIntegers(2)) ) );
  ObjectifyWithAttributes( matroid, TheTypeVectorMatroid,
			SizeOfGroundSet, 0,
			RankOfMatroid, 0
	);

  _alcove_MatroidStandardImplications( matroid );
  _alcove_VectorMatroidImplications( matroid );

  return matroid;
 end

);

##
InstallMethod( MatroidNL,
		"by homalg matrix, no logical implications",
		[ IsHomalgMatrix ],
		30,

 function( matobj )
  local matroid;

  matroid := Objectify( TheTypeVectorMatroid, rec( generatingMatrix := Immutable(matobj) ) );

  return matroid;
 end

);

##
InstallMethod( Matroid,
		"by homalg matrix",
		[ IsHomalgMatrix ],
		30,

 function( matobj )
  local matroid;

  matroid := Objectify( TheTypeVectorMatroid, rec( generatingMatrix := Immutable(matobj) ) );

  _alcove_MatroidStandardImplications( matroid );
  _alcove_VectorMatroidImplications( matroid );

  return matroid;
 end

);


#####################
## Abstract matroids:

##
InstallMethod( MatroidByBases,
		"by size of ground set and list of bases",
		[ IsInt, IsList ],

 function( deg, baselist  )
  local matroid;

# Convert lists to sets if necessary:
  baselist := Set( List( baselist, Set ) );

  if IsEmpty( baselist ) then Error( "the list of bases must be non-empty" ); fi;

  if ForAny( baselist, i -> not IsSubset( [1..deg], i ) ) then
   Error( "elements of <baselist> must be subsets of [1..<deg>]" );
  fi;

# Check basis exchange axiom:
  if ForAny( baselist, b1 -> ForAny( baselist, b2 ->
	ForAny( Difference(b1,b2), e -> ForAll( Difference(b2,b1), f ->
		not Union2( Difference( b1, [e] ), [f] ) in baselist
	) )
  ) ) then Error( "bases must satisfy the exchange axiom" ); fi;

  matroid := MatroidByBasesNCL( deg, baselist );

  _alcove_MatroidStandardImplications( matroid );

  return matroid;

 end

);

##
InstallMethod( MatroidByBasesNCL,
		"by size of ground set and list of bases, no checks or logical implications",
		[ IsInt, IsList ],

 function( deg, baselist  )
  local matroid;

  matroid := rec();
  ObjectifyWithAttributes( matroid, TheTypeAbstractMatroid,
			Bases, baselist,
			SizeOfGroundSet, deg );

  return matroid;
 end

);

##
InstallMethod( MatroidByBases,
		"by ground set and list of bases",
		[ IsList, IsList ],

 function( groundSet, bases )

  return MatroidByBases( Size( groundSet ), List( bases, b -> List( b, e -> Position( groundSet, e ) ) ) );

 end

);

##
InstallMethod( MatroidByBasesNCL,
		"by ground set and list of bases, no checks or logical implications",
		[ IsList, IsList ],

 function( groundSet, bases )

  return MatroidByBasesNCL( Size( groundSet ), List( bases, b -> List( b, e -> Position( groundSet, e ) ) ) );

 end

);

###
#InstallMethod( MatroidByIndependenceOracle,
#		"given size of ground set and boolean function deciding independence of subsets",
#		[ IsInt, IsFunction ],
#
# function( size, isIndep )
#  local matroid;
#
#  #####
#  ## Checks go here!
#  #####
#
#  matroid := MatroidByIndependenceOracleNCL( size, isIndep );
#
#  _alcove_MatroidStandardImplications( matroid );
#
#  return matroid;
# end
#
#);

##
InstallMethod( MatroidByIndependenceOracleNCL,
		"given size of ground set and boolean function deciding independence of subsets, no checks or logical implications",
		[ IsInt, IsFunction ],

 function( size, isIndep )
  local matroid;

  matroid := rec();
  ObjectifyWithAttributes( matroid, TheTypeAbstractMatroid,
			SizeOfGroundSet, size,
			IndependenceOracle, isIndep );

  return matroid;
 end

);

###
#InstallMethod( MatroidByIndependenceOracle,
#		"given ground set and boolean function deciding independence of subsets",
#		[ IsList, IsFunction ],
#
# function( groundSet, isIndep )
#
#  if groundSet = [ 1 .. Size( groundSet ) ] then
#
#   return MatroidByIndependenceOracle( Size( groundSet ), isIndep );
#
#  else
#
#   return MatroidByIndependenceOracle( Size( groundSet ), function(i) return isIndep( groundSet[i] ); end );
#
#  fi;
#
# end
#
#);

##
InstallMethod( MatroidByIndependenceOracleNCL,
		"given ground set and boolean function deciding independence of subsets, no checks or logical implications",
		[ IsList, IsFunction ],

 function( groundSet, isIndep )

  if groundSet = [ 1 .. Size( groundSet ) ] then

   return MatroidByIndependenceOracleNCL( Size( groundSet ), isIndep );

  else

   return MatroidByIndependenceOracleNCL( Size( groundSet ), function(i) return isIndep( groundSet[i] ); end );

  fi;

 end

);

##
InstallMethod( MatroidByCircuits,
		"given size of ground set and list of circuits",
		[ IsInt, IsList ],

 function( size, circs )
  local matroid;

# Convert lists to sets if necessary:
  circs := Set( List( circs, Set ) );

# Check circuit axioms:
  if [] in circs then Error( "the empty set must not be a circuit" ); fi;

  if ForAny( circs, c1 -> ForAny( circs, c2 -> c1 <> c2 and IsSubset( c1, c2 ) ) ) then
   Error( "circuits must not contain each other" );
  fi;

  if	ForAny( circs, c1 ->
		ForAny( circs, c2 -> c1 <> c2 and
			ForAny( Intersection2( c1, c2 ), i ->
				ForAll( circs, c3 -> not IsSubset( Difference( Union2( c1, c2 ), [i] ), c3 ) )
			)
		)
	) then
   Error( "circuits must satisfy the circuit elimination axiom" );
  fi;

  matroid := MatroidByCircuitsNCL( size, circs );

  _alcove_MatroidStandardImplications( matroid );

  return matroid;
 end

);

##
InstallMethod( MatroidByCircuitsNCL,
		"given size of ground set and list of circuits no checks or logical implications",
		[ IsInt, IsList ],

 function( size, circs )
  local matroid;

  matroid := rec();
  ObjectifyWithAttributes( matroid, TheTypeAbstractMatroid,
			SizeOfGroundSet, size,
			Circuits, circs );

  return matroid;
 end

);

##
InstallMethod( MatroidByCircuits,
		"given ground set and list of circuits",
		[ IsList, IsList ],

 function( groundSet, circs )

  return MatroidByCircuits( Size( groundSet ), List( circs, b -> List( b, e -> Position( groundSet, e ) ) ) );

 end

);

##
InstallMethod( MatroidByCircuitsNCL,
		"given ground set and list of circuits no checks or logical implications",
		[ IsList, IsList ],

 function( groundSet, circs )

  return MatroidByCircuitsNCL( Size( groundSet ), List( circs, b -> List( b, e -> Position( groundSet, e ) ) ) );

 end

);

##
InstallMethod( MatroidByRankFunction,
		"given size of ground set and integer valued function",
		[ IsInt, IsFunction ],

 function( size, rankFunc )
  local x, xIter, rx, y, yIter, ry, matroid;

  if rankFunc( [] ) <> 0 then Error( "the empty set must have rank 0" ); fi;

# Naive check for sane rank function:

  xIter := IteratorOfCombinations( [1..size] );
  for x in xIter do

   rx := rankFunc(x);
   if rx > Size(x) then Error( "the rank of a set must not exceed its size" ); fi;

   yIter := ShallowCopy(xIter);		# do not test both (x,y) and (y,x)
   for y in yIter do

    ry := rankFunc(y);

    if ( rx > ry and IsSubset( y, x ) ) or ( rx < ry and IsSubset( x, y ) ) then	# the iterator will probably never produce
											# the case IsSubset( x, y ), but this is not guaranteed
     Error( "for sets x \subset y the rank of x must be at most the rank of y" );
    fi;

    if rankFunc( Union2( x, y ) ) + rankFunc( Intersection2( x, y ) ) > rx + ry then
     Error( "<rankFunc> is not submodular" );
    fi;

   od;

  od;

# Construction:

  matroid := MatroidByRankFunctionNCL( size, rankFunc );

  _alcove_MatroidStandardImplications( matroid );

  return matroid;
 end

);

##
InstallMethod( MatroidByRankFunctionNCL,
		"given size of ground set and integer valued function, no checks or logical implications",
		[ IsInt, IsFunction ],

 function( size, rankFunc )
  local matroid;

  matroid := rec();
  ObjectifyWithAttributes( matroid, TheTypeAbstractMatroid,
			SizeOfGroundSet, size,
			RankFunction, rankFunc );

  return matroid;
 end

);

##
InstallMethod( MatroidByRankFunction,
		"given size of ground set and integer valued function",
		[ IsList, IsFunction ],

 function( groundSet, rankFunc )

  if groundSet = [ 1 .. Size( groundSet ) ] then

   return MatroidByRankFunction( Size( groundSet ), rankFunc );

  else

   return MatroidByRankFunction( Size( groundSet ), function(i) return rankFunc( groundSet[i] ); end );

  fi;

 end

);

##
InstallMethod( MatroidByRankFunctionNCL,
		"given size of ground set and integer valued function, no checks or logical implications",
		[ IsList, IsFunction ],

 function( groundSet, rankFunc )

  if groundSet = [ 1 .. Size( groundSet ) ] then

   return MatroidByRankFunctionNCL( Size( groundSet ), rankFunc );

  else

   return MatroidByRankFunctionNCL( Size( groundSet ), function(i) return rankFunc( groundSet[i] ); end );

  fi;

 end

);


####################
## Graphic matroids:

###
#InstallMethod( MatroidOfGraph,
#		"given an incidence matrix",
#		[ IsMatrix ],
#
# function( incidencemat )
#
# end
#
#);


########################
## Special constructors:

##
InstallMethod( RandomVectorMatroidOverPrimeField,
		"of certain dimensions over a prime field",
		[ IsInt, IsInt, IsInt ],

 function( k, n, p )

  if not ( IsPrimeInt(p) or p = 0 ) or k < 0 or n < 0 then
   Error( "usage: RandomVectorMatroidOverPrimeField( <rows>, <cols>, <char> )" );
  fi;

  if p = 0 then
   return Matroid( HomalgMatrix( RandomMat( k, n, Rationals ), HomalgFieldOfRationals() ) );
  else
   return Matroid( HomalgMatrix( RandomMat( k, n, [ 1 .. p ] ), HomalgRingOfIntegers(p) ) );
  fi;

 end

);

##
InstallMethod( UniformMatroid,
		"as an abstract matroid",
		[ IsInt, IsInt ],

 function( k, n )
  local matroid;

  if k > n then k := n; fi;

  matroid := MatroidByRankFunctionNCL( n, function( x ) if Size(x) < k then return Size(x); else return k; fi; end );
  SetRankOfMatroid( matroid, k );

  _alcove_MatroidStandardImplications( matroid );

  SetIsUniform( matroid, true );

  return matroid;
 end

);

##
InstallMethod( UniformMatroidNL,
		"as an abstract matroid, no logical implications",
		[ IsInt, IsInt ],

 function( k, n )
  local matroid;

  if k > n then k := n; fi;

  matroid := MatroidByRankFunctionNCL( n, function( x ) if Size(x) < k then return Size(x); else return k; fi; end );
  SetRankOfMatroid( matroid, k );

  SetIsUniform( matroid, true );

  return matroid;
 end

);


####################################
##
## Display Methods
##
####################################

##
InstallMethod( PrintObj,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

  if SizeOfGroundSet( matroid ) = 0 then

   Print( "<The boring matroid>" );

  else

   Print( "<A" );
 
   if HasRankOfMatroid( matroid ) then
    Print( " rank ", RankOfMatroid(matroid) );
   fi;
 
   if HasIsUniform( matroid ) and IsUniform( matroid ) then
    Print( " uniform" );
   elif HasIsSimpleMatroid( matroid ) and IsSimpleMatroid( matroid ) then
    Print( " simple" );
   fi;
 
   if IsVectorMatroidRep( matroid ) then
    Print( " vector" );
   fi;
 
   Print( " matroid>" );

  fi;

 end

);

##
InstallMethod( Display,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  local printList, i, pSize;

  if HasDirectSumDecomposition(matroid) and Size(DirectSumDecomposition(matroid)) = 1 and not IsIdenticalObj( matroid, DirectSumDecomposition(matroid)[1][2] ) then

   Display( DirectSumDecomposition(matroid)[1][2] );

  else

   Print( "A rank ", RankOfMatroid(matroid), " abstract matroid on ", SizeOfGroundSet(matroid), " elements.\n" );
   printList := [];
   if HasBases(matroid) then Add(printList,"bases"); fi;
   if HasCircuits(matroid) then Add(printList,"circuits"); fi;
   if HasRankFunction(matroid) then Add(printList,"rank function"); fi;
 
   if IsEmpty( printList ) then
 
    if HasDirectSumDecomposition( matroid ) and not IsConnected( matroid ) then
     Print( "It is the direct sum of ", Size(DirectSumDecomposition(matroid)), " connected components.\n" );
    elif HasTwoSumDecomposition( matroid ) and not Is3Connected( matroid ) then
     Print( "It is a 2-sum of two known minors.\n" );
    else
     Print( "It is pretty much an empty prototype of a matroid, knowing neither its bases, nor its circuits, nor its rank function.\n" );
    fi;
 
   else
 
    pSize := Size(printList);
 
    Print( "Its " );
 
    Print( printList[1] );
 
    for i in [2..pSize-1] do
     Print( ", ", printList[i] );
    od;
 
    if pSize > 1 then
     Print( " and ", printList[pSize] );
    fi;
 
    if pSize = 1 and printList[1][1] = 'r' then
     Print( " is known.\n" );
    else
     Print( " are known.\n" );
    fi;
 
   fi;

  fi;

 end

);

##
InstallMethod( Display,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  local mat;

  if SizeOfGroundSet( matroid ) = 0 then

   Print( "The vector matroid of the empty matrix." );

  else

   mat := MatrixOfVectorMatroid( matroid );

   Print( "The vector matroid of this matrix over " );
   View( HomalgRing(mat) );
   Print( ":\n" );
   Display( mat );

  fi;

 end

);
