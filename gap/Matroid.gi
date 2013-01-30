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
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],

 function( matroid )
  local dualbases, dual;

  dualbases := Set( List( Bases( matroid ), b -> Difference( GroundSet( matroid ), b ) ) );

  dual := MatroidByBasesNCL( GroundSet( matroid ), dualbases );
  SetDualMatroid( dual, matroid );

  return dual;

 end

);

##
InstallMethod( DualMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  local dualmatrix, dual, mat;
  mat := MatrixOfVectorMatroid( matroid );

  dualmatrix := SyzygiesOfRows( Involution( mat ) );

  dual := Matroid( dualmatrix );
  SetDualMatroid( dual, matroid );

  return dual;

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


#######
## Rank

##
InstallMethod( RankOfMatroid,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],

 function( matroid )
  return Size( Bases(matroid)[1] );
 end

);

##
InstallMethod( RankOfMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return RowRankOfMatrix( MatrixOfVectorMatroid(matroid) );
 end

);

##
InstallMethod( Rank,
		"alias for Rank for matroids",
		[ IsMatroid ],

 RankOfMatroid

);


################
## Rank function

##
InstallMethod( RankFunction,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],

 function( matroid )
  return
	function( X )
	 local b, max, s;

	 max := 0;

	 for b in Bases( matroid ) do

	  s := Size( Intersection2( b, X ) );
	  if s > max then

	   max := s;
	   if max = Size( X ) then return max; fi;

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

 function( matroid )
  return function( X ) return RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid( matroid ), X ) ); end;
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
	function( X )
	 local loopsOfMinor, minor;

	 minor := MinorNL( matroid, [], X );

	 loopsOfMinor := List( Loops( minor ), l -> ParentAttr(minor)[2][l] );

	 return Union2( X, loopsOfMinor );
	end;
 end

);


#######################
## IndependenceFunction

##
InstallMethod( IndependenceFunction,
		"for vector matroids",
		[ IsVectorMatroidRep ],
		30,

 function( matroid )
  return
	function( X )
	 local nf, unitVecLabels, otherLabels, checkMat, unitVecsInX, nrCols;

	 nf := NormalFormOfVectorMatroid( matroid );
	 otherLabels := nf[2];
	 unitVecLabels := Difference( GroundSet( matroid ), otherLabels );

	 checkMat := CertainColumns( nf[1], List( Intersection2( X, otherLabels ), col -> Position( otherLabels, col ) ) );

         nrCols := NrColumns( checkMat );
         if nrCols = 0 then return true; fi;

         unitVecsInX := Intersection2( X, unitVecLabels );
         checkMat := CertainRows( checkMat, Difference( [ 1 .. NrRows( checkMat ) ], List( unitVecsInX, row -> Position( unitVecLabels, row ) ) ) );

         if NrRows( checkMat ) < nrCols then return false; fi;

         return ColumnRankOfMatrix( checkMat ) = nrCols;
	end;
 end

);

##
InstallMethod( IndependenceFunction,
		"for matroids with bases",
		[ IsMatroid and HasBases ],
		10,

 function( matroid )
  return
	function( X )
	 return ForAny( Bases( matroid ), b -> IsSubset( b, X ) );
	end;
 end

);

##
InstallMethod( IndependenceFunction,
		"for matroids with circuits",
		[ IsMatroid and HasCircuits ],
		20,

 function( matroid )
  return
	function( X )
	 return ForAll( Circuits( matroid ), c -> not IsSubset( X, c ) );
	end;
 end

);


########
## Bases

##
InstallMethod( Bases,
		"for uniform matroids",
		[ IsMatroid and IsUniform ],
		30,

 function( matroid )
  return Combinations( GroundSet( matroid ), RankOfMatroid( matroid ) );
 end

);

##
InstallMethod( Bases,				# THIS IS AN EXTREMELY NAIVE APPROACH
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], RankOfMatroid( matroid ) ),
		b -> RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid(matroid), b ) ) = RankOfMatroid( matroid ) );
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
		"for matroids",
		[ IsMatroid ],

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
		10,

 function( matroid )

  return Combinations( GroundSet( matroid ), RankOfMatroid( matroid ) + 1 );

 end

);

##
InstallMethod( Circuits,		## incremental polynomial time method for vector matroids
		"for vector matroids",
		[ IsVectorMatroidRep ],

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

  isIndependent := IndependenceFunction( matroid );

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
  isIndependent := IndependenceFunction( matroid );

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
		"method for disconnected matroids",
		[ IsMatroid ],

 function( matroid )

  return Product( DirectSumDecomposition( matroid ), comp -> TuttePolynomial( comp[2] ) );

 end

);

##
InstallMethod( TuttePolynomial,
		"generic method for connected matroids",
		[ IsMatroid and IsConnected ],

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
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

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


##########
## Coloops

##
InstallMethod( Coloops,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases ],

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
InstallMethod( AutomorphismGroup,
		"for matroids",
		[ IsMatroid ],

 function( matroid )


 end

);


#########################
## DirectSumDecomposition

##
InstallMethod( DirectSumDecomposition,
		"for connected matroids",
		[ IsMatroid and IsConnected ],

 function( matroid )
  return [ GroundSet(matroid), matroid ];
 end

);

##
InstallMethod( DirectSumDecomposition,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local fundcircs, circ, i, currentComponent, components, section, remainingPoints;

  fundcircs := ShallowCopy( FundamentalCircuitsWithBasis( matroid )[1] );
  remainingPoints := SizeOfGroundSet( matroid ) - Size( Coloops( matroid ) );
  components := [];

# Determine partition of ground set by fundamental circuits:

  while remainingPoints > 0 do

   circ := Remove( fundcircs );
   i := 1;
   currentComponent := circ;

   while i <= Size( fundcircs ) do
    section := Intersection2( fundcircs[i], circ );

    if not IsEmpty( section ) then

     currentComponent := Union2( currentComponent, Remove(fundcircs,i) );
     if Size( currentComponent ) = remainingPoints then
      break;
     fi;

    else

     i := i+1;

    fi;

   od;

   remainingPoints := remainingPoints - Size( currentComponent );
   AddSet( components, currentComponent );

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

 function( matroid )
  local k, isIndep, n;

  n := SizeOfGroundSet( matroid );
  k := RankOfMatroid( matroid );

  if k = 0 or k = n then return true; fi;

  isIndep := IndependenceFunction( matroid );

  return ForAll( Combinations( [ 1 .. n ], k ), X -> isIndep(X) );
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


####################################
##
## Methods
##
####################################

############
## SomeBasis

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

 function( matroid )
  local basis;

  basis := Difference( GroundSet( matroid ), NormalFormOfVectorMatroid( matroid )[2] );
  AddSet( KnownBases( matroid ), basis );

  return basis;
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


##########
## MinorNL

##
InstallOtherMethod( MinorNL,
		"for empty arguments",
		[ IsMatroid, IsList and IsEmpty, IsList and IsEmpty ],
		20,

 function( matroid, del, contr )
  return matroid;
 end

);

##
InstallMethod( MinorNL,
		"for matroids with bases",
		[ IsAbstractMatroidRep and HasBases, IsList, IsList ],

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

  if IsEmpty( actRows ) then
   minorMat := HomalgMatrix( [], 0, Size( actCols ), HomalgRing( mat ) );
  else
   minorMat := CertainColumns( CertainRows( HomalgMatrix( minorMat, HomalgRing( mat ) ), actRows ), actCols );
  fi;

  minor := Objectify( TheTypeMinorOfVectorMatroid, rec( generatingMatrix := Immutable( minorMat ) ) );
  SetParentAttr( minor, [
	matroid,
	Difference( GroundSet( matroid ), Union2( sdel, scontr ) )
  ] );

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
InstallMethod( DirectSumOfMatroids,
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

  _alcove_MatroidStandardImplications( sum );

  return sum;

 end

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

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetBases( matroid, baselist );
  SetSizeOfGroundSet( matroid, deg );

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

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetBases( matroid, baselist );
  SetSizeOfGroundSet( matroid, deg );

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

##
InstallMethod( MatroidByIndependenceFunction,
		"given size of ground set and boolean function deciding independence of subsets",
		[ IsInt, IsFunction ],

 function( size, isIndep )

 end

);

##
InstallMethod( MatroidByIndependenceFunctionNCL,
		"given size of ground set and boolean function deciding independence of subsets, no checks or logical implications",
		[ IsInt, IsFunction ],

 function( size, isIndep )
  local matroid;

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetSizeOfGroundSet( matroid, size );
  SetIndependenceFunction( matroid, isIndep );

  return matroid;
 end

);

##
InstallMethod( MatroidByIndependenceFunction,
		"given ground set and boolean function deciding independence of subsets",
		[ IsList, IsFunction ],

 function( groundSet, isIndep )
  if groundSet = [ 1 .. Size( groundSet ) ] then
   return MatroidByIndependenceFunction( Size( groundSet ), isIndep );
  else
   return MatroidByIndependenceFunction( Size( groundSet ), function(i) return isIndep( groundSet[i] ); end );
  fi;
 end

);

##
InstallMethod( MatroidByIndependenceFunctionNCL,
		"given ground set and boolean function deciding independence of subsets, no checks or logical implications",
		[ IsList, IsFunction ],

 function( groundSet, isIndep )
  if groundSet = [ 1 .. Size( groundSet ) ] then
   return MatroidByIndependenceFunctionNCL( Size( groundSet ), isIndep );
  else
   return MatroidByIndependenceFunctionNCL( Size( groundSet ), function(i) return isIndep( groundSet[i] ); end );
  fi;
 end

);

##
InstallMethod( MatroidByCircuits,
		"given size of ground set and list of circuits",
		[ IsInt, IsList ],

 function( size, circs )
  local matroid;

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

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetSizeOfGroundSet( matroid, size );
  SetCircuits( matroid, circs );

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

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetSizeOfGroundSet( matroid, size );
  SetCircuits( matroid, circs );

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

 end

);

##
InstallMethod( MatroidByRankFunctionNCL,
		"given size of ground set and integer valued function, no checks or logical implications",
		[ IsInt, IsFunction ],

 function( size, rankFunc )
  local matroid;

  matroid := Objectify( TheTypeAbstractMatroid, rec() );
  SetSizeOfGroundSet( matroid, size );
  SetRankFunction( matroid, rankFunc );

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

  matroid := MatroidByRankFunctionNCL( n, function( X ) if Size(X) < k then return Size(X); else return k; fi; end );
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

  matroid := MatroidByRankFunctionNCL( n, function( X ) if Size(X) < k then return Size(X); else return k; fi; end );
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
 
   ## Print( " matroid on ", SizeOfGroundSet( matroid ), " elements>" );
   Print( " matroid>" );

  fi;

 end

);

##
InstallMethod( Display,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local mat;

  if IsVectorMatroidRep( matroid ) then

   if SizeOfGroundSet( matroid ) = 0 then
    Print( "The vector matroid of the empty matrix." );
   else
    mat := MatrixOfVectorMatroid( matroid );

    Print( "The vector matroid of this matrix over " );
    View( HomalgRing(mat) );
    Print( ":\n" );
    Display( mat );
   fi;

  else

   Print( "The abstract matroid on the ground set\n" );
   Display( GroundSet( matroid ) );
   Print( "with bases\n" );
   Display( Bases( matroid ) );

  fi;

 end

);
