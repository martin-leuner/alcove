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
	[ "bases", "groundSet" ]
);

DeclareRepresentation( "IsVectorMatroidRep",
	IsMatroid and IsAttributeStoringRep,
	[ "generatingMatrix" ]
);

DeclareRepresentation( "IsGraphicMatroidRep",
	IsMatroid and IsAttributeStoringRep,
	[ "incidenceMatrix" ]
);


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

BindGlobal( "TheTypeGraphicMatroid",
	NewType( TheFamilyOfMatroids,
		IsGraphicMatroidRep )
);

BindGlobal( "TheTypeMinorOfGraphicMatroid",
	NewType( TheFamilyOfMatroids,
		IsGraphicMatroidRep and IsMinorOfMatroid )
);


####################################
##
## Attributes
##
####################################


##############
## DualMatroid

InstallMethod( DualMatroid,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  local dualbases, dual;

  dualbases := Set( List( Bases( matroid ), b -> Difference( GroundSet( matroid ), b ) ) );

  dual := Matroid( GroundSet( matroid ), dualbases );
  SetDualMatroid( dual, matroid );

  return dual;

 end

);

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


#CM#################
#CM## ComputedMinors
#CM
#CMInstallMethod( ComputedMinors,
#CM		"for matroids",
#CM		[ IsMatroid ],
#CM
#CM function( matroid )
#CM  return rec( keys := [], minors := [] );
#CM end
#CM
#CM);
#CM
#CM
##################
## SizeOfGroundSet

InstallMethod( SizeOfGroundSet,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  return Size( GroundSet( matroid ) );
 end

);

InstallMethod( SizeOfGroundSet,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
   return NrColumns( MatrixOfVectorMatroid( matroid ) );
 end

);

InstallMethod( SizeOfGroundSet,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )
  local vertnum;
  if IsBound( matroid!.incidenceMatrix ) then

   vertnum := DimensionsMat( matroid!.incidenceMatrix )[1];
   return Sum( List( [ 1 .. vertnum ], i ->
		Sum( List( [ i .. vertnum ], j -> matroid!.incidenceMatrix[i][j] ) )
	) );

  else
   Error( "this graphic matroid apparently lost its incidence matrix, this shouldn't happen" );
  fi;
 end

);


#######
## Rank

InstallMethod( RankOfMatroid,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  return Size( Bases(matroid)[1] );
 end

);

InstallMethod( RankOfMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return RowRankOfMatrix( MatrixOfVectorMatroid(matroid) );
 end

);

InstallMethod( RankOfMatroid,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )

 end

);

InstallMethod( Rank,
		"alias for Rank for matroids",
		[ IsMatroid ],

 RankOfMatroid

);


################
## Rank function

InstallMethod( RankFunction,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  return function( X )
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

InstallMethod( RankFunction,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return function( X ) return RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid( matroid ), X ) ); end;
 end

);

InstallMethod( RankFunction,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )

 end

);


##################
## ClosureFunction

InstallMethod( ClosureFunction,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return
	function( X )
	 local loopsOfMinor, x, i;

	 loopsOfMinor := ShallowCopy( Loops( Contraction( matroid, X ) ) );
	 for x in X do
          for i in [1..Size(loopsOfMinor)] do
           if x <= loopsOfMinor[i] then loopsOfMinor[i] := loopsOfMinor[i]+1; fi;
	  od;
	 od;

	 return Union2( X, loopsOfMinor );
	end;
 end

);


########
## Bases

InstallMethod( Bases,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then
   return matroid!.bases;
  else
   Error( "this matroid does not seem to have any bases, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Bases,				# THIS IS AN EXTREMELY NAIVE APPROACH
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], Rank( matroid ) ),
		b -> RowRankOfMatrix( CertainColumns( MatrixOfVectorMatroid(matroid), b ) ) = Rank( matroid ) );
 end

);

InstallMethod( Bases,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )

 end

);


###########
## Circuits

InstallMethod( Circuits,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local loopsColoops, loopColoopFree, delCircs, conCircs, t, h, l, circs;

# Check all trivial cases:

  if SizeOfGroundSet( matroid ) = 0 then return []; fi;
  if SizeOfGroundSet( matroid ) = 1 then return List( Loops( matroid ), i -> [i] ); fi;

  if Rank( matroid ) = 0 then return List( GroundSet( matroid ), i -> [i] ); fi;

  loopsColoops := Union2( Loops( matroid ), Coloops( matroid ) );

  if Size( loopsColoops ) = SizeOfGroundSet( matroid ) then return List( Loops( matroid ), i -> [i] ); fi;

# Delete loops and coloops and start recursion:

  loopColoopFree := Deletion( matroid, loopsColoops );
  t := SizeOfGroundSet( loopColoopFree );

  delCircs := Circuits( Deletion( loopColoopFree, [t] ) );
  conCircs := Circuits( Contraction( loopColoopFree, [t] ) );

# Combine results:

  circs := Union2( 	List( delCircs, h -> ShallowCopy(h) ),		# this line ensures that the lists in circs are mutable
			List( Difference( conCircs, delCircs ), h -> Union2( h, [t] ) ) );

# Shift labels according to deletion:

  for l in loopsColoops do
   for h in circs do
    for t in [ 1 .. Size( h ) ] do
     if h[t] >= l then h[t] := h[t] + 1; fi;
    od;
   od;
  od;

  return Union2( circs, List( Loops( matroid ), l -> [l] ) );
 end

);


#############
## Cocircuits

InstallMethod( Cocircuits,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Circuits( DualMatroid( matroid ) );
 end

);


##############
## Hyperplanes

InstallMethod( Hyperplanes,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Set( List( Cocircuits( matroid ), c -> Difference( GroundSet( matroid ), c ) ) );
 end

);


##################
## TuttePolynomial

InstallMethod( TuttePolynomial,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  local loopNum, coloopNum, loopsColoops, x, y, p, min;

  x := Indeterminate( Integers, 1 );
  y := Indeterminate( Integers, 2 );
  if not HasIndeterminateName( FamilyObj(x), 1 ) and not HasIndeterminateName( FamilyObj(x), 2 ) then
   SetIndeterminateName( FamilyObj(x), 1, "x" );
   SetIndeterminateName( FamilyObj(x), 2, "y" );
  fi;

# Check whether Tutte polynomial of dual matroid is already known:
  if HasDualMatroid( matroid ) and HasTuttePolynomial( DualMatroid( matroid ) ) then
   return Value( TuttePolynomial( DualMatroid( matroid ) ), [ x, y ], [ y, x ] );
  fi;

  loopNum := Size( Loops( matroid ) );
  coloopNum := Size( Coloops( matroid ) );

  p := x^coloopNum * y^loopNum;

# Termination case:

  if loopNum + coloopNum = SizeOfGroundSet( matroid ) then
   return p;
  fi;

# Recursion:

  loopsColoops := Union2( Loops( matroid ), Coloops( matroid ) );

  min := Deletion( matroid, loopsColoops );

  return p * ( TuttePolynomial( Deletion( min, [1] ) ) + TuttePolynomial( Contraction( min, [1] ) ) );
 end

);


###########################
## RankGeneratingPolynomial

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

InstallMethod( Loops,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  return Coloops( DualMatroid( matroid ) );
 end

);

InstallMethod( Loops,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return ZeroColumns( MatrixOfVectorMatroid( matroid ) );
 end

);

InstallMethod( Loops,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )

 end

);


########
## Coloops

InstallMethod( Coloops,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

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

InstallMethod( Coloops,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return Loops( DualMatroid( matroid ) );
 end

);

InstallMethod( Coloops,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )

 end

);


####################################
##
## Properties
##
####################################

############
## IsUniform

InstallMethod( IsUniform,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Size( Bases( matroid ) ) = Binomial( SizeOfGroundSet( matroid ), Rank( matroid ) );
 end

);


##################
## IsSimpleMatroid

InstallMethod( IsSimpleMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return IsEmpty( Loops( matroid ) )
	and ForAll( GroundSet( matroid ), i -> Size( ClosureFunction(matroid)([i]) ) = 1 );
 end

);

InstallMethod( IsSimple, "for matroids", [ IsMatroid ], IsSimpleMatroid );


############
## IsGraphic

InstallMethod( IsGraphic,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


############
## IsRegular

InstallMethod( IsRegular,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


####################################
##
## Methods
##
####################################

############
## GroundSet

InstallMethod( GroundSet,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  if IsBound( matroid!.groundSet ) then
   return matroid!.groundSet;
  else
   Error( "this matroid does not seem to have a ground set, this shouldn't happen" );
  fi;
 end

);

InstallMethod( GroundSet,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  return [ 1 .. SizeOfGroundSet(matroid) ];
 end

);

InstallMethod( GroundSet,
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )
  local i, j, edges, vertnum, multiplicity;

  if IsBound( matroid!.incidenceMatrix ) then

   vertnum := DimensionsMat( matroid!.incidenceMatrix )[1];
   edges := [];
   for i in [ 1 .. vertnum ] do
    for j in [ i .. vertnum ] do
     multiplicity := matroid!.incidenceMatrix[i][j];
     if multiplicity <> 0 then
      Add( edges, [Set([i,j]),multiplicity] );
     fi;
    od;
   od;
   return edges;

  else
   Error( "this graphic matroid apparently lost its incidence matrix, this shouldn't happen" );
  fi;

 end

);


########################
## MatrixOfVectorMatroid

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


########
## Minor

##
InstallOtherMethod( Minor,
		"for empty arguments",
		[ IsMatroid, IsList and IsEmpty, IsList and IsEmpty ],
		20,

 function( matroid, del, contr )
  return matroid;
 end

);

##
InstallMethod( Minor,
		"for abstract matroids",
		[ IsAbstractMatroidRep, IsList, IsList ],

 function( matroid, del, contr )
  local minorBases, t, sdel, scontr, minor, loopsColoops;

  sdel := Set( del );
  scontr := Set( contr );
  if not IsEmpty( Intersection2( sdel, scontr ) ) then Error( "<del> and <contr> must not meet" ); fi;

# If loops or coloops will be deleted or contracted, delete rather than contract:

  loopsColoops := Intersection2( Union2( Loops( matroid ), Coloops( matroid ) ), scontr );
  scontr := Difference( scontr, loopsColoops );
  sdel := Union2( sdel, loopsColoops );
#CM
#CM# Check whether we computed this minor already:
#CM
#CM  t := Position( ComputedMinors( matroid ).keys, [sdel,scontr] );
#CM  if not IsBool(t) then return ComputedMinors( matroid ).minors[t]; fi;
#CM  t := Position( ComputedMinors( DualMatroid( matroid ) ).keys, [scontr,sdel] );
#CM  if not IsBool(t) then return DualMatroid( ComputedMinors( DualMatroid( matroid ) ).minors[t] ); fi;
#CM
#CM# Otherwise compute it:

  minorBases := ShallowCopy( Bases( Matroid ) );

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

  minor := Objectify( TheTypeMinorOfAbstractMatroid,
	rec( groundSet := Immutable( Difference( Difference( GroundSet( matroid ), sdel ), scontr ) ), bases := Immutable( minorBases ) ) );
  SetParentAttr( minor, matroid );

#CM  Add( ComputedMinors( matroid ).keys, [sdel,scontr] );
#CM  Add( ComputedMinors( matroid ).minors, minor );
#CM
  return minor;
 end

);

##
InstallMethod( Minor,
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

#CM# Check whether we computed this minor already:
#CM
#CM  t := Position( ComputedMinors( matroid ).keys, [sdel,scontr] );
#CM  if not IsBool(t) then return ComputedMinors( matroid ).minors[t]; fi;
#CM  t := Position( ComputedMinors( DualMatroid( matroid ) ).keys, [scontr,sdel] );
#CM  if not IsBool(t) then return DualMatroid( ComputedMinors( DualMatroid( matroid ) ).minors[t] ); fi;
#CM
#CM# Otherwise compute it.
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
  SetParentAttr( minor, matroid );

#CM  Add( ComputedMinors( matroid ).keys, [sdel,scontr] );
#CM  Add( ComputedMinors( matroid ).minors, minor );
#CM
  return minor;
 end

);

##
InstallMethod( Minor,
		"for graphic matroids",
		[ IsGraphicMatroidRep, IsList, IsList ],

 function( matroid, del, contr )

 end

);


###########
## Deletion

InstallMethod( Deletion,
		"for matroids",
		[ IsMatroid, IsList ],

 function( matroid, del )
  return Minor( matroid, del, [] );
 end

);


##############
## Contraction

InstallMethod( Contraction,
		"for matroids",
		[ IsMatroid, IsList ],

 function( matroid, contr )
  return Minor( matroid, [], contr );
 end

);


##########
## IsMinor

InstallMethod( IsMinor,
		"for matroids",
		[ IsMatroid, IsMinorOfMatroid ],

 function( matroid, minor )
  local parent;
  parent := ParentAttr( minor );
  if IsMinorOfMatroid( parent ) then
   return IsMinor( matroid, parent );
  else
   return matroid = parent;
  fi;
 end

);


####################################
##
## Constructors
##
####################################


##
InstallMethod( Matroid,
		"copy constructor",
		[ IsMatroid ],

 IdFunc

);


##
InstallMethod( Matroid,
		"by size of ground set and list of bases or independent sets",
		[ IsInt, IsList ],

 function( deg, indep  )
  local gset, baselist, rk, sizelist, matroid;

  if IsEmpty( indep ) then Error( "the list of independent sets must be non-empty" ); fi;

  gset := Immutable([ 1 .. deg ]);

  if ForAny( indep, i -> not IsSubset( gset, i ) ) then
   Error( "elements of <indep> must be subsets of [1..<deg>]" );
  fi;

  sizelist := List( indep, i -> Size( Set( i ) ) );
  rk := Maximum( sizelist );

# Extract bases from indep list:
  baselist := Immutable( List( Filtered( [ 1 .. Size( indep ) ], i -> sizelist[i] = rk ), i -> Set( indep[i] ) ) );

# Check base exchange axiom:
  if ForAny( baselist, b1 -> ForAny( baselist, b2 ->
	ForAny( Difference(b1,b2), e -> ForAll( Difference(b2,b1), f ->
		not Union( Difference( b1, [e] ), [f] ) in baselist
	) )
  ) ) then Error( "bases must satisfy the exchange axiom" ); fi;

  matroid := Objectify( TheTypeAbstractMatroid, rec( groundSet := gset, bases := baselist ) );
  SetRankOfMatroid( matroid, rk );

  return matroid;

 end

);


##
InstallMethod( Matroid,
		"by size of ground set and list of bases",
		[ IsInt, IsList ],

 function( deg, baselist  )
  return Objectify( TheTypeAbstractMatroid, rec( groundSet := Immutable([1..deg]), bases := Immutable(baselist) ) );
 end

);


##
InstallMethod( Matroid,
		"by ground set and list of bases or independent sets",
		[ IsList, IsList ],

 function( groundset, indep )
  local matroid, sizelist, rk, baselist;

  if IsEmpty( indep ) then Error( "the list of independent sets must be non-empty" ); fi;

  if ForAny( indep, i -> not IsSubset( groundset, i ) ) then
   Error( "elements of <indep> must be subsets of <groundset>" );
  fi;

  sizelist := List( indep, i -> Size( Set( i ) ) );
  rk := Maximum( sizelist );

# Extract bases from indep list:
  baselist := Immutable( List( Filtered( [ 1 .. Size( indep ) ], i -> sizelist[i] = rk ), i -> Set( indep[i] ) ) );

# Check base exchange axiom:
  if ForAny( baselist, b1 -> ForAny( baselist, b2 ->
	ForAny( Difference(b1,b2), e -> ForAll( Difference(b2,b1), f ->
		not Union( Difference( b1, [e] ), [f] ) in baselist
	) )
  ) ) then Error( "bases must satisfy the exchange axiom" ); fi;

  matroid := Objectify( TheTypeAbstractMatroid, rec( groundSet := Immutable(groundset), bases := baselist ) );
  SetRankOfMatroid( matroid, rk );

  return matroid;

 end

);


##
InstallMethod( MatroidNC,
		"by ground set and list of bases, no checks",
		[ IsList, IsList ],

 function( groundset, baselist )
  return Objectify( TheTypeAbstractMatroid, rec( groundSet := Immutable(groundset), bases := Immutable(baselist) ) );
 end

);



##
InstallMethod( Matroid,
		"by matrix",
		[ IsMatrix ],
		10,

 function( mat )
  local matobj;

  matobj := Immutable( MakeMatrix( mat ) );		## guess the base field and construct matrix object

  return Objectify( TheTypeVectorMatroid, rec( generatingMatrix := matobj ) );
 end

);


##
InstallMethod( Matroid,
		"by empty matrix",
		[ IsGeneralizedRowVector and IsNearAdditiveElementWithInverse and IsAdditiveElement ],

 function( mat )
  if not IsEmpty( mat[1] ) then Error( "constructor for empty vector matroids called on non-empty matrix" ); fi;

  return ObjectifyWithAttributes( rec( generatingMatrix := Immutable(mat) ),
			TheTypeVectorMatroid,
			SizeOfGroundSet, 0,
			RankOfMatroid, 0
	);
 end

);


##
InstallMethod( Matroid,
		"by matrix object",
		[ IsMatrixObj ],
		20,

 function( matobj )
  if DimensionsMat( matobj )[2] = 0 then return Matroid( [[]] ); fi;		# call constructor for empty matrix
  return Objectify( TheTypeVectorMatroid, rec( generatingMatrix := Immutable(matobj) ) );
 end

);


##
InstallMethod( Matroid,
		"by homalg matrix",
		[ IsHomalgMatrix ],
		30,

 function( matobj )
  return Objectify( TheTypeVectorMatroid, rec( generatingMatrix := Immutable(matobj) ) );
 end

);


##
InstallMethod( Matroid,
		"given ground set and boolean function deciding independence of subsets",
		[ IsList, IsFunction ],

 function( groundset, testindep )

 end

);


##
InstallMethod( MatroidByCircuits,
		"given ground set and list of circuits",
		[ IsList, IsList ],

 function( groundset, circs )

 end

);


##
InstallMethod( MatroidByRankFunction,
		"given ground set and integer valued function",
		[ IsList, IsFunction ],

 function( groundset, rank )

 end

);


##
InstallMethod( MatroidOfGraph,
		"given an incidence matrix",
		[ IsMatrix ],

 function( incidencemat )

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
   elif IsGraphicMatroidRep( matroid ) then
    Print( " graphic" );
   fi;
 
   ## Print( " matroid over a ", SizeOfGroundSet( matroid ), " element ground set>" );
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

    Print( "The vector matroid of this matrix" );
    Print( " over ", HomalgRing(mat) );
    Print( ":\n" );
    Display( mat );
   fi;

  elif IsGraphicMatroidRep( matroid ) then

   Print( "The matroid of the (multi-)graph with this incidence matrix:\n" );

  else

   Print( "The abstract matroid on the ground set\n" );
   Display( GroundSet( matroid ) );
   Print( "with bases\n" );
   Display( Bases( matroid ) );

  fi;

 end

);
