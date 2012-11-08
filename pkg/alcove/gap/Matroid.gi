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

BindGlobal( "TheTypeVectorMatroid",
	NewType( TheFamilyOfMatroids,
		IsVectorMatroidRep )
);

BindGlobal( "TheTypeGraphicMatroid",
	NewType( TheFamilyOfMatroids,
		IsGraphicMatroidRep )
);


####################################
##
## Attributes
##
####################################


##################
## SizeOfGroundSet

InstallMethod( SizeOfGroundSet,
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  if IsBound( matroid!.groundSet ) and not IsEmpty( matroid!.groundSet ) then
   return Size( matroid!.groundSet );
  else
   Error( "this matroid does not seem to have a ground set, this shouldn't happen" );
  fi;
 end

);

InstallMethod( SizeOfGroundSet,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  if IsBound( matroid!.generatingMatrix ) then
   return DimensionsMat( matroid!.generatingMatrix )[2];
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
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
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then
   return Size( matroid!.bases[1] );
  else
   Error( "this matroid does not seem to have any bases, this shouldn't happen" );
  fi;
 end

);

InstallMethod( RankOfMatroid,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  if IsBound( matroid!.generatingMatrix ) then
   return Rank( matroid!.generatingMatrix );
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
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
		"for matroids",
		[ IsMatroid ],

 function( matroid )

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
  if IsBound( matroid!.generatingMatrix ) then
   return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], Rank( matroid ) ),
		b -> Rank( ExtractSubMatrix( matroid!.generatingMatrix, [1..DimensionsMat(matroid!.generatingMatrix)], b ) ) = Rank( matroid ) );
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
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

InstallMethod( Circuits,				# CPT. PLACEHOLDER BECKONS
		"for abstract matroids",
		[ IsAbstractMatroidRep ],

 function( matroid )
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then

  else
   Error( "this matroid does not seem to have any bases, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Circuits,				# CPT. PLACEHOLDER BECKONS
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  if IsBound( matroid!.generatingMatrix ) then

  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Circuits,				# CPT. PLACEHOLDER BECKONS
		"for graphic matroids",
		[ IsGraphicMatroidRep ],

 function( matroid )
  if IsBound( matroid!.incidenceMatrix ) then

  else
   Error( "this graphic matroid apparently lost its incidence matrix, this shouldn't happen" );
  fi;
 end

);


####################################
##
## Properties
##
####################################

###################
## IsUniformMatroid

InstallMethod( IsUniformMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )
  return Size( Bases( matroid ) ) = Binomial( SizeOfGroundSet( matroid ), Rank( matroid ) );
 end

);

InstallMethod( IsUniform, "for matroids", [ IsMatroid ], IsUniformMatroid );


##################
## IsSimpleMatroid

InstallMethod( IsSimpleMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);

InstallMethod( IsSimple, "for matroids", [ IsMatroid ], IsSimpleMatroid );


###################
## IsGraphicMatroid

InstallMethod( IsGraphicMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


###################
## IsRegularMatroid

InstallMethod( IsRegularMatroid,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);

InstallMethod( IsRegular, "for matroids", [ IsMatroid ], IsRegularMatroid );


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
   return ShallowCopy( matroid!.groundSet );
  else
   Error( "this matroid does not seem to have a ground set, this shouldn't happen" );
  fi;
 end

);

InstallMethod( GroundSet,
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  local template;
  if IsBound( matroid!.generatingMatrix ) then

#   return List( [ 1 .. SizeOfGroundSet(matroid) ],
#	col -> List( [ 1 .. DimensionsMat( matroid!.generatingMatrix )[1] ], row -> matroid!.generatingMatrix[row][col]  ) );

   template := CompatibleVector(  matroid!.generatingMatrix );
   return List( [ 1 .. SizeOfGroundSet(matroid) ],
	col -> Unfold( ExtractSubMatrix( matroid!.generatingMatrix, [1..DimensionsMat(matroid!.generatingMatrix)[1]], [col] ), template ) );

  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
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

  gset := [ 1 .. deg ];

  if ForAny( indep, i -> not IsSubset( gset, i ) ) then
   Error( "elements of <indep> must be subsets of [1..<deg>]" );
  fi;

  sizelist := List( indep, i -> Size( Set( i ) ) );
  rk := Maximum( sizelist );

# Extract bases from indep list:
  baselist := List( Filtered( [ 1 .. Size( indep ) ], i -> sizelist[i] = rk ), i -> Set( indep[i] ) );

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
		"by ground set and list of bases or independent sets",
		[ IsList, IsList ],

 function( groundset, indep )

  if IsEmpty( indep ) then Error( "the list of independent sets must be non-empty" ); fi;

  if ForAny( indep, i -> not IsSubset( groundset, i ) ) then
   Error( "elements of <indep> must be subsets of <groundset>" );
  fi;

  sizelist := List( indep, i -> Size( Set( i ) ) );
  rk := Maximum( sizelist );

# Extract bases from indep list:
  baselist := List( Filtered( [ 1 .. Size( indep ) ], i -> sizelist[i] = rk ), i -> Set( indep[i] ) );

# Check base exchange axiom:
  if ForAny( baselist, b1 -> ForAny( baselist, b2 ->
	ForAny( Difference(b1,b2), e -> ForAll( Difference(b2,b1), f ->
		not Union( Difference( b1, [e] ), [f] ) in baselist
	) )
  ) ) then Error( "bases must satisfy the exchange axiom" ); fi;

  matroid := Objectify( TheTypeAbstractMatroid, rec( groundSet := groundset, bases := baselist ) );
  SetRankOfMatroid( matroid, rk );

  return matroid;

 end

);


##
InstallMethod( Matroid,
		"by list of bases or independent sets",
		[ IsList ],

 function( indep )
  local gset;

  if IsEmpty( indep ) then Error( "the list of independent sets must be non-empty" ); fi;
  gset := Union( indep );

  sizelist := List( indep, i -> Size( Set( i ) ) );
  rk := Maximum( sizelist );

# Extract bases from indep list:
  baselist := List( Filtered( [ 1 .. Size( indep ) ], i -> sizelist[i] = rk ), i -> Set( indep[i] ) );

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
		"by matrix",
		[ IsMatrix ],

 function( mat )
  local matobj;

  matobj := ( MakeMatrix( mat ) );		## guess the base field and construct matrix object
  return Objectify( TheTypeVectorMatroid, rec( generatingMatrix := matobj ) );

 end

);


##
InstallMethod( Matroid,
		"by matrix object",
		[ IsMatrixObj ],

 function( matobj )

  return Objectify( TheTypeVectorMatroid, rec( generatingMatrix := matobj ) );
  
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
 
   if HasIsUniformMatroid( matroid ) and IsUniformMatroid( matroid ) then
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

  if IsVectorMatroidRep( matroid ) then

   Print( "The vector matroid of this matrix:\n" );
   Display( matroid!.generatingMatrix );

  elif IsGraphicMatroidRep( matroid ) then

   Print( "The matroid of the (multi-)graph with this incidence matrix:\n" );
   Display( matroid!.incidenceMatrix );

  else

   Print( "The abstract matroid on the ground set\n" );
   Display( GroundSet( matroid ) );
   Print( "with bases\n" );
   Display( Bases( matroid ) );

  fi;

 end

);
