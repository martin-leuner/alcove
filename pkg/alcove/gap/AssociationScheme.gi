#############################################################################
##
##  AssocationScheme.gi            alcove package               Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  Association scheme methods for alcove.
##
#############################################################################

####################################
##
## Representations
##
####################################

DeclareRepresentation( "IsAssociationSchemeRep",
		IsAssociationScheme and IsAttributeStoringRep,
		[ "groundSet", "relations" ]
	);


####################################
##
## Types and Families
##
####################################


BindGlobal( "TheFamilyOfMatroids",
	NewFamily( "TheFamilyOfMatroids" , IsMatroid ) );

BindGlobal( "TheTypeAbstractMatroid",
	NewType( TheFamilyOfMatroids,
		IsAbstractMatroidRep ) );

BindGlobal( "TheTypeVectorMatroid",
	NewType( TheFamilyOfMatroids,
		IsVectorMatroidRep ) );


####################################
##
## Attributes
##
####################################


##################
## SizeOfGroundSet

InstallMethod( SizeOfGroundSet,
		"for abstract matroids",
		[ IsAbstractMatroid ],

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


#######
## Rank

InstallMethod( Rank,
		"for abstract matroids",
		[ IsAbstractMatroid ],

 function( matroid )
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then
   return Size( matroid!.bases[1] );
  else
   Error( "this matroid does not seem to have a basis, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Rank,
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


################
## Rank function

InstallMethod( RankFunction,
		"for matroids",
		[ IsMatroid ],

 function

 end

);


########
## Bases

InstallMethod( Bases,
		"for abstract matroids",
		[ IsAbstractMatroid ],

 function( matroid )
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then
   return matroid!.bases;
  else
   Error( "this matroid does not seem to have a basis, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Bases,				# THIS IS AN EXTREMELY NAIVE APPROACH
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  if IsBound( matroid!.generatingMatrix ) then
   return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], Rank( matroid ) ), b -> Rank(
				List( [1..DimensionsMat(matroid!.generatingMatrix)[1]], i -> List( b, j -> matroid!.generatingMatrix[i][j] ) )
			 ) = Rank( matroid ) );
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
 end

);


###########
## Circuits

InstallMethod( Circuits,				# CPT. PLACEHOLDER BECKONS
		"for abstract matroids",
		[ IsAbstractMatroid ],

 function( matroid )
  if IsBound( matroid!.bases ) and not IsEmpty( matroid!.bases ) then
   return matroid!.bases;
  else
   Error( "this matroid does not seem to have a basis, this shouldn't happen" );
  fi;
 end

);

InstallMethod( Circuits,				# CPT. PLACEHOLDER BECKONS
		"for vector matroids",
		[ IsVectorMatroidRep ],

 function( matroid )
  if IsBound( matroid!.generatingMatrix ) then
   return Filtered( Combinations( [ 1 .. SizeOfGroundSet( matroid ) ], Rank( matroid ) ), b -> Rank(
				List( [1..DimensionsMat(matroid!.generatingMatrix)[1]], i -> List( b, j -> matroid!.generatingMatrix[i][j] ) )
			 ) = Rank( matroid ) );
  else
   Error( "this vector matroid apparently lost its matrix, this shouldn't happen" );
  fi;
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


###########
## IsSimple

InstallMethod( IsSimple,
		"for matroids",
		[ IsMatroid ],

 function( matroid )

 end

);


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
		"given size of ground set and list of bases or independent sets",
		[ IsInt, IsList ],

 function(  )

 end

);


##
InstallMethod( Matroid,
		"given list of independent sets",
		[ IsList ],

 function(  )

 end

);


##
InstallMethod( Matroid,
		"given ground set and list of bases or independent sets",
		[ IsList, IsList ],

 function(  )

 end

);


##
InstallMethod( Matroid,
		"for a vector matroid",
		[ IsMatrix ],

 function(  )

 end

);


##
InstallMethod( MatroidByCircuits,
		"given ground set and list of circuits",
		[ IsList, IsList ],

 function(  )

 end

);


##
InstallMethod( MatroidByRankFunction,
		"given ground set and integer valued function",
		[ IsInt, IsFunction ],

 function(  )

 end

);


##
InstallMethod( MatroidOfGraph,
		"given an incidence matrix",
		[ IsMatrix ],

 function(  )

 end

);


####################################
##
## Display Methods
##
####################################

##
InstallMethod( ViewObj,
		"for homalg fans",
		[ IsMatroid ],

  function( fan )
    local str;

    Print( "<A" );

    if HasIsComplete( fan ) then

        if IsComplete( fan ) then

		Print( " complete" );

        fi;

    fi;

    if HasIsPointed( fan ) then

        if IsPointed( fan ) then

		Print( " pointed" );

        fi;

    fi;

    if HasIsSmooth( fan ) then

        if IsSmooth( fan ) then

		Print( " smooth" );

        fi;

    fi;

    Print( " fan in |R^" );

    Print( String( AmbientSpaceDimension( fan ) ) );

    if HasRays( fan ) then

        Print( " with ", String( Length( Rays( fan ) ) )," rays" );

    fi;

    Print( ">" );

end );

##
InstallMethod( Display,
		"for homalg polytopes",
		[ IsMatroid ],

  function( fan )
    local str;

    Print( "A" );

    if HasIsComplete( fan ) then

        if IsComplete( fan ) then

		Print( " complete" );

        fi;

    fi;

    Print( " fan in |R^" );

    Print( String( AmbientSpaceDimension( fan ) ) );

    if HasRays( fan ) then

        Print( " with ", String( Length( Rays( fan ) ) )," rays" );

    fi;

    Print( ".\n" );

end );
