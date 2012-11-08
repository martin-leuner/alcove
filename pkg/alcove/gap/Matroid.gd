#############################################################################
##
##  Matroid.gd                  alcove package                  Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  Matroid declarations for alcove.
##
#############################################################################

DeclareCategory( "IsMatroid",
		IsObject );

DeclareCategory( "IsMinor",
		IsMatroid );


####################################
##
## Attributes
##
####################################

DeclareAttribute( "SizeOfGroundSet",
		IsMatroid );

DeclareAttribute( "RankOfMatroid",
		IsMatroid );

#DeclareOperation( "Rank",
		#[ IsMatroid ] );

#DeclareOperation( "HasRank",
#		[ IsMatroid ] );
#
#DeclareOperation( "SetRank",
#		[ IsMatroid, IsInt ] );

DeclareAttribute( "RankFunction",
		IsMatroid );

DeclareAttribute( "Bases",
		IsMatroid );

DeclareAttribute( "Circuits",
		IsMatroid );


####################################
##
## Properties
##
####################################

DeclareProperty( "IsUniformMatroid",
		IsMatroid );

DeclareProperty( "IsSimpleMatroid",
		IsMatroid );

DeclareProperty( "IsGraphicMatroid",
		IsMatroid );

DeclareSynonym( "IsGraphic", IsGraphicMatroid );

DeclareProperty( "IsRegularMatroid",
		IsMatroid );


####################################
##
## Methods
##
####################################

DeclareOperation( "GroundSet",
		[ IsMatroid ] );


####################################
##
## Constructors
##
####################################

DeclareOperation( "Matroid",
		[ IsMatroid ] );

DeclareOperation( "Matroid",
		[ IsInt, IsList ] );

DeclareOperation( "Matroid",
		[ IsList, IsList ] );

DeclareOperation( "Matroid",
		[ IsList ] );	# this implies a declaration for IsMatrix

DeclareOperation( "Matroid",
		[ IsMatrixObj ] );

DeclareOperation( "Matroid",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByCircuits",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByRankFunction",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidOfGraph",
		[ IsMatrix ] );
