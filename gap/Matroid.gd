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

DeclareCategory( "IsMinorOfMatroid",
		IsMatroid );


####################################
##
## Attributes
##
####################################

DeclareAttribute( "DualMatroid",
		IsMatroid );

DeclareAttribute( "ParentAttr",
		IsMinorOfMatroid );

DeclareAttribute( "SimplifiedMatroid",
		IsMatroid );

DeclareAttribute( "NormalForm",
		IsMatroid );

DeclareAttribute( "SizeOfGroundSet",
		IsMatroid );

DeclareAttribute( "RankOfMatroid",
		IsMatroid );

DeclareAttribute( "RankFunction",
		IsMatroid );

DeclareAttribute( "ClosureFunction",
		IsMatroid );

DeclareAttribute( "Bases",
		IsMatroid );

DeclareAttribute( "Circuits",
		IsMatroid );

DeclareAttribute( "Cocircuits",
		IsMatroid );

DeclareAttribute( "Hyperplanes",
		IsMatroid );

DeclareAttribute( "TuttePolynomial",
		IsMatroid );

DeclareAttribute( "RankGeneratingPolynomial",
		IsMatroid );

DeclareAttribute( "Loops",
		IsMatroid );

DeclareAttribute( "Coloops",
		IsMatroid );

DeclareAttribute( "AutomorphismGroup",
		IsMatroid );


####################################
##
## Properties
##
####################################

DeclareProperty( "IsUniform",
		IsMatroid );

DeclareProperty( "IsSimpleMatroid",
		IsMatroid );

DeclareProperty( "IsGraphic",
		IsMatroid );

DeclareProperty( "IsRegular",
		IsMatroid );


####################################
##
## Methods
##
####################################

DeclareOperation( "GroundSet",
		[ IsMatroid ] );

DeclareOperation( "MatrixOfVectorMatroid",
		[ IsMatroid ] );

DeclareOperation( "Minor",
		[ IsMatroid, IsList, IsList ] );

DeclareOperation( "Deletion",
		[ IsMatroid, IsList ] );

DeclareOperation( "Contraction",
		[ IsMatroid, IsList ] );

DeclareOperation( "IsMinor",
		[ IsMatroid, IsMinorOfMatroid ] );

####################################
##
## Constructors
##
####################################

DeclareOperation( "Matroid",
		[ IsMatroid ] );

DeclareOperation( "Matroid",
		[ IsInt, IsList ] );

DeclareOperation( "MatroidNC",
		[ IsInt, IsList ] );

DeclareOperation( "Matroid",
		[ IsList, IsList ] );

DeclareOperation( "MatroidNC",
		[ IsList, IsList ] );

#DeclareOperation( "Matroid",		# this declaration is implied by the next one
#		[ IsMatrix ] );

DeclareOperation( "Matroid",
		[ IsGeneralizedRowVector and IsNearAdditiveElementWithInverse and IsAdditiveElement ] );

DeclareOperation( "Matroid",
		[ IsMatrixObj ] );

#DeclareOperation( "Matroid",		# implied by IsMatrixObj
#		[ IsHomalgMatrix ] );

DeclareOperation( "RandomVectorMatroidOverFinitePrimeField",
		[ IsInt, IsInt, IsInt ] );

DeclareOperation( "RandomVectorMatroidOverRationals",
		[ IsInt, IsInt ] );

DeclareOperation( "Matroid",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByCircuits",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByRankFunction",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidOfGraph",
		[ IsMatrix ] );
