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

DeclareAttribute( "NormalFormOfVectorMatroid",
		IsMatroid );

DeclareOperation( "GroundSet",
		[ IsMatroid ] );

DeclareAttribute( "SizeOfGroundSet",
		IsMatroid );

DeclareAttribute( "RankOfMatroid",
		IsMatroid );

DeclareAttribute( "RankFunction",
		IsMatroid );

DeclareAttribute( "ClosureFunction",
		IsMatroid );

DeclareAttribute( "IndependenceFunction",
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

## Copy:

DeclareOperation( "Matroid",
		[ IsMatroid ] );

## Vector matroids:

DeclareOperation( "Matroid",
		[ IsGeneralizedRowVector and IsNearAdditiveElementWithInverse and IsAdditiveElement ] );

DeclareOperation( "Matroid",
		[ IsMatrixObj ] );

DeclareOperation( "MatroidNL",
		[ IsMatrixObj ] );

## Abstract matroids:

DeclareOperation( "MatroidByBases",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByBasesNCL",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByBases",
		[ IsInt, IsList ] );

DeclareOperation( "MatroidByBasesNCL",
		[ IsInt, IsList ] );

DeclareOperation( "MatroidByIndependenceFunction",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByIndependenceFunctionNCL",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByIndependenceFunction",
		[ IsInt, IsFunction ] );

DeclareOperation( "MatroidByIndependenceFunctionNCL",
		[ IsInt, IsFunction ] );

DeclareOperation( "MatroidByCircuits",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByCircuitsNCL",
		[ IsList, IsList ] );

DeclareOperation( "MatroidByCircuits",
		[ IsInt, IsList ] );

DeclareOperation( "MatroidByCircuitsNCL",
		[ IsInt, IsList ] );

DeclareOperation( "MatroidByRankFunction",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByRankFunctionNCL",
		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByRankFunction",
		[ IsInt, IsFunction ] );

DeclareOperation( "MatroidByRankFunctionNCL",
		[ IsInt, IsFunction ] );

#DeclareOperation( "MatroidOfGraph",
#		[ IsMatrix ] );

DeclareOperation( "RandomVectorMatroidOverPrimeField",
		[ IsInt, IsInt, IsInt ] );

