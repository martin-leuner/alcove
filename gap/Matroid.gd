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

## Special matroids:

DeclareOperation( "RandomVectorMatroidOverPrimeField",
		[ IsInt, IsInt, IsInt ] );

DeclareOperation( "UniformMatroid",
		[ IsInt, IsInt ] );

DeclareOperation( "UniformMatroidNL",
		[ IsInt, IsInt ] );


####################################
##
## Attributes
##
####################################

DeclareAttributeWithDocumentation( "DualMatroid",
				IsMatroid,
		 		[ "Computes the dual matroid of <A>mat</A>. If possible, it will be given in the same representation." ],
				"a matroid",
				"mat",
				[ "Matroids", "Constructions" ]
			);

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

DeclareAttributeWithDocumentation( "Bases",
				IsMatroid,
				[ "Computes the bases of <A>mat</A>. For larger matroids this may take a very long time." ],
				"a list of bases",
				"mat",
				[ "Matroids", "Bases,_circuits_and_all_their_companions" ]
			);

DeclareAttribute( "KnownBases",
		IsMatroid,
		"mutable" );

DeclareAttributeWithDocumentation( "Circuits",
				IsMatroid,
				[ "Computes the circuits of <A>mat</A>. This is done using an incremental polynomial time",
				  "algorithm, so for matroids with many circuits this may take a long time." ],
				"a list of circuits",
				"mat",
				[ "Matroids", "Bases,_circuits_and_all_their_companions" ]
			);

DeclareAttribute( "KnownCircuits",
		IsMatroid,
		"mutable" );

DeclareAttribute( "FundamentalCircuitsWithBasis",
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

DeclareAttribute( "DirectSumDecomposition",
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

DeclareProperty( "IsConnected",
		IsMatroid );


####################################
##
## Methods
##
####################################

DeclareOperation( "SomeBasis",
		[ IsMatroid ] );

DeclareOperation( "MatrixOfVectorMatroid",
		[ IsMatroid ] );

DeclareOperationWithDocumentation( "Minor",
				[ IsMatroid, IsList, IsList ],
				[ "Computes the minor <A>mat</A> \\ <A>del</A> / <A>con</A> where <A>del</A> and <A>con</A> are subsets of the ground set of <A>mat</A>." ],
				"a matroid",
				"mat,del,con",
				[ "Matroids", "Constructions" ]
			);

DeclareOperation( "MinorNL",
		[ IsMatroid, IsList, IsList ] );

DeclareOperationWithDocumentation( "Deletion",
				[ IsMatroid, IsList ],
				[ "Computes the minor <A>mat</A> \\ <A>del</A> where <A>del</A> is a subset of the ground set of <A>mat</A>." ],
				"a matroid",
				"mat,del",
				[ "Matroids", "Constructions" ]
			);

DeclareOperationWithDocumentation( "Contraction",
				[ IsMatroid, IsList ],
				[ "Computes the minor <A>mat</A> / <A>con</A> where <A>con</A> is a subset of the ground set of <A>mat</A>." ],
				"a matroid",
				"mat,con",
				[ "Matroids", "Constructions" ]
			);

DeclareOperation( "IsMinor",
		[ IsMatroid, IsMinorOfMatroid ] );


