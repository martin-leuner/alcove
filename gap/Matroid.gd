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

DeclareOperationWithDocumentation( "Matroid",
				[ IsHomalgMatrix ],
				[ "Returns the vector matroid corresponding to the homalg matrix <A>h</A>. ",
				  "Note that homalg matrices know the ring containing their entries." ],
				"a vector matroid",
				"h",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MatroidNL",
		[ IsHomalgMatrix ] );

## Abstract matroids:

DeclareOperationWithDocumentation( "MatroidByBases",
				[ IsList, IsList ],
				[ "Returns the abstract matroid having the list <A>gset</A> as ground set ",
				  "and the list <A>bases</A> as list of bases. ",
				  "The elements of <A>bases</A> must be subsets of <A>gset</A>. ",
				  "The method checks the base exchange axiom. To construct an abstract matroid ",
				  "with bases without any checks, use the method MatroidByBasesNCL." ],
				"an abstract matroid with bases",
				"gset,bases",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MatroidByBasesNCL",
		[ IsList, IsList ] );

DeclareOperationWithDocumentation( "MatroidByBases",
				[ IsInt, IsList ],
				[ "Returns the abstract matroid on <A>n</A> elements having ",
				  "the list <A>bases</A> as list of bases. ",
				  "The elements of <A>bases</A> must be subsets of [ 1 .. <A>n</A> ]. ",
				  "The method checks the base exchange axiom. To construct an abstract matroid ",
				  "with bases without any checks, use the method MatroidByBasesNCL." ],
				"an abstract matroid with bases",
				"n,bases",
				[ "Matroids", "Construction" ]
			 );

DeclareOperation( "MatroidByBasesNCL",
		[ IsInt, IsList ] );

#DeclareOperation( "MatroidByIndependenceOracle",
#		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNCL",
		[ IsList, IsFunction ] );

#DeclareOperation( "MatroidByIndependenceOracle",
#		[ IsInt, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNCL",
		[ IsInt, IsFunction ] );

DeclareOperationWithDocumentation( "MatroidByCircuits",
				[ IsList, IsList ],
				[ "Returns the abstract matroid having the list <A>gset</A> as ground set and ",
				  "the list <A>circs</A> as list of circuits. ",
				  "The elements of <A>circs</A> must be subsets of <A>gset</A>. ",
				  "The method checks the circuit elimination axiom. To construct an abstract ",
				  "matroid with circuits without any checks, use the method MatroidByCircuitsNCL." ],
				"an abstract matroid with circuits",
				"gset,circs",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MatroidByCircuitsNCL",
		[ IsList, IsList ] );

DeclareOperationWithDocumentation( "MatroidByCircuits",
				[ IsInt, IsList ],
				[ "Returns the abstract matroid on <A>n</A> elements having the list ",
				  "<A>circs</A> as list of circuits. ",
				  "The elements of <A>circs</A> must be subsets of [ 1 .. <A>n</A> ]. ",
				  "The method checks the circuit elimination axiom. To construct an abstract ",
				  "matroid with circuits without any checks, use the method MatroidByCircuitsNCL." ],
				"an abstract matroid with circuits",
				"n,circs",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MatroidByCircuitsNCL",
		[ IsInt, IsList ] );

DeclareOperationWithDocumentation( "MatroidByRankFunction",
				[ IsList, IsFunction ],
				[ "Returns the abstract matroid having the list <A>gset</A> as ground set and ",
				  "the function <A>rk</A> as rank function. ",
				  "<A>rk</A> must be well-defined on the power set of <A>gset</A>. ",
				  "The method checks whether <A>rk</A> is submodular and respects inclusion. ",
				  "These checks are extremely expensive. To construct an abstract matroid ",
				  "with rank function without any checks, use the method MatroidByRankFunctionNCL." ],
				"an abstract matroid with rank function",
				"gset,rk",
				[ "Matroids", "Construction" ]
			);


DeclareOperation( "MatroidByRankFunctionNCL",
		[ IsList, IsFunction ] );

DeclareOperationWithDocumentation( "MatroidByRankFunction",
				[ IsInt, IsFunction ],
				[ "Returns the abstract matroid on <A>n</A> elements having the function ",
				  "<A>rk</A> as rank function. ",
				  "<A>rk</A> must be well-defined on the power set of [ 1 .. <A>n</A> ]. ",
				  "The method checks whether <A>rk</A> is submodular and respects inclusion. ",
				  "These checks are extremely expensive. To construct an abstract matroid ",
				  "with rank function without any checks, use the method MatroidByRankFunctionNCL." ],
				"an abstract matroid with rank function",
				"n,rk",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MatroidByRankFunctionNCL",
		[ IsInt, IsFunction ] );

#DeclareOperation( "MatroidOfGraph",
#		[ IsMatrix ] );

## Special matroids:

DeclareOperationWithDocumentation( "RandomVectorMatroidOverPrimeField",
				[ IsInt, IsInt, IsInt ],
				[ "Returns the vector matroid corresponding to a random <A>k</A> by <A>n</A> matrix ",
				  "over the prime field of characteristic <A>p</A>. This method is intended mainly ",
				  "for testing purposes." ],
				"a vector matroid",
				"k,n,p",
				[ "Matroids", "Construction" ]
			);

DeclareOperationWithDocumentation( "UniformMatroid",
				[ IsInt, IsInt ],
				[ "Returns the rank <A>k</A> uniform matroid on <A>n</A> elements as an abstract ",
				  "matroid with known rank function." ],
				  "an abstract matroid with rank function",
				"k,n",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "UniformMatroidNL",
		[ IsInt, IsInt ] );


####################################
##
## Operators
##
####################################

DeclareOperation( "\+",
		[ IsMatroid, IsMatroid ] );

####################################
##
## Attributes
##
####################################

DeclareAttributeWithDocumentation( "DualMatroid",
				IsMatroid,
		 		[ "Computes the dual matroid of <A>mat</A>. If <A>mat</A> is not known to be ",
				  "connected, the dual will be given as the direct sum of the duals of <A>mat</A>'s ",
				  "connected components."  ],
				"a matroid",
				"mat",
				[ "Matroids", "Construction" ]
			);

DeclareAttribute( "ParentAttr",
		IsMinorOfMatroid );

DeclareAttribute( "SimplifiedMatroid",
		IsMatroid );

DeclareAttributeWithDocumentation( "NormalFormOfVectorMatroid",
				IsMatroid,
				[ "Computes the reduced row echelon form of the underlying matrix of the vector ",
				  "matroid <A>mat</A> and returns its columns other than the first occurence of ",
				  "each unit vector along with the corresponding column labels. This attribute is ",
				  "mainly intended for internal use." ],
				"a list",
				"mat",
				[ "Matroids", "Accessing_attributes" ]
			);

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

DeclareAttribute( "IndependenceOracle",
		IsMatroid );

DeclareAttribute( "CircuitOracle",
		IsMatroid );

DeclareAttributeWithDocumentation( "Bases",
				IsMatroid,
				[ "Computes the bases of <A>mat</A>. For larger matroids this may take a very long ",
				  "time." ],
				"a list of bases",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
			);

DeclareAttribute( "KnownBases",
		IsMatroid,
		"mutable" );

DeclareAttributeWithDocumentation( "Circuits",
				IsMatroid,
				[ "Computes the circuits of <A>mat</A>. This is done using an incremental ",
				  "polynomial time algorithm, so for matroids with many circuits this may ",
				  "take a long time." ],
				"a list of circuits",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
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

DeclareAttributeWithDocumentation( "TuttePolynomial",
				IsMatroid,
				[ "Computes the Tutte polynomial of <A>mat</A>. The method uses an exponential ",
				  "time algorithm, so the computation is feasible only for matroids on very few ",
				  "elements. Moreover, it is much faster for vector matroids than for isomorphic ",
				  "abstract matroids." ],
				"a polynomial in Z[x,y]",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
			);

DeclareAttributeWithDocumentation( "RankGeneratingPolynomial",
				IsMatroid,
				[ "Computes the rank generating polynomial (or Whitney polynomial) of <A>mat</A>. ",
				  "This method calls TuttePolynomial, so the same comments on its runtime apply." ],
				"a polynomial in Z[x,y]",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
			);

DeclareAttributeWithDocumentation( "Loops",
				IsMatroid,
				[ "Returns the set of elements of <A>mat</A> which are not contained in any ",
				  "basis." ],
				"a set",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
			 );

DeclareAttributeWithDocumentation( "Coloops",
				IsMatroid,
				[ "Returns the set of elements of <A>mat</A> which are contained in every basis." ],
				"a set",
				"mat",
				[ "Matroids", "Bases,_circuits_and_their_companions" ]
			);

DeclareAttribute( "AutomorphismGroup",
		IsMatroid );

DeclareAttribute( "DirectSumDecomposition",
		IsMatroid );

DeclareAttribute( "TwoSumDecomposition",
		IsMatroid );

DeclareAttribute( "Flats",
		IsMatroid );

DeclareAttribute( "FlatsOfRank",
		IsMatroid,
		"mutable" );


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

DeclareProperty( "Is3Connected",
		IsMatroid );


####################################
##
## Methods
##
####################################

DeclareOperation( "SomeBasis",
		[ IsMatroid ] );

DeclareOperationWithDocumentation( "MatrixOfVectorMatroid",
				[ IsMatroid ],
				[ "Returns the homalg matrix which was used to define the vector matroid <A>mat</A>." ],
				"a homalg matrix",
				"mat",
				[ "Matroids", "Accessing_attributes" ]
			);

DeclareOperationWithDocumentation( "Minor",
				[ IsMatroid, IsList, IsList ],
				[ "Computes the minor <A>mat</A> \\ <A>del</A> / <A>con</A> where <A>del</A> and ",
				  "<A>con</A> are subsets of the ground set of <A>mat</A>." ],
				"a matroid",
				"mat,del,con",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "MinorNL",
		[ IsMatroid, IsList, IsList ] );

DeclareOperationWithDocumentation( "Deletion",
				[ IsMatroid, IsList ],
				[ "Computes the minor <A>mat</A> \\ <A>del</A> where <A>del</A> is a subset of the ",
				  "ground set of <A>mat</A>." ],
				"a matroid",
				"mat,del",
				[ "Matroids", "Construction" ]
			);

DeclareOperationWithDocumentation( "Contraction",
				[ IsMatroid, IsList ],
				[ "Computes the minor <A>mat</A> / <A>con</A> where <A>con</A> is a subset of the ",
				  "ground set of <A>mat</A>." ],
				"a matroid",
				"mat,con",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "IsMinor",
		[ IsMatroid, IsMinorOfMatroid ] );

DeclareOperation( "DirectSumOfMatroidsNL",
		[ IsMatroid, IsMatroid ] );

DeclareOperationWithDocumentation( "DirectSumOfMatroids",
				[ IsMatroid, IsMatroid ],
				[ "Computes the direct sum <A>M1</A> + <A>M2</A> as an abstract matroid. ",
				  "DirectSumOfMatroids silently applies DirectSumDecomposition to its arguments. ",
				  "The operator \\+ (and consequently the function Sum) may also be used to ",
				  "construct direct sums of matroids." ],
				"a matroid",
				"M1,M2",
				[ "Matroids", "Construction" ]
			);

DeclareOperationWithDocumentation( "TwoSumOfMatroids",
				[ IsMatroid, IsInt, IsMatroid, IsInt ],
				[ "Computes the 2-sum <A>M1</A> +_2 <A>M2</A> as an abstract matroid. ",
				  "To obtain it, consider the parallel connection P(<A>M1</A>,<A>M2</A>) ",
				  "identifying the elements <A>p1</A> and <A>p2</A> and delete the ",
				  "common element. Note that <A>p1</A> must be neither a loop nor a coloop ",
				  "of <A>M1</A> and the same holds for <A>p2</A> in <A>M2</A>. ",
				  "Moreover, <A>M1</A> and <A>M2</A> need to have at least 3 elements." ],
				"a matroid",
				"M1,p1,M2,p2",
				[ "Matroids", "Construction" ]
			);

DeclareOperation( "TwoSumOfMatroidsNL",
		[ IsMatroid, IsInt, IsMatroid, IsInt ] );

DeclareOperation( "TwoSum",
		[ IsMatroid, IsInt, IsMatroid, IsInt ] );

DeclareOperation( "TwoSumNL",
		[ IsMatroid, IsInt, IsMatroid, IsInt ] );

DeclareOperationWithDocumentation( "HomalgRing",
				[ IsMatroid ],
				[ "The underlying homalg ring of <A>mat</A>." ],
				"a vector matroid",
				"mat",
				[ "Matroids", "Accessing_attributes" ]
			);

