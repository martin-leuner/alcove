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

#! @Description
#!  Returns the vector matroid corresponding to the homalg matrix <A>h</A>. 
#!  Note that homalg matrices know the ring containing their entries.
#!  <Ref Func="MatroidNL" Label="for IsHomalgMatrix"/> returns a matroid which does not automatically 
#!  use logical implications.
#! @Returns a vector matroid
#! @Arguments h
#! @ChapterInfo Matroids, Construction
#! @Group Matrix-Constructor
#! @Label Matroid
DeclareOperation( "Matroid",
                  [ IsHomalgMatrix ] );

#! @Description
#!  
#! @Returns a vector matroid
#! @Arguments h
#! @ChapterInfo Matroids, Construction
#! @Group Matrix-Constructor
#! @Label Matroid
DeclareOperation( "MatroidNL",
                  [ IsHomalgMatrix ] );

## Abstract matroids:
#! @Description
#!  The argument <A>ground</A> must be either a list or an integer.<P/>
#!  In the first case, <Ref Func="MatroidByBases" Label="for IsList, IsList"/> 
#!  returns the abstract matroid on <A>ground</A>, in the second case 
#!  the abstract matroid on [ 1 .. <A>ground</A> ] having 
#!  the list <A>bases</A> as list of bases. 
#!  The elements of <A>bases</A> must be subsets of the ground set. 
#!  <Ref Func="MatroidByBases" Label="for IsList, IsList"/> checks the base exchange axiom. To construct an 
#!  abstract matroid with bases without any checks, use 
#!  <Ref Func="MatroidByBasesNC" Label="for IsList, IsList"/>. In addition to that, if you do not want the matroid 
#!  to automatically use logical implications, use <Ref Func="MatroidByBasesNCL" Label="for IsList, IsList"/>.
#! @Returns an abstract matroid with bases
#! @Arguments ground,bases
#! @ChapterInfo Matroids, Construction
#! @Group Bases-Constructor
#! @Label MatroidByBases
DeclareOperation( "MatroidByBases",
                  [ IsList, IsList ] );

#! @Description
#!  
#! @Returns an abstract matroid with bases
#! @Arguments ground,bases
#! @ChapterInfo Matroids, Construction
#! @Group Bases-Constructor
#! @Label MatroidByBases
DeclareOperation( "MatroidByBasesNC",
                  [ IsList, IsList ] );

#! @Description
#!  
#! @Returns an abstract matroid with bases
#! @Arguments ground,bases
#! @ChapterInfo Matroids, Construction
#! @Group Bases-Constructor
#! @Label MatroidByBases
DeclareOperation( "MatroidByBasesNCL",
                  [ IsList, IsList ] );


DeclareOperation( "MatroidByBases",
                [ IsInt, IsList ] );

DeclareOperation( "MatroidByBasesNC",
                [ IsInt, IsList ] );

DeclareOperation( "MatroidByBasesNCL",
                [ IsInt, IsList ] );

#DeclareOperation( "MatroidByIndependenceOracle",
#		[ IsList, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNC",
                [ IsList, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNCL",
                [ IsList, IsFunction ] );

#DeclareOperation( "MatroidByIndependenceOracle",
#		[ IsInt, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNC",
                [ IsInt, IsFunction ] );

DeclareOperation( "MatroidByIndependenceOracleNCL",
                [ IsInt, IsFunction ] );

#! @Description
#!  Returns the abstract matroid having the list <A>gset</A> as ground set and 
#!  the list <A>circs</A> as list of circuits. 
#!  The elements of <A>circs</A> must be subsets of <A>gset</A>. 
#!  The method checks the circuit elimination axiom. To construct an abstract 
#!  matroid with circuits without any checks, use the method MatroidByCircuitsNCL.
#! @Returns an abstract matroid with circuits
#! @Arguments gset,circs
#! @ChapterInfo Matroids, Construction
DeclareOperation( "MatroidByCircuits",
                  [ IsList, IsList ] );


DeclareOperation( "MatroidByCircuitsNC",
                [ IsList, IsList ] );

DeclareOperation( "MatroidByCircuitsNCL",
                [ IsList, IsList ] );

#! @Description
#!  Returns the abstract matroid on <A>n</A> elements having the list 
#!  <A>circs</A> as list of circuits. 
#!  The elements of <A>circs</A> must be subsets of [ 1 .. <A>n</A> ]. 
#!  The method checks the circuit elimination axiom. To construct an abstract 
#!  matroid with circuits without any checks, use the method MatroidByCircuitsNCL.
#! @Returns an abstract matroid with circuits
#! @Arguments n,circs
#! @ChapterInfo Matroids, Construction
DeclareOperation( "MatroidByCircuits",
                  [ IsInt, IsList ] );


DeclareOperation( "MatroidByCircuitsNC",
                [ IsInt, IsList ] );

DeclareOperation( "MatroidByCircuitsNCL",
                [ IsInt, IsList ] );

#! @Description
#!  Returns the abstract matroid having the list <A>gset</A> as ground set and 
#!  the function <A>rk</A> as rank function. 
#!  <A>rk</A> must be well-defined on the power set of <A>gset</A>. 
#!  The method checks whether <A>rk</A> is submodular and respects inclusion. 
#!  These checks are extremely expensive. To construct an abstract matroid 
#!  with rank function without any checks, use the method MatroidByRankFunctionNCL.
#! @Returns an abstract matroid with rank function
#! @Arguments gset,rk
#! @ChapterInfo Matroids, Construction
DeclareOperation( "MatroidByRankFunction",
                  [ IsList, IsFunction ] );


DeclareOperation( "MatroidByRankFunctionNC",
                [ IsList, IsFunction ] );

DeclareOperation( "MatroidByRankFunctionNCL",
                [ IsList, IsFunction ] );

#! @Description
#!  Returns the abstract matroid on <A>n</A> elements having the function 
#!  <A>rk</A> as rank function. 
#!  <A>rk</A> must be well-defined on the power set of [ 1 .. <A>n</A> ]. 
#!  The method checks whether <A>rk</A> is submodular and respects inclusion. 
#!  These checks are extremely expensive. To construct an abstract matroid 
#!  with rank function without any checks, use the method MatroidByRankFunctionNCL.
#! @Returns an abstract matroid with rank function
#! @Arguments n,rk
#! @ChapterInfo Matroids, Construction
DeclareOperation( "MatroidByRankFunction",
                  [ IsInt, IsFunction ] );


DeclareOperation( "MatroidByRankFunctionNC",
                [ IsInt, IsFunction ] );

DeclareOperation( "MatroidByRankFunctionNCL",
                [ IsInt, IsFunction ] );

#DeclareOperation( "MatroidOfGraph",
#		[ IsMatrix ] );

## Special matroids:

#! @Description
#!  Returns the vector matroid corresponding to a random <A>k</A> by <A>n</A> matrix 
#!  over the prime field of characteristic <A>p</A>. This method is intended mainly 
#!  for testing purposes.
#! @Returns a vector matroid
#! @Arguments k,n,p
#! @ChapterInfo Matroids, Construction
DeclareOperation( "RandomVectorMatroidOverPrimeField",
                  [ IsInt, IsInt, IsInt ] );

#! @Description
#!  Returns the rank <A>k</A> uniform matroid on <A>n</A> elements as an abstract 
#!  matroid with known rank function.
#! @Returns an abstract matroid with rank function
#! @Arguments k,n
#! @ChapterInfo Matroids, Construction
DeclareOperation( "UniformMatroid",
                  [ IsInt, IsInt ] );


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

#! @Description
#!  Computes the dual matroid of <A>mat</A>. If <A>mat</A> is not known to be 
#!  connected, the dual will be given as the direct sum of the duals of <A>mat</A>'s 
#!  connected components.
#! @Returns a matroid
#! @Arguments mat
#! @ChapterInfo Matroids, Construction
DeclareAttribute( "DualMatroid",
                  IsMatroid );


DeclareAttribute( "ParentAttr",
                IsMinorOfMatroid );

DeclareAttribute( "SimplifiedMatroid",
                IsMatroid );

#! @Description
#!  Computes the reduced row echelon form of the underlying matrix of the vector 
#!  matroid <A>mat</A> and returns its columns other than the first occurence of 
#!  each unit vector along with the corresponding column labels. This attribute is 
#!  mainly intended for internal use.
#! @Returns a list
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareAttribute( "StandardMatrixOfVectorMatroid",
                  IsMatroid );

#! @Description
#!  Returns the ground set of the matroid <A>mat</A>.
#! @Returns a set
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareOperation( "GroundSet",
                  [ IsMatroid ] );

#! @Description
#!  Returns the number of elements of the matroid <A>mat</A>.
#! @Returns a non-negative integer
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareAttribute( "Size",
                  IsMatroid );


DeclareAttribute( "NominalGroundSet",
                IsMatroid );

#! @Description
#!  Returns the rank of <A>mat</A>.
#! @Returns a non-negative integer
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareAttribute( "RankOfMatroid",
                  IsMatroid );

#! @Chapter Matroids
#! @Section Bases, circuits and their companions

#! @Description
#!  Computes the bases of <A>mat</A>. For larger matroids this may take a very long 
#!  time.
#! @Returns a list of bases
#! @Arguments mat
DeclareAttribute( "Bases",
                  IsMatroid );

DeclareAttribute( "KnownBases",
                IsMatroid,
                "mutable" );

#! @Description
#!  Computes the circuits of <A>mat</A>. This is done using an incremental 
#!  polynomial time algorithm, so for matroids with many circuits this may 
#!  take a long time.
#! @Returns a list of circuits
#! @Arguments mat
DeclareAttribute( "Circuits",
                  IsMatroid );


DeclareAttribute( "KnownCircuits",
                IsMatroid,
                "mutable" );

DeclareAttribute( "FundamentalCircuitsWithBasis",
                IsMatroid );

DeclareAttribute( "Cocircuits",
                IsMatroid );

DeclareAttribute( "Hyperplanes",
                IsMatroid );

#! @Description
#!  Returns a function mapping subsets of <A>mat</A>'s ground set to their 
#!  rank in <A>mat</A>.
#! @Returns a function
#! @Arguments mat
DeclareAttribute( "RankFunction",
                  IsMatroid );

#! @Description
#!  Returns a function mapping subsets of <A>mat</A>'s ground set to their 
#!  closure in <A>mat</A>.
#! @Returns a function
#! @Arguments mat
DeclareAttribute( "ClosureOperator",
                  IsMatroid );

#! @Description
#!  Returns a function mapping flats <A>X</A> of <A>mat</A> to the flat 
#!  obtained by deleting the coloops of <A>mat|X</A>. The function does NOT 
#!  check whether its input actually is a flat.
#! @Returns a function
#! @Arguments mat
DeclareAttribute( "EssentialityOperator",
                  IsMatroid );

#! @Description
#!  Returns a boolean function which decides the independence of subsets of 
#!  <A>mat</A>'s ground set.
#! @Returns a function
#! @Arguments mat
DeclareAttribute( "IndependenceOracle",
                  IsMatroid );

#! @Description
#!  Returns a boolean function which decides whether a subset of 
#!  <A>mat</A>'s ground set is a circuit.
#! @Returns a function
#! @Arguments mat
DeclareAttribute( "CircuitOracle",
                  IsMatroid );

#! @Description
#!  Creates two indeterminates for the Tutte polynomial.
#! @Returns a list two indeterminates
DeclareGlobalFunction( "IndeterminatesOfTuttePolynomial" );


DeclareAttribute( "TuttePolynomial",
                  IsMatroid );

#! @Description
#!  Computes the Tutte polynomial of the matroid <A>mat</A> as a polynomial expression
#!  in &ZZ;<M>[</M><A>x</A>,<A>y</A><M>]</M>. The method uses an exponential
#!  time algorithm, so the computation is feasible only for matroids on very few
#!  elements. Moreover, it is much faster for vector matroids than for isomorphic
#!  abstract matroids. If the ring elements <A>x</A>,<A>y</A> are not specified
#!  (as second and third argument) predefined indeterminates over &ZZ; will be used instead.
#! @Returns a polynomial in &ZZ;<M>[</M><A>x</A>,<A>y</A><M>]</M>
#! @Arguments mat, [x, y]
DeclareOperation( "TuttePolynomial",
                  [ IsMatroid, IsRingElement, IsRingElement ] );

#! @Description
#!  Computes the rank generating polynomial (or Whitney polynomial) of <A>mat</A>. 
#!  This method calls TuttePolynomial, so the same comments on its runtime apply.
#! @Returns a polynomial in &ZZ;<M>[</M><A>x</A>,<A>y</A><M>]</M>
#! @Arguments mat
DeclareAttribute( "RankGeneratingPolynomial",
                  IsMatroid );

#! @Description
#!  Creates an indeterminate for the characteristic polynomial.
#! @Returns an indeterminate
DeclareGlobalFunction( "IndeterminateOfCharacteristicPolynomial" );

#! @Description
#!  Returns the characteristic polynomial of the matroid <A>mat</A>
#! @Returns a univariate polynomial in <M>t=</M><C>IndeterminateOfCharacteristicPolynomial()</C>
#! @Arguments mat
DeclareAttribute( "CharacteristicPolynomial",
                  IsMatroid );


DeclareOperation( "CharacteristicPolynomial",
                  [ IsMatroid, IsRingElement ] );

#! @Description
#!  Returns the Poincare polynomial of the matroid <A>mat</A>
#! @Returns a univariate polynomial in <M>t=</M><C>IndeterminateOfCharacteristicPolynomial()</C>
#! @Arguments mat
DeclareAttribute( "PoincarePolynomial",
                  IsMatroid );


DeclareOperation( "PoincarePolynomial",
                  [ IsMatroid, IsRingElement ] );

#! @Description
#!  Returns the leading coefficient of the Poincare polynomial of the matroid <A>mat</A>
#! @Returns a positive integer
#! @Arguments mat
DeclareAttribute( "LeadingCoefficientOfPoincarePolynomial",
                  IsMatroid );

#! @Description
#!  Returns the set of elements of <A>mat</A> which are not contained in any 
#!  basis.
#! @Returns a set
#! @Arguments mat
DeclareAttribute( "Loops",
                  IsMatroid );

#! @Description
#!  Returns the set of elements of <A>mat</A> which are contained in every basis.
#! @Returns a set
#! @Arguments mat
DeclareAttribute( "Coloops",
                  IsMatroid );


DeclareAttribute( "AutomorphismGroup",
                IsMatroid );

DeclareAttribute( "KnownAutomorphisms",
                IsMatroid,
                "mutable" );

DeclareAttribute( "DirectSumDecomposition",
                IsMatroid );

DeclareAttribute( "TwoSumDecomposition",
                IsMatroid );

DeclareAttribute( "Flats",
                IsMatroid );

DeclareAttribute( "FlatsOfRank",
                IsMatroid,
                "mutable" );

#! @Description
#!  Returns the set of rank one flats of <A>mat</A> with more than one element.
#! @Returns a set
#! @Arguments mat
DeclareAttribute( "NonTrivialParallelClasses",
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

DeclareProperty( "Is3Connected",
                IsMatroid );

DeclareProperty( "ProbablyHasManyLargeCircuits",
                IsMatroid );


####################################
##
## Methods
##
####################################

DeclareOperation( "SomeBasis",
                [ IsMatroid ] );

#! @Description
#!  Returns the homalg matrix which was used to define the vector matroid <A>mat</A>.
#! @Returns a homalg matrix
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareOperation( "MatrixOfVectorMatroid",
                  [ IsMatroid ] );

#! @Description
#!  Computes the minor <A>mat</A> \ <A>del</A> / <A>con</A> where <A>del</A> and 
#!  <A>con</A> are subsets of the ground set of <A>mat</A>.
#! @Returns a matroid
#! @Arguments mat,del,con
#! @ChapterInfo Matroids, Construction
DeclareOperation( "Minor",
                  [ IsMatroid, IsList, IsList ] );


DeclareOperation( "MinorNL",
                [ IsMatroid, IsList, IsList ] );

#! @Description
#!  Computes the minor <A>mat</A> \ <A>del</A> where <A>del</A> is a subset of the 
#!  ground set of <A>mat</A>.
#! @Returns a matroid
#! @Arguments mat,del
#! @ChapterInfo Matroids, Construction
DeclareOperation( "Deletion",
                  [ IsMatroid, IsList ] );

#! @Description
#!  Computes the minor <A>mat</A> / <A>con</A> where <A>con</A> is a subset of the 
#!  ground set of <A>mat</A>.
#! @Returns a matroid
#! @Arguments mat,con
#! @ChapterInfo Matroids, Construction
DeclareOperation( "Contraction",
                  [ IsMatroid, IsList ] );

#! @Description
#!  Computes <A>mat</A> / <A>comp</A> assuming that the subset <A>comp</A> of the 
#!  ground set of <A>mat</A> is a connected component. This method is mainly 
#!  intended for internal use. It does NOT check whether <A>comp</A> actually 
#!  is a connected component.
#! @Returns a matroid
#! @Arguments mat,con
#! @ChapterInfo Matroids, Construction
DeclareOperation( "RestrictionToComponentNC",
                  [ IsMatroid, IsList ] );


DeclareOperation( "IsMinor",
                [ IsMatroid, IsMinorOfMatroid ] );

DeclareOperation( "DirectSumOfMatroidsNL",
                [ IsMatroid, IsMatroid ] );

#! @Description
#!  Computes the direct sum <A>M1</A> + <A>M2</A> as an abstract matroid. 
#!  DirectSumOfMatroids silently applies DirectSumDecomposition to its arguments. 
#!  The operator \+ (and consequently the function Sum) may also be used to 
#!  construct direct sums of matroids.
#! @Returns a matroid
#! @Arguments M1,M2
#! @ChapterInfo Matroids, Construction
DeclareOperation( "DirectSumOfMatroids",
                  [ IsMatroid, IsMatroid ] );

#! @Description
#!  Computes the 2-sum <A>M1</A> +_2 <A>M2</A> as an abstract matroid. 
#!  To obtain it, consider the parallel connection P(<A>M1</A>,<A>M2</A>) 
#!  identifying the elements <A>p1</A> and <A>p2</A> and delete the 
#!  common element. Note that <A>p1</A> must be neither a loop nor a coloop 
#!  of <A>M1</A> and the same holds for <A>p2</A> in <A>M2</A>. 
#!  Moreover, <A>M1</A> and <A>M2</A> need to have at least 3 elements.
#! @Returns a matroid
#! @Arguments M1,p1,M2,p2
#! @ChapterInfo Matroids, Construction
DeclareOperation( "TwoSumOfMatroids",
                  [ IsMatroid, IsInt, IsMatroid, IsInt ] );


DeclareOperation( "TwoSumOfMatroidsNL",
                [ IsMatroid, IsInt, IsMatroid, IsInt ] );

DeclareOperation( "TwoSum",
                [ IsMatroid, IsInt, IsMatroid, IsInt ] );

DeclareOperation( "TwoSumNL",
                [ IsMatroid, IsInt, IsMatroid, IsInt ] );

#! @Description
#!  The underlying homalg ring of <A>mat</A>.
#! @Returns a vector matroid
#! @Arguments mat
#! @ChapterInfo Matroids, Accessing attributes
DeclareOperation( "HomalgRing",
                  [ IsMatroid ] );
