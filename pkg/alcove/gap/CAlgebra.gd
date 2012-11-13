#############################################################################
##
##  CAlgebra.gd                alcove package                   Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  C-Algebra declarations for alcove.
##
#############################################################################

DeclareCategory( "IsCAlgebra",
		IsAlgebra );


####################################
##
## Attributes
##
####################################

DeclareAttribute( "Basis",
		IsCAlgebra );

DeclareAttribute( "Idempotents",
		IsCAlgebra );


####################################
##
## Properties
##
####################################

DeclareProperty( "IsCommutative",
		IsCAlgebra );


####################################
##
## Methods
##
####################################

DeclareOperation( "GroundSet",
		[ IsCAlgebra ] );

DeclareOperation( "RelationsOfCAlgebra",
		[ IsCAlgebra ] );

DeclareOperation( "GroupOfCAlgebra",
		[ IsCAlgebra ] );

DeclareOperation( "ActionOfCAlgebra",
		[ IsCAlgebra ] );


####################################
##
## Constructors
##
####################################

DeclareOperation( "CAlgebra",
		[ IsCAlgebra ] );

DeclareOperation( "CAlgebra",
		[ IsListOrCollection, IsList ] );

DeclareOperation( "CAlgebra",
		[ IsGroup, IsListOrCollection, IsFunction ] );

DeclareOperation( "CAlgebra",
		[ IsGroup ] );

