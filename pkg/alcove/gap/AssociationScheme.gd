#############################################################################
##
##  AssocationScheme.gd            alcove package               Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  Association scheme declarations for alcove.
##
#############################################################################

DeclareCategory( "IsAssociationScheme",
		IsObject );


####################################
##
## Attributes
##
####################################

DeclareAttribute( "AdjacencyAlgebra",
		IsAssociationScheme );

DeclareAttribute( "IsSymmetric",
		IsAssociationScheme );


####################################
##
## Properties
##
####################################

DeclareProperty( "IsCommutative",
		IsAssociationScheme );


####################################
##
## Methods
##
####################################

DeclareOperation( "GroundSet",
		[ IsAssociationScheme ] );

DeclareOperation( "RelationsOfAssociationScheme",
		[ IsAssociationScheme ] );

DeclareOperation( "GroupOfAssociationScheme",
		[ IsAssociationScheme ] );

DeclareOperation( "ActionOfAssociationScheme",
		[ IsAssociationScheme ] );


####################################
##
## Constructors
##
####################################

DeclareOperation( "AssociationScheme",
		[ IsAssociationScheme ] );

DeclareOperation( "AssociationScheme",
		[ IsList, IsList ] );

DeclareOperation( "AssociationScheme",
		[ IsGroup, IsList, IsFunction ] );

DeclareOperation( "AssociationScheme",
		[ IsGroup ] );

