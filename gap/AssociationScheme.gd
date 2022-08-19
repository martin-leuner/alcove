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

DeclareAttribute( "AdjacencyMatrices",
                IsAssociationScheme );

DeclareAttribute( "AdjacencyAlgebra",
                IsAssociationScheme );


####################################
##
## Properties
##
####################################

DeclareProperty( "IsCommutative",
                IsAssociationScheme );

# needed for LAGUNA < 3.9.4 and other packages
# see https://github.com/gap-packages/laguna/issues/19 and linked issues
if IsBoundGlobal( "IsSymmetric" ) and IsAttribute( IsSymmetric ) then

DeclareAttribute( "IsSymmetric",
                  IsAssociationScheme );

else

DeclareProperty( "IsSymmetric",
                 IsAssociationScheme );

fi;

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
                [ IsListOrCollection, IsList ] );

DeclareOperation( "AssociationScheme",
                [ IsGroup, IsListOrCollection, IsFunction ] );

DeclareOperation( "AssociationScheme",
                [ IsGroup ] );

