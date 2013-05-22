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
                IsAlgebraWithOne );


####################################
##
## Attributes
##
####################################

DeclareAttribute( "DualCAlgebra",
                [ IsCAlgebra ] );

DeclareAttribute( "Eigenmatrix",
                [ IsCAlgebra ] );


####################################
##
## Properties
##
####################################


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

DeclareOperation( "CAlgebra",
                [ IsCAlgebra ] );

DeclareOperation( "CAlgebra",
                [ IsBasis, IsFunction ] );

DeclareOperation( "CAlgebra",
                [ IsAssociationScheme and IsCommutative ] );

