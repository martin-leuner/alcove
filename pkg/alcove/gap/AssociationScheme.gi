#############################################################################
##
##  AssocationScheme.gi            alcove package               Martin Leuner
##
##  Copyright 2012 Lehrstuhl B für Mathematik, RWTH Aachen
##
##  Association scheme methods for alcove.
##
#############################################################################

####################################
##
## Representations
##
####################################

DeclareRepresentation( "IsAssociationSchemeByRelationsRep",
	IsAssociationScheme and IsAttributeStoringRep,
	[ "groundSet", "relations" ]
);

DeclareRepresentation( "IsAssociationSchemeByFunctionsRep",
	IsAssociationScheme and IsAttributeStoringRep,
	[ "groundSet", "functions" ]
);

DeclareRepresentation( "IsAssociationSchemeByActionRep",
	IsAssociationScheme and IsAttributeStoringRep,
	[ "group", "groundSet", "action" ]
);

DeclareRepresentation( "IsAssociationSchemeByGroupRep",
	IsAssociationScheme and IsAttributeStoringRep,
	[ "group" ]
);


####################################
##
## Types and Families
##
####################################


BindGlobal( "TheFamilyOfAssociationSchemes",
	NewFamily( "TheFamilyOfAssociationSchemes" , IsAssociationScheme ) );

BindGlobal( "TheTypeRelationAssociationScheme",
	NewType( TheFamilyOfAssociationSchemes, IsAssociationSchemeByRelationsRep ) );

BindGlobal( "TheTypeFunctionAssociationScheme",
	NewType( TheFamilyOfAssociationSchemes, IsAssociationSchemeByFunctionsRep ) );

BindGlobal( "TheTypeActionAssociationScheme",
	NewType( TheFamilyOfAssociationSchemes, IsAssociationSchemeByActionRep ) );

BindGlobal( "TheTypeGroupAssociationScheme",
	NewType( TheFamilyOfAssociationSchemes, IsAssociationSchemeByGroupRep ) );


####################################
##
## Attributes
##
####################################


####################
## AdjacencyMatrices

InstallMethod( AdjacencyMatrices,
		"for relation association schemes",
		[ IsAssociationSchemeByRelationsRep ],

 function( ascheme )
  local rels, dom;

  rels := RelationsOfAssociationScheme( ascheme );
  dom := GroundSet( ascheme );

  return List( rels, rel -> List( dom, i -> List( dom, function(j) if [i,j] in rel then return 1; else return 0; fi; end ) ) );
 end

);

InstallMethod( AdjacencyMatrices,
		"for relation association schemes",
		[ IsAssociationSchemeByFunctionsRep ],

 function( ascheme )
  local funcs, dom;

  funcs := RelationsOfAssociationScheme( ascheme );
  dom := GroundSet( ascheme );

  return List( funcs, f -> List( dom, i -> List( dom, function(j) if f(i,j) then return 1; else return 0; fi; end ) ) );
 end

);

InstallMethod( AdjacencyMatrices,				# CAUTION: Naive and probably less-than-optimal approach.
		"for group association schemes",
		[ IsAssociationSchemeByGroupRep ],

 function( ascheme )
  local grp, conj;

  grp := GroundSet( ascheme );
  conj := ConjugacyClasses( GroupOfAssociationScheme( ascheme ) );

  return List( conj, class -> List( grp, i -> List( grp, function(j) if j^-1*i in class then return 1; else return 0; fi; end ) ) );
 end

);

InstallMethod( AdjacencyMatrices,
		"for association schemes defined by a transitive action",
		[ IsAssociationSchemeByActionRep ],

 function( ascheme )
  local dom, stab, staborbs, reps, orb, mats, mat, tmporb, i, grp, act;
 
  grp := GroupOfAssociationScheme( ascheme );
  dom := GroundSet( ascheme );
  act := ActionOfAssociationScheme( ascheme );

  stab := Stabiliser( grp, dom[1], act );
  staborbs := OrbitsDomain( stab, dom, act );
 
  reps := [];
  for i in [ 1 .. Size(dom) ] do reps[i] := RepresentativeAction( grp, dom[1], dom[i], act ); od;
 
  mats := [];
  for orb in staborbs do
   mat := [];
   for i in [ 1 .. Size(dom) ] do
    tmporb := List( orb, x -> act(x,reps[i]) );
    Add( mat, List( [ 1 .. Size(dom) ], function(j) if dom[j] in tmporb then return 1; else return 0; fi; end ) );
   od;
   Add( mats, mat );
  od;
 
  return mats;
 end

);


###################
## AdjacencyAlgebra

InstallMethod( AdjacencyAlgebra,
		"for association schemes",
		[ IsAssociationScheme ],

 function( ascheme )
  local irreds, F, mats;

  mats := AdjacencyMatrices( ascheme );

  F := Rationals;
  irreds := Filtered( Union( List( mats, m -> Factors( MinimalPolynomial( F, m ) ) ) ), p -> Degree(p) > 1 );
  while not IsEmpty( irreds ) do
   F := FieldExtension( F, irreds[1] );
   irreds := Filtered( Union( List( irreds, f -> Factors( PolynomialRing(F,1), f ) ) ), p -> Degree(p) > 1 );
  od;

  return Algebra( F, mats );
 end

);


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


############
## GroundSet

InstallMethod( GroundSet,
		"for association schemes",
		[ IsAssociationScheme ],

 function( ascheme )
  if IsBound( ascheme!.groundSet ) then
   return ascheme!.groundSet;
  else
   Error( "this association scheme apparently lost its ground set, this shouldn't happen" );
  fi;
 end

);

InstallMethod( GroundSet,
		"for group association schemes",
		[ IsAssociationSchemeByGroupRep ],
		10,

 function( ascheme )
  if IsBound( ascheme!.group ) then
   return EnumeratorSorted( ascheme!.group );
  else
   Error( "this group association scheme apparently lost its group, this shouldn't happen" );
  fi;
 end

);

InstallMethod( GroundSet,
		"for association schemes defined by a transitive action",
		[ IsAssociationSchemeByActionRep ],
		10,

 function( ascheme )
  if IsBound( ascheme!.groundSet ) then
   return EnumeratorSorted( ascheme!.groundSet );
  else
   Error( "this association scheme apparently lost its ground set, this shouldn't happen" );
  fi;
 end

);


###############################
## RelationsOfAssociationScheme

InstallMethod( RelationsOfAssociationScheme,
		"for relation association schemes",
		[ IsAssociationSchemeByRelationsRep ],

 function( ascheme )
  if IsBound( ascheme!.relations ) then
   return ascheme!.relations;
  else
   Error( "this association scheme apparently lost its set of relations, this shouldn't happen" );
  fi;
 end

);

InstallMethod( RelationsOfAssociationScheme,
		"for relation association schemes",
		[ IsAssociationSchemeByFunctionsRep ],

 function( ascheme )
  if IsBound( ascheme!.functions ) then
   return ascheme!.functions;
  else
   Error( "this association scheme apparently lost its set of relations, this shouldn't happen" );
  fi;
 end

);

InstallMethod( RelationsOfAssociationScheme,					# POTENTIALLY EXPENSIVE AND NAIVE APPROACH, SHOULD DO THIS DIFFERENTLY IF ADJACENCY ALGEBRA IS KNOWN
		"for association schemes defined by a transitive action",
		[ IsAssociationSchemeByActionRep ],

 function( ascheme )
  local dom, grp, act;
  dom := GroundSet( ascheme );
  grp := GroupOfAssociationScheme( ascheme );
  act := ActionOfAssociationScheme( ascheme );
  return OrbitsDomain( grp, Cartesian2( dom, dom ), function( tup, g ) return [ act( tup[1], g ), act( tup[2], g ) ]; end );
 end

);


###########################
## GroupOfAssociationScheme

InstallMethod( GroupOfAssociationScheme,
		"for group association schemes",
		[ IsAssociationSchemeByGroupRep ],

 function( ascheme )
  if IsBound( ascheme!.group ) then
   return ascheme!.group;
  else
   Error( "this group association scheme apparently lost its group, this shouldn't happen" );
  fi;
 end

);

InstallMethod( GroupOfAssociationScheme,
		"for association schemes defined by a transitive action",
		[ IsAssociationSchemeByActionRep ],

 function( ascheme )
  if IsBound( ascheme!.group ) then
   return ascheme!.group;
  else
   Error( "this association scheme apparently lost its group, this shouldn't happen" );
  fi;
 end

);


############################
## ActionOfAssociationScheme

InstallMethod( ActionOfAssociationScheme,
		"for association schemes defined by a transitive action",
		[ IsAssociationSchemeByActionRep ],

 function( ascheme )
  if IsBound( ascheme!.action ) then
   return ascheme!.action;
  else
   Error( "this association scheme apparently lost its action, this shouldn't happen" );
  fi;
 end

);


####################################
##
## Constructors
##
####################################


##
InstallMethod( AssociationScheme,
		"copy constructor",
		[ IsAssociationScheme ],

 IdFunc

);


##
InstallMethod( AssociationScheme,
		"by ground set and relations",
		[ IsListOrCollection, IsList ],
		10,

 function( gset, rels )
  local aScheme, srels, sgset;

  if IsEmpty(rels) or not IsList(rels[1]) then TryNextMethod(); fi;

# Sort ground set and relations:
  sgset := Set( Enumerator( gset ) );
  srels := List( rels, rel -> List( rel, Set ) );

# Check whether relations satisfy conditions:
# TO DO

  aScheme := Objectify( TheTypeRelationAssociationScheme, rec( groundSet := Immutable(sgset), relations := Immutable(srels) ) );

  return aScheme;
 end

);


##
InstallMethod( AssociationScheme,
		"by ground set and functions",
		[ IsListOrCollection, IsList ],

 function( gset, funcs )
  local aScheme, sgset;

  sgset := Set( Enumerator( gset ) );

# Check whether relations satisfy conditions:
# TO DO

  aScheme := Objectify( TheTypeFunctionAssociationScheme, rec( groundSet := Immutable(sgset), functions := Immutable(funcs) ) );
 end

);


##
InstallMethod( AssociationScheme,
		"by transitive action",
		[ IsGroup, IsListOrCollection, IsFunction ],

 function( grp, dom, act )
  local aScheme;

  if not IsTransitive( grp, dom, act ) then
   Error( "<grp> must act transitively on <dom>" );
  fi;

  aScheme := Objectify( TheTypeActionAssociationScheme, rec( group := Immutable(grp), groundSet := Immutable(dom), action := Immutable(act) ) );

  return aScheme;
 end

);


##
InstallMethod( AssociationScheme,
		"by group",
		[ IsGroup ],

 function( grp )
  local aScheme;

  aScheme := Objectify( TheTypeGroupAssociationScheme, rec( group := Immutable(grp) ) );
  SetIsCommutative( aScheme, true );

  return aScheme;
 end

);


####################################
##
## Display Methods
##
####################################

##
InstallMethod( PrintObj,
		"for association schemes",
		[ IsAssociationScheme ],

 function( ascheme )

  if IsAssociationSchemeByGroupRep( ascheme ) then

   Print( "<A group association scheme>" );

  else

   if 	( HasIsCommutative( ascheme ) and IsCommutative( ascheme ) ) or
	( HasIsSymmetric( ascheme ) and IsSymmetric( ascheme ) ) then
    Print( "<A" );
   else
    Print( "<An" );
   fi;

   if HasIsCommutative( ascheme ) and IsCommutative( ascheme ) then
    Print( " commutative" );
   fi;

   if HasIsSymmetric( ascheme ) and IsSymmetric( ascheme ) then
    Print( " symmetric" );
   fi;

   Print( " association scheme>" );

  fi;

 end

);

##
InstallMethod( Display,
		"for group association schemes",
		[ IsAssociationSchemeByGroupRep ],

 function( ascheme )

  Print( "The group association scheme of " );
  Display( GroupOfAssociationScheme( ascheme ) );

 end

);

##
InstallMethod( Display,
		"for action association schemes",
		[ IsAssociationSchemeByActionRep ],

 function( ascheme )

  Print( "The association scheme for the action of " );
  Display( GroupOfAssociationScheme( ascheme ) );
  Print( " on " );
  Display( GroundSet( ascheme ) );
  Print( " via ", ActionOfAssociationScheme( ascheme ), "." );

 end

);

##
InstallMethod( Display,
		"for relation association schemes",
		[ IsAssociationSchemeByRelationsRep ],

 function( ascheme )

  Print( "The association scheme on " );
  Display( GroundSet( ascheme ) );
  Print( " with relations defined by " );
  Display( RelationsOfAssociationScheme( ascheme ) );

 end

);

##
InstallMethod( Display,
		"for function association schemes",
		[ IsAssociationSchemeByFunctionsRep  ],

 function( ascheme )

  Print( "The association scheme on " );
  Display( GroundSet( ascheme ) );
  Print( " with relations defined by " );
  Display( RelationsOfAssociationScheme( ascheme ) );

 end

);

