#
# alcove: Moduli space
#
# Declarations
#

#! @Description
#!  Compute a general matrix of indeterminates over the ring <A>R</A>
#!  which potentially represents the given <A>matroid</A> as a vector matroid.
#! @Arguments matroid, R
#! @Returns a list of listlists
DeclareOperation( "MatrixForMatroidRepresentation",
        [ IsMatroid, IsHomalgRing ] );

#! @Description
#!  Compute the list of equations and inequations defining
#!  the possibly empty moduli space of vector matroids of the given <A>matroid</A>
#!  over the ring <A>R</A>.
#!  This is a quasi-affine representation of the (abstractly) affine moduli space.
#! @Arguments matroid, R
#! @Returns a list of listlists
DeclareOperation( "ModuliSpaceOfMatroidByEquationsAndInequations",
        [ IsMatroid, IsHomalgRing ] );
