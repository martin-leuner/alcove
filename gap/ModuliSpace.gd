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
