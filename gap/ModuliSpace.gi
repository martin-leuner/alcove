#
# alcove: Moduli space
#
# Implementations
#

##
InstallMethod( MatrixForMatroidRepresentation,
        "for a matroid and a homalg ring",
        [ IsMatroid, IsHomalgRing ],
        
  function( matroid, homalgRing )
    local circs, basis, nonIdCols, rk, mat, row, col, n, varNum, c, circ, var, polyRing;
    
    if not IsEmpty( Loops( matroid ) ) then
        Error( "matroid must not have loops\n" );
    fi;
    
    if IsVectorMatroidRep( matroid ) and
       Characteristic( HomalgRing( matroid ) ) <> Characteristic( homalgRing ) then #or
        #DegreeOverPrimeField( HomalgRing( matroid ) ) <> DegreeOverPrimeField( homalgRing ) then
        
        Error( "incompatible fields\n" );
        
    fi;
    
    # prepare the combinatorial data
    
    circs := FundamentalCircuitsWithBasis( matroid );
    basis := circs[2];
    circs := circs[1];
    
    nonIdCols := Difference( GroundSet( matroid ), basis );
    circs := List( circs, c -> Difference( c, nonIdCols ) );
    
    rk := Rank( matroid );
    
    # create the matrix and fill it with entries
    
    mat := List( [ 1 .. rk ], i -> [ ] );
    
    for row in [ 1 .. rk ] do
        for col in [ 1 .. rk ] do
            
            if row = col then
                mat[ row ][ basis[col] ] := "1";
            else
                mat[ row ][ basis[col] ] := "0";
            fi;
            
        od;
    od;
    
    n := Size( matroid );
    
    varNum := 0;
    
    for col in [ 1 .. n - rk ] do
        for row in [ 1 .. rk ] do
            
            circ := circs[col];
            c := nonIdCols[col];
            
            if basis[row] = Minimum( circ ) then
                
                mat[ row ][ c ] := "1";
                
            elif basis[row] in circ then
                
                varNum := varNum + 1;
                mat[ row ][ c ] := Concatenation( "a", String( varNum ) );
                    
            else
                
                mat[ row ][ c ] := "0";
                
            fi;
            
        od;
    od;
    
    mat := Concatenation( "[",
                   JoinStringsWithSeparator( Concatenation( mat ) ),
                   "]" );
    
    # create the polynomial ring
    var := List( [ 1 .. varNum ], i -> Concatenation( "a", String( i ) ) );
    
    polyRing := homalgRing * var;
    
    return HomalgMatrix( mat, rk, n, polyRing );
    
end );
