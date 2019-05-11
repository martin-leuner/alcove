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
    local circs, basis, nonIdCols, rk, unUsedRowsForOnes, mat, row, col, n, varNum, c, circ, var, polyRing;
    
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
    unUsedRowsForOnes := [ 2 .. rk ];
    
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
                
                if row in unUsedRowsForOnes then
                    
                    mat[ row ][ c ] := "1";
                    unUsedRowsForOnes := Difference( unUsedRowsForOnes, [ row ]);
                    
                else
                    
                    varNum := varNum + 1;
                    mat[ row ][ c ] := Concatenation( "a", String( varNum ) );
                    
                fi;
                
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

##
InstallMethod( EquationsAndInequationsOfModuliSpaceOfMatroid,
        "for a matroid and a homalg ring",
        [ IsMatroid, IsHomalgRing ],
        
  function( matroid, homalgRing )
    local m, eqs, ineqs, bases, col, det, R;
    
    m := MatrixForMatroidRepresentation( matroid, homalgRing );
    
    # write down equations and inequations
    
    eqs := [ ];
    ineqs := [ ];
    bases := Bases( matroid );
    
    for col in IteratorOfCombinations( GroundSet( matroid ), Rank( matroid ) ) do
        
        det := DeterminantMat( CertainColumns( m, col ) );
        
        if Degree( det ) <= 0 then
            
            if not IsZero( det ) and col in bases then
                if IsUnit( det ) then
                    continue;
                fi;
            elif not IsZero( det ) then
                Error( "determinant nonzero but set not a basis\n" );
            elif col in bases then
                Error( "determinant zero but set is a basis\n" );
            else
                continue;
            fi;
            
        fi;
        
        if Set( col ) in bases then
            AddSet( ineqs, det );
        else
            AddSet( eqs, det );
        fi;
        
    od;
    
    return [ eqs, ineqs, HomalgRing( m ), Matroid( m ) ];
    
end );
