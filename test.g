LoadPackage( "alcove" );

f3 := HomalgRingOfIntegers( 3 );
ex1 := Matroid( HomalgMatrix(
                  [ [ Z(3)^0, 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0, Z(3), Z(3), Z(3)^0, 0*Z(3) ],
                    [ 0*Z(3), Z(3)^0, Z(3), 0*Z(3), Z(3)^0, Z(3), 0*Z(3), Z(3), Z(3)^0 ],
                    [ 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0, Z(3)^0, Z(3)^0, Z(3)^0, Z(3)^0, Z(3)^0 ] ],
                  f3 )
              );

x := IndeterminatesOfTuttePolynomial()[1];
y := IndeterminatesOfTuttePolynomial()[2];

Assert( 0, TuttePolynomial(ex1) = y^6+3*y^5+6*y^4+x^3+10*y^3+6*x^2+12*x*y+15*y^2+9*x+9*y, "ex1: incorrect Tutte polynomial" );

Assert( 0, IsConnected(ex1), "ex1: did not recognise connectivity" );


# check for errors in vector matroids

for i in [ 1 .. 100 ] do
  m := RandomVectorMatroidOverPrimeField( Random([2..5]), Random([4..8]), Random([0,2,3,7]) );

  DualMatroid(m);

  TuttePolynomial(m);

  IsConnected(m);

  IsUniform(m);

  Restriction(m,[1,2]);

  Contraction(m,[1,2]);

  IndependenceOracle(m)([1,2]);

  RankFunction(m)([1,2]);

  f := ClosureOperator(m)([1,2]);

  EssentialityOperator(m)(f);

  if not IsEmpty( Loops(m) ) or not IsEmpty( NonTrivialParallelClasses(m) ) then
    Assert( 0, Size( Simplification(m)[1] ) < Size( m ), "simplification failed" );
  fi;
od;


Print( "# Finished quick test run.\n" );
