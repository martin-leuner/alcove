#####
# RepresentationElementarySymmetricPolynomials expresses a symmetric polynomial in the elementary symmetrics polynomials
# Input: one polynomial
#

RepresentationElementarySymmetricPolynomials := function( p )
 local Vars, ReducePolynomial, L, Mon, ESP, NewTerm;
 if not IsPolynomial(p) then
  Error( "bad input, please provide a polynomial" );
 fi;
 L := [];
 Vars := IndeterminatesOfPolynomialRing( DefaultRing( p ) );

 ESP := function( i )
  return( Sum( List( Combinations(Vars,i), Product ) ) );
 end;

 ReducePolynomial := function( pol )
  local LM, CurrProd, CurrRedPol;
  LM := ShallowCopy( LeadingMonomial( pol ) ); # GAP uses deglex here, should work fine
  CurrProd := One( p );
  CurrRedPol := One( p );
  while Size( LM ) > 2 do
   if( LM[3] - LM[1] > 1 ) then # an indeterminate is missing in the LM, impossible for symmetric polynomials
    Print( "Input was not a symmetric polynomial.\n" );
    return fail;
   fi;
   CurrRedPol := CurrRedPol * ESP( LM[1] ) ^ ( LM[2] - LM[4] );
   CurrProd := CurrProd * Vars[ Remove(LM,1) ] ^ ( Remove(LM,1) - LM[2] );
  od;
  CurrRedPol := CurrRedPol * ESP( LM[1] )^LM[2];
  CurrProd := CurrProd * Vars[ LM[1] ]^LM[2];
  p := p - LeadingCoefficient(pol)*CurrRedPol;
  return LeadingCoefficient(pol)*CurrProd;
 end;

 while p <> Zero(p) do
  NewTerm := ReducePolynomial(p);
  if NewTerm <> fail then
   AddSet( L, NewTerm );
  else
   return fail;
  fi;
 od;

 return L;
end;

PrintRepElSymm := function( L )
 local Mon, LM;
 for Mon in L do
  Print( LeadingCoefficient(Mon), "*" );
  LM := ShallowCopy( LeadingMonomial(Mon) );
  while not IsEmpty(LM) do
   Print( "s_", Remove(LM,1), "^", Remove(LM,1) );
   if not IsEmpty(LM)  then Print("*"); fi;
  od;
  if Mon <> L[Size(L)] then
   Print( " + " );
  else
   Print( "\n" );
  fi;
 od;
end;


R := PolynomialRing(Rationals,3);;
x1 := R.1;; x2 := R.2;; x3 := R.3;;
f := x1^3*x2+x1*x2^3+x1^3*x3+x1*x3^3+x2*x3^3+x2^3*x3;;

Print( "Zerlegung von f = ", f, ":\n" );
PrintRepElSymm( RepresentationElementarySymmetricPolynomials( f ) );


# Ergibt folgenden Output:

# gap> Read( "ElemSymmPol.gi" );
# Zerlegung von f = x_1^3*x_2+x_1^3*x_3+x_1*x_2^3+x_1*x_3^3+x_2^3*x_3+x_2*x_3^3:
# -2*s_2^2 + -1*s_1^1*s_3^1 + 1*s_1^2*s_2^1

