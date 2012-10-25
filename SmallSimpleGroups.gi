#InterestingList := Filtered( [ 2 .. 1000 ], IsEvenInt );
InterestingList := [ 2 .. 1000 ];
InterestingList := Filtered( InterestingList, i -> not IsPrimePowerInt(i) );
InterestingList := Filtered( InterestingList, i -> Size( DuplicateFreeList( FactorsInt( i ) ) ) > 2 );

SylowYieldsNormal := function( n )
 local PrimeFactors, PossSylNumbers, CurrFac, OtherProd;
 PrimeFactors := Collected( FactorsInt( n ) );
 for CurrFac in PrimeFactors do
  OtherProd := n / ( CurrFac[1] ^ CurrFac[2] );
  PossSylNumbers := Filtered( List( [ 0 .. Int( OtherProd / CurrFac[1] ) ], i -> 1 + CurrFac[1] * i ), i -> IsInt( OtherProd / i ) );
  if( PossSylNumbers = [ 1 ] ) then
   return true;
  fi;
 od;
 return false;
end;

InterestingList := Filtered( InterestingList, i -> not SylowYieldsNormal(i) );
