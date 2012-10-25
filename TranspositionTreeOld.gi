

#####
# UpperMulBoundLeqN returns a multiplicative upper bound for the order
# of all elements without p^k cycles of S_n, n <= N
#

UpperMulBoundLeqN := function( N, p )
 local k, L, r, BigPrimes;

 if( N > 1008 ) then		# prepare prime list for big N
  L := AsList(Primes);
  L := Union2( L, Filtered( [1009..N], IsPrimeInt ) );	# questionable...
  BigPrimes := ShallowCopy( L );
 else				# prepare prime list for small N
  k := Maximum( Int(N/6), 1 );
  N := Minimum( N, 997 );	# NOTE: There are no prime powers between 997 and 1009
  if( Primes[k] > N ) then
   while( Primes[k] > N ) do
    k := k - 5;
   od;
  else
   while( Primes[k] < N ) do
    k := Minimum( 168, k+5 );
   od;
  fi;
  while( Primes[k] > N ) do
   k := k-1;
  od;
  while( Primes[k] < N ) do
   k := k+1;
  od;
  L := List( [1..k], i -> Primes[i] );
  BigPrimes := Primes;
 fi;				# prepared prime lists

 for r in [1..Size(L)] do	# raise to powers <= N
  if( L[r] <> p) then
   while( L[r] <= N ) do
    L[r] := L[r] * BigPrimes[r];
   od;
  fi;
  L[r] := L[r] / BigPrimes[r];
 od;

 return Product(L);
end;;


#####
# ConstructTransposition returns a possible transposition (chance 1-epsilon)
#

ConstructTransposition := function( G, N, epsilon, MulBound )
 local MaxTries, TotalTests, Candidate, Tests, Rand, RandSquared;
 MaxTries := 100;	# REPLACE: calculate bound depending on N, epsilon
 TotalTests := 100;	# REPLACE: calculate bound depending on N, epsilon
 while( MaxTries > 0 ) do
  Candidate := PseudoRandom( G )^MulBound;	# choose random element, discard cycles of length <> 2^k
  if( Candidate <> One(G) and Candidate^2 = One(G) ) then	# candidate is a product of transpositions,
								# commence testing for transposition
   Tests := TotalTests;
   while( Tests > 0 ) do
    Rand := Comm( Candidate, PseudoRandom( G ) );
    if( Rand <> One(G) ) then			# commutator not 1
     RandSquared := Rand*Rand;
     if( RandSquared <> One(G) ) then		# commutator not of order 1 or 2
      if( Rand*RandSquared <> One(G) ) then	# commutator not of order 1, 2 or 3
       break;
      fi;
     fi;
    fi;
    Tests := Tests - 1;		# passed one more test, lower remaining number
   od;
   if( Tests = 0 ) then		# all tests successfully passed
    return Candidate;
   fi;
  fi;
  MaxTries := MaxTries - 1;	# had another unsuccessful try at finding a transposition, lower remaining number
 od;				# main searching loop closes
# SET INFO RECORD
 return fail;			# no transposition found within given boundary
end;;


#####
# ConstructLongCycle returns an element containing a relatively long cycle of the
# form (1,2,...,k) where (1,2) is the provided transposition along with the
# conjugates of said transposition under said cycle
# 
# This first version proceeds in the aforementioned order, another approach would be
# to generate random elements and conjugate the transposition a few times to check
# whether other transpositions match them better, i.e. their moved points lie in longer cycles
#
# With a chance of at least 1-epsilon the resulting cycle is of length at least n/2
#

ConstructLongCycle := function( G, N, epsilon, Transposition )
 local NumberOfCandidates, SearchTries, RemainingTries, CandidateList, CurrentCandidate, Commutator, i, ElementWithLongestCycle, CurrentCycle, LongestCycle, CurrentConjugate, StartTime;

StartTime := Runtime();
 NumberOfCandidates := Int( 5/2*Log2Int( Int(1/epsilon) + 1 ) ) + 1;	# bound for k >= 3/4*n
 SearchTries := Int( N/2*( Int( 7/10*( Log2Int( Int(1/epsilon) + 1 ) + 1 ) ) + 1 ) ) + 1;
 CandidateList := [];

 while( NumberOfCandidates - Size(CandidateList) > 0 ) do
  RemainingTries := SearchTries;
  while( RemainingTries > 0 ) do
   CurrentCandidate := PseudoRandom(G);
   Commutator := Comm( CurrentCandidate, Transposition );
   if( Commutator <> One(G) and Commutator^3 = One(G) ) then	# commutator is of order 3
    Add(CandidateList,CurrentCandidate);
    break;
   else								# commutator not of order 3
    RemainingTries := RemainingTries - 1;
   fi;
  od;
  if( RemainingTries = 0 ) then					# no matching element found within given boundary
# SET INFO RECORD
   return fail;
  fi;
 od;

#Print( "Constructed candidates, this took ", Runtime()-StartTime, "ms.\n" );
StartTime := Runtime();

 for i in [1..NumberOfCandidates] do	# check whether canditates fix one of transposition's moved points
  if( Comm( Transposition, CandidateList[i]^2 )^2 <> One(G) ) then
   CandidateList[i] := CandidateList[i]*Transposition;
  fi;
 od;					# now all candidates should be of the form (1,2,...,k_i)(j,...) (wlog)
  
#Print( "Edited candidates, this took ", Runtime()-StartTime, "ms.\n" );
StartTime := Runtime();

# LongestConjugateList := [];
# for i in CandidateList do	# find longest cycle by repeatedly conjugating transposition with candidates
#  ConjugateList := [Transposition];
#  CurrentConjugate := Transposition^i;
#  while( CurrentConjugate <> Transposition ) do
#   Add( ConjugateList, CurrentConjugate );
#   CurrentConjugate := CurrentConjugate^i;
#  od;
#  if( Size(ConjugateList) > Size(LongestConjugateList) ) then
#   LongestConjugateList := ConjugateList;	# probably no ShallowCopy needed here, but watch this...
#   ElementWithLongestCycle := i;
#  fi;
# od;
# if( Size(LongestConjugateList) > 2 ) then	# last conjugate intersects with original transposition
#  Remove( LongestConjugateList, Size(LongestConjugateList) );
# elif( Size(LongestConjugateList) = 2 ) then	# treat a minor special case
#  ElementWithLongestCycle := ElementWithLongestCycle*Transposition;
#  LongestConjugateList[2] := Transposition^ElementWithLongestCycle;
# fi;


 LongestCycle := [2,Transposition];
 for i in CandidateList do	# find longest cycle by repeatedly conjugating transposition with candidates
  CurrentCycle := [2,Transposition];
  CurrentConjugate := Transposition^i;
  while( CurrentConjugate <> Transposition ) do
   CurrentCycle[1] := CurrentCycle[1] + 1;
   CurrentCycle[2] := CurrentConjugate * CurrentCycle[2];
   CurrentConjugate := CurrentConjugate^i;
  od;
  if( CurrentCycle[1] > LongestCycle[1] ) then
   LongestCycle := CurrentCycle;	# probably no ShallowCopy needed here, but watch this...
   ElementWithLongestCycle := i;
  fi;
 od;
 if( LongestCycle[1] > 2 ) then		# last conjugate intersects with original transposition
  LongestCycle[1] := LongestCycle[1] - 1;
  LongestCycle[2] := ( Transposition ^ ( ElementWithLongestCycle ^ (-1) ) ) * LongestCycle[2];
 fi;


#Print( "Found longest cycle and constructed conjugates, this took ", Runtime()-StartTime, "ms.\n" );

 return LongestCycle;
end;;


#####
# ConstructNiceGeneratorsSn returns elements satisfying the S_n-presentation
#

ConstructNiceGeneratorsSn := function( G, N, epsilon )
 local EnlargeCycles, LongCycle, LengthOfCycle, CheckingCycle, GeneratingCycle, Transposition, MulBound,
	CurrentNumberOfPoints, i, RemainingSteps, NewTranspositions, ConjugatedTransposition,
	RandomLongCycle, FindFixedAndMovedPoints, ExchangingElement, LastTransposition, MatchRandomLongCycle;

 # MatchRandomLongCycle sets RandomLongCycle to a random conjugate of LongCycle fixing k and not fixing k-1
 MatchRandomLongCycle := function()
  local Points;
  RandomLongCycle := LongCycle ^ PseudoRandom( G );
  Points := FindFixedAndMovedPoints();
  if( Points = fail ) then				# conjugate of LongCycle consists of known points
   return( fail );
  fi;

  if( Points[1] = CurrentNumberOfPoints ) then		# k fixed
   if( Points[2] = CurrentNumberOfPoints-1 ) then	# k-1 not fixed
    ExchangingElement := One( G );
   else							# k-1 fixed
    ExchangingElement := LastTransposition ^ ( ( CheckingCycle ^ Points[2] ) * LastTransposition );
   fi;
  else							# k not fixed
   if( Points[2] = CurrentNumberOfPoints-1 ) then	# k-1 not fixed
    ExchangingElement := LastTransposition ^ ( CheckingCycle ^ Points[1] );
   else							# k-1 fixed
    ExchangingElement := LastTransposition;
   fi;
  fi;

  return( RandomLongCycle ^ ExchangingElement );
 end;

 # EnlargeCycles uses the matched RandomLongCycle to add new points to GeneratingCycle, CheckingCycle and to update CurrentNumberOfPoints
 EnlargeCycles := function()
  ConjugatedTransposition := LastTransposition;
  for i in [ 1 .. LengthOfCycle - 1 ] do
   ConjugatedTransposition := ConjugatedTransposition ^ RandomLongCycle;
   if( Comm( ConjugatedTransposition, CheckingCycle ) = One( G ) ) then		# ConjugatedTransposition moves a hitherto fixed point
    GeneratingCycle := ConjugatedTransposition * GeneratingCycle;
    CurrentNumberOfPoints := CurrentNumberOfPoints + 1;
    RandomLongCycle := RandomLongCycle ^ ConjugatedTransposition;
    LastTransposition := ConjugatedTransposition;
   fi;
  od;
  CheckingCycle := LastTransposition * GeneratingCycle;
 end;

 # FindFixedAndMovedPoints returns [1] a point fixed by RandomLongCycle and [2] a point not fixed by RandomLongCycle
 # if k is fixed, [1] must be k
 # if k-1 is not fixed, [2] must be k-1
# FindFixedAndMovedPoints := function()
#  local Result, TestComm, TempConjugate
#  Result := [,];
#
#  TestComm := Comm( LastTransposition, RandomLongCycle );
#  if( TestComm = One( G ) ) then					# k-1, k fixed			!!!!! ASSUMING RandomLongCycle IS NOT A TRANSPOSITION !!!!!
#   Result[1] := CurrentNumberOfPoints;
#  elif( TestComm ^ 2 = One( G ) ) then					# k-1, k not fixed
#   Result[2] := CurrentNumberOfPoints - 1;
#  else									# |[ RLC, LT ]| = 3, additional tests necessary
#   TempConjugate := LastTransposition ^ GeneratingCycle;		# (1,k)
#   TestComm := Comm( TempConjugate, RandomLongCycle );
#   if( TestComm = One( G ) ) then					# 1, k fixed => k-1 not fixed
#    return( [ CurrentNumberOfPoints, CurrentNumberOfPoints - 1 ] );
#   elif( TestComm ^ 2 = One( G ) ) then					# 1, k not fixed
#   
# end;


 FindFixedAndMovedPoints := function()
  local IsFixedPoint, Pred, Succ, Succ2, PredFixed, LastFixed;

if( LengthOfCycle < 7 ) then			# !!!!! might be a problem !!!!!
 Error( "LengthOfCycle < 7" );
fi;

  IsFixedPoint := function()
   local TmpConjugate, TempElement, Check2, Check3, Check4, Check5, Counter;
   TmpConjugate := Succ ^ RandomLongCycle;
   TempElement := TmpConjugate * Pred;

   if( TempElement = One( G ) ) then
    return( ( Succ * ( Pred ^ RandomLongCycle ) ) ^ 2 <> One( G ) );
   elif( TempElement ^ 2 = One( G ) ) then
    return( false );
   else
    TmpConjugate := Pred ^ ( RandomLongCycle ^ (-1) );

    Counter := 0;
    Check2 := Succ2;
    TempElement := GeneratingCycle * Succ2;
    Check3 := Check2 ^ TempElement;
    Check4 := Check3 ^ TempElement;
    Check5 := Check4 ^ TempElement;
    if( Check2 * TmpConjugate <> TmpConjugate * Check2 ) then
     Counter := Counter + 1;
    fi;
    if( Check3 * TmpConjugate <> TmpConjugate * Check3 ) then
     Counter := Counter + 1;
    fi;
    if( Check4 * TmpConjugate <> TmpConjugate * Check4 ) then
     Counter := Counter + 1;
    fi;
    if( Check5 * TmpConjugate <> TmpConjugate * Check5 ) then
     Counter := Counter + 1;
    fi;
    if( Counter >= 3 ) then
     return( false );
    fi;

    TmpConjugate := Pred ^ ( GeneratingCycle ^ (-1) * RandomLongCycle ^ (-1) );
    Counter := 0;
    Check2 := Check2 ^ Succ;
    Check3 := Check3 ^ Succ;
    Check4 := Check4 ^ Succ;
    Check5 := Check5 ^ Succ;
    if( Check2 * TmpConjugate <> TmpConjugate * Check2 ) then
     Counter := Counter + 1;
    fi;
    if( Check3 * TmpConjugate <> TmpConjugate * Check3 ) then
     Counter := Counter + 1;
    fi;
    if( Check4 * TmpConjugate <> TmpConjugate * Check4 ) then
     Counter := Counter + 1;
    fi;
    if( Check5 * TmpConjugate <> TmpConjugate * Check5 ) then
     Counter := Counter + 1;
    fi;
    if( Counter >= 3 ) then
     return( false );
    fi;

    return( true );

   fi;
  end;

#  Succ := LastTransposition ^ ( GeneratingCycle ^ (-1) );
#  Pred := Succ ^ ( GeneratingCycle ^ (-1) );
#  Succ2 := LastTransposition;
  Succ := LastTransposition;
  Pred := Succ ^ ( GeneratingCycle ^ (-1) );
  Succ2 := Succ ^ GeneratingCycle;
  PredFixed := IsFixedPoint();
  Pred := Succ;
  Succ := Succ2;
  Succ2 := Succ2 ^ GeneratingCycle;
  LastFixed := IsFixedPoint();

  if( LastFixed <> PredFixed ) then
   if( LastFixed ) then
    return( [ CurrentNumberOfPoints, CurrentNumberOfPoints-1 ] );
   else
    return( [ CurrentNumberOfPoints-1, CurrentNumberOfPoints ] );
   fi;
  else
   for i in [ 1 .. CurrentNumberOfPoints - 2 ] do
    Pred := Succ;
    Succ := Succ2;
    Succ2 := Succ2 ^ GeneratingCycle;
    if( IsFixedPoint() <> LastFixed ) then
     if( LastFixed ) then
      return( [ CurrentNumberOfPoints, i ] );
     else
      return( [ i, CurrentNumberOfPoints-1 ] );
     fi;
    fi;
   od;
  fi;

  return( fail );
 end;;

# MulBound := UpperMulBoundLeqN( N, 2 );
# Transposition := ConstructTransposition( G, N, epsilon, MulBound );
# LongCycle := ConstructLongCycle( G, N, epsilon, Transposition );
# LengthOfCycle := Size( LongCycle ) - ( Size( LongCycle ) mod 3 );
# CurrentNumberOfPoints := LengthOfCycle;
# RemainingSteps := Int( 1/2*( Log2Int(N) + Log2Int( Int( 1/epsilon ) + 1 ) ) );	# bound for k >= 3/4*n, n >= 4
#
# # construct (1,...,k) and (1,...,k-1) from list of transpositions returned by ConstructLongCycle
# CheckingCycle := One(G);
# for i in [ 1 .. ( CurrentNumberOfPoints - 1 ) ] do
#  CheckingCycle := LongCycle[i] * CheckingCycle;
# od;
# LastTransposition := LongCycle[ CurrentNumberOfPoints ];
# LongCycle := LastTransposition * CheckingCycle;
# GeneratingCycle := LongCycle;
# CurrentNumberOfPoints := CurrentNumberOfPoints + 1;


 MulBound := UpperMulBoundLeqN( N, 2 );
 Transposition := ConstructTransposition( G, N, epsilon, MulBound );
 if( Transposition = fail ) then
  return fail;
 fi;
 LongCycle := ConstructLongCycle( G, N, epsilon, Transposition );
 if( LongCycle = fail ) then
  return fail;
 fi;
 LengthOfCycle := LongCycle[1];
 CurrentNumberOfPoints := LengthOfCycle;
 RemainingSteps := Int( 1/2*( Log2Int(N) + Log2Int( Int( 1/epsilon ) + 1 ) ) );		# bound for k >= 3/4*n, n >= 4
 LongCycle := LongCycle[2];
 GeneratingCycle := LongCycle;
 LastTransposition := Transposition ^ ( GeneratingCycle ^ (-2) );
 CheckingCycle := LastTransposition * GeneratingCycle;


 while( RemainingSteps > 0 ) do			# main loop

#  XiTable := ConstructXiSn( CurrentNumberOfPoints, GeneratingCycle, Transposition );
#  RandomLongCycle := LongCycle ^ PseudoRandom(G);
#
#  Points := FindFixedAndMovedPoints();		# IMPORTANT: CATCH FAIL RETURN VALUE
#  ExchangingElement := ();
#  if( Points[1] <> CurrentNumberOfPoints ) then		# k not fixed
#   if( Points[2] <> CurrentNumberOfPoints-1 ) then	# k-1 fixed, use LastTransposition to switch k and k-1
#    ExchangingElement := fail;
#   else							# k-1 not fixed
#    ExchangingElement := ( Points[1], CurrentNumberOfPoints );
#   fi;
#  else							# k fixed
#   if( Points[2] <> CurrentNumberOfPoints-1 ) then	# k-1 fixed
#    ExchangingElement := ( Points[2], CurrentNumberOfPoints-1 );
#   fi;							# else k-1 not fixed, no conjugation necessary
#  fi;
#  if( ExchangingElement = fail ) then
#   ExchangingElement := LastTransposition;
#  elif( ExchangingElement = () ) then
#   ExchangingElement := One(G);
#  else
#   ExchangingElement := SLPforSn( CurrentNumberOfPoints, ExchangingElement );
#   ExchangingElement := ResultOfStraightLineProgram( ExchangingElement, [ Transposition, GeneratingCycle ] );
#  fi;
#  RandomLongCycle := RandomLongCycle ^ ExchangingElement;	# RandomLongCycle now should fix k and not fix k-1

  RandomLongCycle := MatchRandomLongCycle();

  if( RandomLongCycle <> fail ) then
   EnlargeCycles();
  fi;

  RemainingSteps := RemainingSteps - 1;

 od;

 return( [ GeneratingCycle, Transposition ] );
end;;


