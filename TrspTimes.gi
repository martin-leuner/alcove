

#####
# UpperMulBoundLeqN returns a multiplicative upper bound for the order
# of all elements without p^k cycles of S_n, n <= N
#

UpperMulBoundLeqN := function( N, p )
 local k, L, r, BigPrimes;

 L := Filtered( [ 1 .. N ], IsPrimeInt );
 BigPrimes := ShallowCopy(L);

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
 MaxTries := 100;	# REPLACE: calculate bound depending on N, epsilon								!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 TotalTests := 100;	# REPLACE: calculate bound depending on N, epsilon								!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Print( "# Commencing construction of the first transposition.\n" );
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
 local RemainingCandidates, SearchTries, RemainingTries, CurrentCandidate, Commutator, i, ElementWithLongestCycle, CurrentLength, GreatestLength, LongestCycle, CurrentConjugate;

 RemainingCandidates := Int( 5/2*Log2Int( Int(1/epsilon) + 1 ) ) + 1;	# bound for k >= 3/4*n
 SearchTries := Int( N/2*( Int( 7/10*( Log2Int( Int(1/epsilon) + 1 ) + 1 ) ) + 1 ) ) + 1;
Print( "# Commencing construction of long cycle, will create ", RemainingCandidates, " elements and search at most ", SearchTries, " times for each.\n" );
 GreatestLength := 2;

 while( RemainingCandidates > 0 ) do
  RemainingTries := SearchTries;
  while( RemainingTries > 0 ) do
   CurrentCandidate := PseudoRandom(G);
   Commutator := Comm( CurrentCandidate, Transposition );
   if( Commutator <> One(G) and Commutator^3 = One(G) ) then	# commutator is of order 3
    if( Comm( Transposition, CurrentCandidate^2 )^2 <> One(G) ) then
     CurrentCandidate := CurrentCandidate*Transposition;
    fi;								# now candidate has form (1,2,...,k)(..)(..)..
    CurrentConjugate := Transposition ^ CurrentCandidate;	# determine length of cycle containing (1,2,...
    CurrentLength := 1;
    while( CurrentConjugate <> Transposition ) do
     CurrentLength := CurrentLength + 1;
     CurrentConjugate := CurrentConjugate ^ CurrentCandidate;
    od;
    if( CurrentLength > GreatestLength ) then			# if longer cycle has been found, save corresponding candidate
     GreatestLength := CurrentLength;
     ElementWithLongestCycle := CurrentCandidate;
    fi;
    break;
   else								# commutator not of order 3
    RemainingTries := RemainingTries - 1;
   fi;
  od;
  if( RemainingTries = 0 ) then					# no matching element found within given boundary
# SET INFO RECORD
   return fail;
  fi;
  RemainingCandidates := RemainingCandidates - 1;
 od;

 if( GreatestLength = 2 ) then
# SET INFO RECORD
  return fail;
 fi;

 LongestCycle := Transposition;					# compute long cycle
 CurrentConjugate := Transposition ^ ElementWithLongestCycle;
 for i in [ 1 .. GreatestLength - 2 ] do
  LongestCycle := CurrentConjugate * LongestCycle;
  CurrentConjugate := CurrentConjugate ^ ElementWithLongestCycle;
 od;

 return [ GreatestLength, LongestCycle ];
end;;


#####
# ConstructNiceGeneratorsSn returns elements satisfying the S_n-presentation
#

ConstructNiceGeneratorsSn := function( G, N, epsilon )
 local 	EnlargeCycles, LongCycle, LengthOfCycle, CheckingCycle, GeneratingCycle, Transposition, MulBound,
	CurrentNumberOfPoints, i, RemainingSteps, ConjugatedTransposition, RandomLongCycle, FindFixedAndMovedPoints,
	ExchangingElement, LastTransposition, MatchRandomLongCycle,
	MarkedTime, ModCycleTime, TotalMatchTime, DecideFixedTime;

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
   if( ConjugatedTransposition * CheckingCycle = CheckingCycle * ConjugatedTransposition ) then		# ConjugatedTransposition moves a hitherto fixed point
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
 FindFixedAndMovedPoints := function()
  local IsFixedPoint, Pred, Succ, Succ2, PredFixed, LastFixed;

if( LengthOfCycle < 7 ) then			# !!!!! might be a problem !!!!!						!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 Error( "LengthOfCycle < 7" );
fi;

  IsFixedPoint := function()
   local TmpConjugate, TempElement, Check2, Check3, Check4, Check5, Counter, LocalMarkedTime;
LocalMarkedTime := Runtime();
   TmpConjugate := Succ ^ RandomLongCycle;
   TempElement := TmpConjugate * Pred;

   if( TempElement = One( G ) ) then
DecideFixedTime := DecideFixedTime + Runtime() - LocalMarkedTime;
    return( ( Succ * ( Pred ^ RandomLongCycle ) ) ^ 2 <> One( G ) );
   elif( TempElement ^ 2 = One( G ) ) then
DecideFixedTime := DecideFixedTime + Runtime() - LocalMarkedTime;
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
DecideFixedTime := DecideFixedTime + Runtime() - LocalMarkedTime;
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
DecideFixedTime := DecideFixedTime + Runtime() - LocalMarkedTime;
     return( false );
    fi;

DecideFixedTime := DecideFixedTime + Runtime() - LocalMarkedTime;
    return( true );

   fi;
  end;

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

 # local functions defined, initialize main function
 MulBound := UpperMulBoundLeqN( N, 2 );
MarkedTime := Runtime();
 Transposition := ConstructTransposition( G, N, epsilon, MulBound );
Print( "# Time needed to construct transposition: ", Runtime() - MarkedTime, "ms.\n" );
 if( Transposition = fail ) then
  return fail;
 fi;
MarkedTime := Runtime();
 LongCycle := ConstructLongCycle( G, N, epsilon, Transposition );
Print( "# Time needed to construct long cycle: ", Runtime() - MarkedTime, "ms.\n" );
 if( LongCycle = fail ) then
  return fail;
 fi;
 LengthOfCycle := LongCycle[1];
 CurrentNumberOfPoints := LengthOfCycle;
 RemainingSteps := Int( 1/2*( Log2Int(N) + Log2Int( Int( 1/epsilon ) + 1 ) ) );		# bound for k >= 3/4*n, n >= 4
Print( "# Commencing main loop, this will run ", RemainingSteps, " times.\n" );
 LongCycle := LongCycle[2];
 GeneratingCycle := LongCycle;
 LastTransposition := Transposition ^ ( GeneratingCycle ^ (-2) );
 CheckingCycle := LastTransposition * GeneratingCycle;

ModCycleTime := 0;
TotalMatchTime := 0;
DecideFixedTime := 0;
 while( RemainingSteps > 0 ) do			# main loop
MarkedTime := Runtime();
  RandomLongCycle := MatchRandomLongCycle();
TotalMatchTime := TotalMatchTime + Runtime() - MarkedTime;
  if( RandomLongCycle <> fail ) then
MarkedTime := Runtime();
   EnlargeCycles();
ModCycleTime := ModCycleTime + Runtime() - MarkedTime;
  fi;
  RemainingSteps := RemainingSteps - 1;
 od;

Print( "# Time spent on modifying the generating cycles: ", ModCycleTime, "ms.\n" );
Print( "# Total time spent on matching the random long cycle: ", TotalMatchTime, "ms.\n" );
Print( "# Time spent on deciding whether a point is fixed: ", DecideFixedTime, "ms.\n" );
 return( [ GeneratingCycle, Transposition ] );
end;;


