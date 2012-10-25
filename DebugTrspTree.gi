################################
## DEBUG IsFixedPoint
if( IsBound(DebugIsFixedPoint) and DebugIsFixedPoint ) then
################################
G := SymmetricGroup( 30 );
LongCycle := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22);
GeneratingCycle := LongCycle;
RandomLongCycle := LongCycle ^ PseudoRandom( G );
Succ := (21,22);
Pred := (20,21);
Succ2 := (22,1);

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
 end;;
 
##################################################
fi;
# finished IsFixedPoint
##################################################


################################
## DEBUG FindFixedAndMovedPoints
if( IsBound(DebugFindPoints) and DebugFindPoints ) then
################################
LengthOfCycle := 22;
G := SymmetricGroup( 30 );
LongCycle := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22);
RandomLongCycle := LongCycle;
GeneratingCycle := (23,24)*(22,23)*LongCycle;
CurrentNumberOfPoints := 24;
LastTransposition := (23,24);
i := 0;

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

  Succ := LastTransposition;
  Pred := Succ ^ ( GeneratingCycle ^ (-1) );
  Succ2 := Succ ^ GeneratingCycle;
  PredFixed := IsFixedPoint();
Error();
  Pred := Succ;
  Succ := Succ2;
  Succ2 := Succ2 ^ GeneratingCycle;
  LastFixed := IsFixedPoint();
Error();

  if( LastFixed <> PredFixed ) then
   if( LastFixed ) then
Error();
    return( [ CurrentNumberOfPoints, CurrentNumberOfPoints-1 ] );
   else
Error();
    return( [ CurrentNumberOfPoints-1, CurrentNumberOfPoints ] );
   fi;
  else
   for i in [ 1 .. CurrentNumberOfPoints - 2 ] do
    Pred := Succ;
    Succ := Succ2;
    Succ2 := Succ2 ^ GeneratingCycle;
    if( IsFixedPoint() <> LastFixed ) then
     if( LastFixed ) then
Error();
      return( [ CurrentNumberOfPoints, i ] );
     else
Error();
      return( [ i, CurrentNumberOfPoints-1 ] );
     fi;
    fi;
   od;
  fi;

  return( fail );
 end;;

##################################################
fi;
# finished FindFixedAndMovedPoints
##################################################
