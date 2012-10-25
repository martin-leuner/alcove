 MatchRandomLongCycle := function()
  local Points;
  RandomLongCycle := LongCycle ^ PseudoRandom( G );
Print( "RandomLongCycle: ", RandomLongCycle, "\n" );
  Points := FindFixedAndMovedPoints();
Print( "Points: ", Points, "\n" );
  if( Points = fail ) then				# conjugate of LongCycle consists of known points
   return fail;
  fi;

  if( Points[1] = CurrentNumberOfPoints ) then		# k fixed
   if( Points[2] = CurrentNumberOfPoints-1 ) then	# k-1 not fixed
    ExchangingElement := One( G );
   else							# k-1 fixed
    ExchangingElement := LastTransposition ^ ( CheckingCycle ^ Points[2] * LastTransposition );
   fi;
  else							# k not fixed
   if( Points[2] = CurrentNumberOfPoints-1 ) then	# k-1 not fixed
    ExchangingElement := LastTransposition ^ ( CheckingCycle ^ Points[1] );
   else							# k-1 fixed
    ExchangingElement := LastTransposition;
   fi;
  fi;

Print( "ExchangingElement: ", ExchangingElement, "\n" );
  RandomLongCycle := RandomLongCycle ^ ExchangingElement;
Print( "RandomLongCycle: ", RandomLongCycle, "\n" );
 end;

