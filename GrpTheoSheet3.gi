Print( "\n\n------------------------------------------------------------\n                        Aufgabe 2                           \n------------------------------------------------------------\n\n" );

Print( "# Der Quelltext von HasNormalSubgroupsC2GL32 und HasOtherFactorOfCorrectType ist in der eingelesenen Datei zu finden.\n\n");
Print( "# Offenbar haben Gruppen mit Kompositionsfaktoren C_2, GL(3,2) Ordnung 336. Weiter haben wir eine Kompositionsreihe G > N > 1,\n# wo N isomorph zu C_2 oder GL(3,2) ist und G/N isomorph zum jeweils anderen Faktor.\n# Suche also Gruppen mit maximalem Normalteiler C_2 oder GL(3,2).\n" );

ListOfCandidates := AllSmallGroups( 336 );;
ListOfCandidates := Filtered( ListOfCandidates, G -> Size( CompositionSeries(G) ) = 3 );;

HasNormalSubgroupC2GL32 := function( G )
 local MaxNormalSubgps, C2, GL32;
 MaxNormalSubgps := MaximalNormalSubgroups( G );
 C2 := CyclicGroup(2);
 GL32 := GL(3,2);

 return ForAny( MaxNormalSubgps, N -> IsomorphismGroups( N, C2 ) <> fail or IsomorphismGroups( N, GL32 ) <> fail );
end;;

Print( "gap> ListOfCandidates := Filtered( AllSmallGroups( 2*168 ), HasNormalSubgroupC2GL32 );;\n\n" );

ListOfCandidates := Filtered( ListOfCandidates, HasNormalSubgroupC2GL32 );;

Print( "# Betrachte nun noch den Faktor G/N.\n" );

HasOtherFactorOfCorrectType := function( G )
 local C2, GL32, N, M;
 C2 := CyclicGroup( 2 );
 GL32 := GL(3,2);
 M := MaximalNormalSubgroups( G );
 for N in M do
  if( IsomorphismGroups( N, C2 ) <> fail ) then
   if( IsomorphismGroups( FactorGroup( G, N ), GL32 ) <> fail ) then
    return true;
   fi;
  fi;
 od;
 for N in M do
  if( IsomorphismGroups( N, GL32 ) <> fail ) then
   if( IsomorphismGroups( FactorGroup( G, N ), C2 ) <> fail ) then
    return true;
   fi;
  fi;
 od;
 return false;
end;;

Print( "gap> ListOfCandidates := Filtered( ListOfCandidates, HasOtherFactorOfCorrectType );;\n\n" );
ListOfCandidates := Filtered( ListOfCandidates, HasOtherFactorOfCorrectType );;

Print( "# Somit sind alle gesuchten Gruppen herausgefiltert.\ngap> List( ListOfCandidates, StructureDescription );\n" );
Display( List( ListOfCandidates, StructureDescription ) );
