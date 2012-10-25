Print("# MinimalBlock( G, M, omega ) returns a minimal non-trivial block of G-set M under the action omega.\n# NOTE: omega must be a right action.\n");

MinimalBlock := function( G, M, omega )
 local Pos, gen, Gens, SmallestComputedBlock, ImageOfBlock, BlockCandidate, Unused, CurrentBlock, Orb, CurrentCheck;

 Gens := GeneratorsOfGroup(G);
 SmallestComputedBlock := M;

 if( Size(M) <= 1 ) then
  Print( "The provided G-set should contain at least 4 elements.\n" );
  return M;
 fi;
 if( Size( Orbit( G, M[1], omega ) ) < Size(M) ) then
  Error( "action must be transitive" );
 fi;

 for Pos in [ 2 .. Size(M) ] do

  BlockCandidate := Set([ M[1], M[Pos] ]);
  Unused := [ BlockCandidate ];
  Orb := [  ];

  while( not IsEmpty( Unused ) ) do
   CurrentBlock := Remove(Unused);
   for gen in Gens do
    ImageOfBlock := Set( CurrentBlock, m -> omega(m,gen) );
    if( not IsEmpty( Intersection( ImageOfBlock, BlockCandidate ) ) and ImageOfBlock <> BlockCandidate  ) then
     Unused := Set(ImageOfBlock);
     for CurrentCheck in Orb do
      if( not IsEmpty( Intersection( ImageOfBlock, CurrentCheck ) ) ) then
       Unused := Union2( Unused, CurrentCheck );
      fi;
     od;
     BlockCandidate := Union2( BlockCandidate, Unused );
     Orb := [ ];
     Unused := [ BlockCandidate ];
     break;
    fi;
    if( not ImageOfBlock in Orb ) then
     AddSet(Unused,ImageOfBlock);
    fi;
   od;
   AddSet( Orb, CurrentBlock );
  od;

  if( Size( BlockCandidate ) < Size( SmallestComputedBlock ) ) then
   SmallestComputedBlock := BlockCandidate;
  fi;
 od;

 if( Size(SmallestComputedBlock) = Size(M) ) then
  Print( "Primitive action.\n" );
 else
  Print( "Imprimitive action.\n" );
 fi;

 return SmallestComputedBlock;
end;;
