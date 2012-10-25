# Exercises in SSCGT 2011, part 1

###
# Exercise 1

if not IsBound( AtlasGroup ) then
LoadPackage( "AtlasRep" );
fi;

if not IsBound( Orb ) then
LoadPackage( "Orb" );
fi;

HN := AtlasGroup( "HN", Ring, GF(2) );

Print( "Read in HN.\n" );

input := GeneratorsOfGroup( HN );

work := [];;
output := [];;
work[1] := input[1];;
work[2] := input[2];;
work[3] := work[1] * work[2];;
work[4] := work[3] * work[2];;
work[2] := work[3] * work[4];;
work[6] := work[3] * work[2];;
work[7] := work[6] * work[3];;
work[5] := work[6]^-1;;
work[3] := work[6] * work[1];;
work[1] := work[3] * work[5];;
work[6] := work[7] * work[7];;
work[5] := work[6]^-1;;
work[3] := work[5] * work[2];;
work[2] := work[3] * work[6];;
output[1] := work[1];;
output[2] := work[2];;

Print( "Computed generators for 2.HN.2 <= HN.\n" );

HMod := GModuleByMats( output, GF(2) );;

Print( "Created H-module.\n" );

W := MTX.BasesMinimalSubmodules( HMod )[1][1];

Print( "Computed minimal submodules.\n" );

W := OnLines( W, One( HN ) );

Print( "Scaled vector to canonical form.\n" );

HWOrbit := Orb( input, W, OnLines, rec( hashlen := 3000000, report := 10000, storenumbers := true ) );

Print( "Created orb object.\n" );

Enumerate( HWOrbit );

Print( "Computed orbit.\n" );

PermGens := ActionOnOrbit( HWOrbit, input );
