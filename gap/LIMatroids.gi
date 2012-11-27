

########################################################
##
## Tweet some properties and attributes to dual matroids
## 
########################################################


## IsUniform

InstallImmediateMethod( Twitter,
			IsMatroid and HasDualMatroid and HasIsUniform,
			0,

 function( matroid )

  SetIsUniform( DualMatroid( matroid ), IsUniform( matroid ) );
  TryNextMethod();

 end

);


## Rank

InstallImmediateMethod( Twitter,
			IsMatroid and HasDualMatroid and HasRankOfMatroid,
			0,

 function( matroid )

  SetRankOfMatroid( DualMatroid( matroid ), SizeOfGroundSet( matroid ) - RankOfMatroid( matroid ) );
  TryNextMethod();

 end

);


