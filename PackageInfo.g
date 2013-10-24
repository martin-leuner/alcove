
SetPackageInfo( rec(

PackageName := "alcove",

Subtitle := "A package for algebraic combinatorics",

Version :=  Maximum( [
  "2013-10-21", ## Martin's version
## this line prevents merge conflicts
  "2013-10-24", ## Mohamed's version
## this line prevents merge conflicts
  "2013-04-08", ## Sebas' version
] ),

Date := ~.Version{[ 1 .. 10 ]},
Date := Concatenation( ~.Date{[ 9, 10 ]}, ".", ~.Date{[ 6, 7 ]}, ".", ~.Date{[ 1 .. 4 ]} ),

#ArchiveURL :=
#          Concatenation( "http://wwwb.math.rwth-aachen.de/~leuner/gap/", ~.PackageName, "/", ~.PackageName, "-", ~.Version ),
#
#ArchiveFormats := ".tar.gz",



Persons := [
rec(
    LastName      := "Leuner",
    FirstNames    := "Martin",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "leuner@momo.math.rwth-aachen.de",
    WWWHome       := "http://wwwb.math.rwth-aachen.de/~leuner/",
    PostalAddress := Concatenation( [
                       "Martin Leuner\n",
                       "Lehrstuhl B fuer Mathematik, RWTH Aachen\n",
                       "Templergraben 64\n",
                       "52062 Aachen\n",
                       "Germany" ] ),
    Place         := "Aachen",
    Institution   := "RWTH Aachen University"
  ),
],

Status := "dev",


#README_URL :=
#  "http://wwwb.math.rwth-aachen.de/~gutsche/gap_packages/Convex/README.Convex",
#PackageInfoURL :=
#  "http://wwwb.math.rwth-aachen.de/~gutsche/gap_packages/Convex/PackageInfo.g",

#AbstractHTML :=
#  Concatenation( "Convex provides structures and algorithms for convex geometry. It can handle convex, ",
#                 "fans and polytopes. Not only the structures are provided, but also a collection of ",
#                 "algorithms to handle those objects. Basically, it provides convex geometry to GAP. ",
#                 "It is capable of communicating with the CAS polymake via the package PolymakeInterface",
#                 " and also provides several methods by itself." ),

PackageWWWHome := "https://github.com/martin-leuner/alcove",

PackageDoc := rec(
  BookName  := "alcove",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "alcove - Objects in Algebraic Combinatorics",
  Autoload  := true
),


Dependencies := rec(
  GAP := ">=4.5",
  NeededOtherPackages := [
         [ "MatricesForHomalg", ">=2012.10.25" ],
         [ "GaussForHomalg", ">=2012.10.22" ],
         [ "ToolsForHomalg", ">=2013.04.08" ],
         [ "GAPDoc", ">=1.0" ],
         [ "AutoDoc", ">=2012.07.29" ]
      ],
  SuggestedOtherPackages := [
         [ "RingsForHomalg", ">=2012.10.22" ],
      ],
  ExternalConditions := [ ]

),

AvailabilityTest := ReturnTrue,

Autoload := false,

Keywords := [ "Algebraic combinatorics", "Matroid", "Association scheme", "Tutte polynomial" ]

) );


