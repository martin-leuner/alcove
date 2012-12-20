
SetPackageInfo( rec(

PackageName := "alcove",

Subtitle := "A package for algebraic combinatorics",

Version :=  Maximum( [
  "2012-12-20", ## Martin's version ## in case this package should ever be edited by more than one person
] ),

Date := ~.Version{[ 1 .. 10 ]},
Date := Concatenation( ~.Date{[ 9, 10 ]}, ".", ~.Date{[ 6, 7 ]}, ".", ~.Date{[ 1 .. 4 ]} ),

ArchiveURL := 
          Concatenation( "http://wwwb.math.rwth-aachen.de/~leuner/gap/", ~.PackageName, "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",



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

#PackageWWWHome := "http://wwwb.math.rwth-aachen.de/~gutsche/gap_packages/Convex/",
#               
#PackageDoc := rec(
#  BookName  := "alcove",
#  ArchiveURLSubset := ["doc"],
#  HTMLStart := "doc/chap0.html",
#  PDFFile   := "doc/manual.pdf",
#  SixFile   := "doc/manual.six",
#  LongTitle := "alcove - Objects in Algebraic Combinatorics",
#  Autoload  := false
#),


Dependencies := rec(
  GAP := ">=4.5",
  NeededOtherPackages := [ [ "MatricesForHomalg", ">=0" ], [ "RingsForHomalg", ">=0" ], [ "GaussForHomalg", ">=0" ], [ "ToolsForHomalg", ">=2012.12.01" ] ],
  SuggestedOtherPackages := [],
  ExternalConditions := []
                      
),

AvailabilityTest := ReturnTrue,
    
Autoload := false,

Keywords := [ "Algebraic combinatorics", "Matroid", "Association scheme", "Tutte polynomial", "C-algebra" ]

));


