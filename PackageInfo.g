package_name := "alcove";

github_account := "martin-leuner";
github_base_url := Concatenation("https://github.com/", github_account, "/");
github_raw_url := Concatenation("https://raw.githubusercontent.com/", github_account, "/", package_name, "/master/");

SetPackageInfo( rec(

PackageName := package_name,

Subtitle := "A package for algebraic combinatorics",

Version := "2024.01-01",

Date := ~.Version{[ 1 .. 10 ]},
Date := Concatenation( ~.Date{[ 9, 10 ]}, "/", ~.Date{[ 6, 7 ]}, "/", ~.Date{[ 1 .. 4 ]} ),

License := "GPL-2.0-or-later",

PackageWWWHome := Concatenation(github_base_url, ~.PackageName),

ArchiveURL := Concatenation(~.PackageWWWHome, "/archive/master.zip"),
ArchiveFormats := ".zip",

README_URL := Concatenation(github_raw_url, "README"),
PackageInfoURL := Concatenation(github_raw_url, "PackageInfo.g"),


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

AbstractHTML := Concatenation(
    "This package aims at providing an implementation of matroids as GAP objects.",
    "Efficient computation of their structural properties and certain invariants",
    "is a key goal of the package, partly using powerful logical methods from the",
    "homalg project."
    ),

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


