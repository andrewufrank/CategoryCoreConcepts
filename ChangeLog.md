Journal of work in CategorCoreConcepts
0.2
- try with hgeometry. The use of multiple "advanced" features of ghc makes solving bugs difficult. Examples 
    - the use of `:+` to add a (nearly) facultativ name to a point. It is sometimes demanded and must then be filled with `()`. Seemingly a convention, to require a name for some functions with `'` and no name without. 
    - use of NonEmpty list 
    - use of Lens but without some non-standard values 

    The advantage would be that hgeometry calculates all in haskell and is all pure code. 

0.2.1 use ghull
- build hq for triangulation
    disadvantage : runs in IO
    advantage - can build the hq (done in uniform-geometry)
- change delaunay2 with input list of V2 (rest default)
    then the hqs are produced with 
    `trip_hqs_faces` applied to result 

- using stack 
    