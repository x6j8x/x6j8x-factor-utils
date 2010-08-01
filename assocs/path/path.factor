! See http://factorcode.org/license.txt for BSD license

USING: assocs fry kernel math sequences splitting arrays vectors hashtables ;

IN: assocs.path

<PRIVATE

: (@) ( key object quot -- value/f )
    [ dup sequence? ] dip 
    [ '[ _ with map sift
         dup empty?
         [ drop f ]
         [ dup length 1 = [ first ] when ] if
    ] ] keep '[ @ ] if ; inline

: (at@) ( key object -- value/f )
    [ at ] (@) ; inline

: (delete-at@) ( key object -- value/f )
    [ delete-at* drop ] (@) ; inline

: split-keypath ( keypath assoc -- last assoc head )
    swap "." split [ 1 tail-slice* swap ] [ 1 head-slice* ] bi ; inline

: prepare-path ( assoc head -- last-part )
    [ swap [ ] [ (at@) ] 2bi
      [ [ 2drop ] dip ]
      [ [ H{ } clone dup ] 2dip set-at ] if*
    ] each ; inline

: (change-at@) ( key assoc quot -- )
    [ [ at ] dip call ] [ drop ] 3bi
    pick [ set-at ] [ 3drop ] if ; inline

PRIVATE>

! retrieves values from nested assocs...
! "a.b.c" H{ { "a" H{ { "b" { H{ { "c" 23 } } H{ { "c" 42 } } } } } } } at@
! => { 23 42 }
: at@ ( pathstr assoc -- array/f )
    swap "." split [ swap (at@) ] each
    dup [ vector? ] [ array? ] bi or
    [ dup [ 1array ] when ] unless ; 

: at@> ( pathstr assoc -- first/f )
    at@ [ first ] [ f ] if* ; inline

! stores a value into a nested assoc, if a certain graph doesn't
! yet exists, a new assoc is created for the key
: set-at@ ( value keypath object -- )
    split-keypath prepare-path
    [ first ] dip [ drop ] change-at ; inline

: change-at@ ( keypath object quot -- )
    [ split-keypath prepare-path [ first ] dip ] dip
    (change-at@) ; inline

: delete-at@ ( keypath assoc -- value/f )
    split-keypath [ swap (at@) ] each
    [ first ] dip (delete-at@) ; inline

: update-selected ( selectors assoc1 assoc2 -- )
    '[ [ _ at@ dup length 1 = [ first ] [ ] if ] keep
       [ _ set-at@ ] curry
       [ ] if* ] each ; inline

: extract@ ( assoc selectors -- assoc' )
    swap H{ } clone [ update-selected ] keep ; inline

: associate@ ( value key -- hash )
    2 <hashtable> [ set-at@ ] keep ; inline
