USING: arrays assocs combinators.short-circuit fry hashtables
kernel locals make math mongodb.driver sequences sorting ;

FROM: mongodb.driver => find ;

IN: mongodb.util

<PRIVATE

: make-key ( prefix key -- key )
    over length 0 > [ 2array "." join ] [ nip ] if ;

:: extract-paths ( prefix assoc -- )
    assoc keys
    [| key | key assoc at
        dup { [ array? ] [ length 0 > ] } 1&&
        [ first ] when dup hashtable?
        [ [ prefix key make-key ] dip extract-paths ]
        [ drop prefix key make-key , ] if
    ] each ;  

: process-entries ( results assoc -- )
    '[ "" swap [ extract-paths ] { } make _ '[ 1 swap _ set-at ] each ] each ;

: process-results ( assoc cursor result -- assoc )
    [ [ over ] dip swap process-entries ] when*
    [ find process-results ] when* ;

: get-key-assoc ( collection -- assoc )
    [ H{ } clone ] dip
    H{ } <query> 5000 limit find process-results ;

PRIVATE>

: describe-schema ( collection -- schema )
    get-key-assoc keys natural-sort ;
