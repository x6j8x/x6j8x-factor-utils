! Copyright (C) 2009 Sascha Matzke
! See http://factorcode.org/license.txt for BSD license.
USING: kernel threads concurrency.mailboxes continuations locals
namespaces assocs accessors summary fry calendar math sequences math.order ;

RENAME: receive concurrency.messaging => (receive)
RENAME: receive-if concurrency.messaging => (receive-if)

IN: concurrency.messaging.timeout

TUPLE: envelope sender tag data expiry ;

<PRIVATE

: new-envelope ( data class -- envelope )
    new swap >>data self >>sender ;

PRIVATE>

TUPLE: synchronous < envelope ;

TUPLE: reply < envelope ;

: <envelope> ( data -- envelope )
    dup envelope?
    [ envelope new-envelope ] unless ;

<PRIVATE

GENERIC: expired? ( message -- ? )

M: envelope expired? 
    expiry>> [ now before? ] [ f ] if* ;

M: object expired?
    drop f ;

: ?valid ( message -- message/f )
    ?linked dup expired? [ drop f ] [ ] if ; 

: wait-time ( border -- wait )
    now (time-) dup 0 >
    [ ] [ drop 0 ] if seconds ;

PRIVATE>

GENERIC: send-timeout ( timeout message thread -- )

M: thread send-timeout ( timeout message thread -- )
    [ <envelope> swap hence >>expiry ] dip send ;

: receive ( -- message )
    (receive) ?valid [ (receive) ] unless* ; 

:: receive-timeout ( timeout -- message )
    [let | border [ timeout hence ]
           result! [ f ] |
        [ result ?valid not ]  
        [ my-mailbox border wait-time
          mailbox-get-timeout result! ] while
        result
    ] ;

: receive-if ( pred -- message )
    dup (receive-if) ?valid [ nip ] [ (receive-if) ] if* ; inline recursive

:: receive-if-timeout ( timeout pred -- message )
    [let | border [ timeout hence ]
           result! [ f ]
    |
        [ result ?valid not ] 
        [ my-mailbox border wait-time pred
          mailbox-get-timeout? result! ] while
        result
    ] ;

<PRIVATE

: <synchronous> ( data -- sync )
    synchronous new-envelope
    synchronous counter >>tag ;

: <reply> ( data synchronous -- reply )
    [ reply new-envelope ] dip
    tag>> >>tag ;

: synchronous-reply? ( response synchronous -- ? )
    over reply? [ [ tag>> ] bi@ = ] [ 2drop f ] if ;

PRIVATE>

ERROR: cannot-send-synchronous-to-self message thread ;

M: cannot-send-synchronous-to-self summary
    drop "Cannot synchronous send to myself" ;

<PRIVATE

: ?self ( message thread -- message thread )
    dup self eq? [ cannot-send-synchronous-to-self ] when ; inline

: reply-synchronous ( message synchronous -- )
    dup expired?
    [ 2drop ] [ [ <reply> ] keep sender>> send ] if ;

PRIVATE>

:: send-synchronous ( message thread -- reply )
    [let | envelope [ message <synchronous> ]
           | envelope thread ?self send
             [ envelope synchronous-reply? ] receive-if data>>
    ] ;

:: send-synchronous-timeout ( timeout message thread -- reply )
    [let | envelope [ message <synchronous> ]
           | timeout envelope thread ?self send-timeout
             timeout [ envelope synchronous-reply? ] receive-if-timeout data>>
    ] ;


: handle-synchronous ( quot -- )
    receive [
        swap call
    ] keep reply-synchronous ; inline
