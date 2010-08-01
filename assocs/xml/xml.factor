USING: accessors assocs formatting io io.encodings.binary
io.streams.byte-array kernel namespaces sequences strings tools.walker
vectors xml xml.data fry continuations ;

IN: assocs.xml

<PRIVATE

TUPLE: state root scope exemplar ;

: init-parse-state ( exemplar -- state )
    [ state new ] dip >>exemplar ; inline

: current-state ( -- state )
    state get ; inline

TUPLE: element name content value children? ;

: <element> ( name -- element )
    [ element new ] dip >>name ; inline

: current-element ( -- element )
    current-state scope>> dup
    empty?
    [ drop f ]
    [ [ pop ] keep
      [ push ] [ drop ] 2bi ] if ; inline

: element-content ( element -- assoc )
    [ ] [ content>> ] bi
    [ nip ]
    [ current-state exemplar>> clone [ >>content drop ] keep ] if* ; inline

: current-content ( -- assoc )
    current-element
    [ element-content ]
    [ current-state root>> ] if* ; inline

: add-to-content ( value key -- )
    [ ] [ current-content at ] bi dup 
    [ dup vector?
      [ nip push ]
      [ 1vector swap [ [ push ] keep ] dip current-content set-at ] if 
    ]
    [ drop current-content set-at ] if ; inline

: add-attr ( key value -- )
    swap [ "@" ] dip main>> "%s%s" sprintf 
    current-content set-at ; inline

: set-string-value ( string -- )
    current-element [ swap >>value drop ] [ drop ] if* ; inline

: process-attributes ( event -- )
    attrs>>
    [ alist>> [ add-attr ] assoc-each ] when* ; inline

: mark-child ( -- )
    current-element [ t >>children? drop ] when* ; inline

: open-element ( event -- )
    mark-child
    name>> main>> <element>
    current-state scope>> push ; inline

: set-string-content ( value assoc -- assoc )
    [ "Content" ] dip [ set-at ] keep ;

: choose-content ( element -- content )
    [ value>> ] [ children?>> ] [ content>> ] tri
    [ swap [ nip ] [ over [ set-string-content ] [ nip ] if ] if ]
    [ drop ] if* ; inline

: close-element ( event -- )
    name>> main>> 
    current-state scope>> pop
    [ choose-content ] [ name>> dup ] bi
    [ pick ] dip = [ add-to-content ] [ 2drop ] if drop ; inline

GENERIC: process-event ( event -- ) 

M: prolog process-event
    drop current-state [ ] [ exemplar>> clone ] bi >>root
    V{ } clone >>scope drop ;

M: opener process-event
    [ open-element ]
    [ process-attributes ] bi ;

M: closer process-event
    close-element ;

M: contained process-event
    [ open-element ]
    [ process-attributes ]
    [ close-element ] tri ;

M: string process-event
    set-string-value ;

M: tuple process-event drop ;

: with-parse-state ( exemplar quot: ( -- result ) -- result )
    [ init-parse-state state ] dip '[ _ call( -- result ) ] with-variable ; inline

PRIVATE>

: xml>assoc ( bytearray -- assoc/f )
    [ binary
      [ H{ } [ input-stream get [ process-event ] each-element
               current-state root>> ] with-parse-state ] with-byte-reader
    ] [ 2drop f ] recover ;
