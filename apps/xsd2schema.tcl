# Simple converter from (sane) xsd to tdom schema.
#
# BEWARE: Within the xsd namespace in this code there is a 'list'
# procedure. Refer to the core [list] command by ::list within code
# evaluated within the xsd namespace.

package require tdom
namespace import tdom::*

namespace eval xsd {
    variable indent 2
    variable level 0
    variable xsddata
    variable basedir
    variable targetNS
    variable schema
    variable output "collect"
    variable nrLookAt 0
    variable xsd2schemaName [dict create {*}{
        base64Binary base64
        decimal number
        ID id
        IDREF idref
        Name name
        NCName ncname
        NMTOKEN nmtoken
        QName qname
    }]

    namespace unknown forwardCompatibility
}

# Not used by the converter code itself but written to the created
# schema with the help of [info body].
proc xsd::tclxsdtypes {type value} {
    switch $type {
        default {
            puts  "Type $type not implemented"
        }
    }
    return 1
}

proc xsd::prolog {} {
    variable output

    if {$output eq "collect"} {
        variable schema
        append schema "# Prolog\n"
        append schema "::namespace eval ::xsd { }\n"
        append schema "proc ::xsd::tclxsdtypes {type value} \{[info body ::xsd::tclxsdtypes]\}\n"
        append schema "# Prolog end\n"
    } else {
        puts "# Prolog"
        puts "::namespace eval ::xsd { }"
        puts "proc ::xsd::tclxsdtypes {type value} \{[info body ::xsd::tclxsdtypes]\}"
        puts "# Prolog end"
    }
}

proc xsd::forwardCompatibility {args} {
    return
}

proc xsd::indent {} {
    variable indent
    variable level
    return [string repeat " " [expr {$indent * $level}]]
}

proc xsd::prefix {ns} {
    variable xsddata
    return [dict get $xsddata nsprefix $ns]
}

proc xsd::saveReset {args} {
    foreach var $args {
        uplevel "set __saved_$var \[set $var\]; set $var \"\""
    }
}

proc xsd::restoreSaved {{keeplocalcopies 0}} {
    if {$keeplocalcopies} {
        uplevel {
            foreach __var [info vars __saved_*] {
                set _[string range $__var 8 end] [set [string range $__var 8 end]]
                set [string range $__var 8 end] [set $__var]
            }
        }
    } else {
        uplevel {
            foreach __var [info vars __saved_*] {
                set [string range $__var 8 end] [set $__var]
            }
        }
    }
}

proc rproc {name args body} {
    proc $name $args "variable level;upvar result result;$body"
}

rproc xsd::sput {text} {
    append result "[indent]$text\n"
}

rproc xsd::sputnnl {text} {
    append result "[indent]$text"
}

rproc xsd::sputc {text} {
    foreach line [split $text "\n"] {
        append result "[indent]# $line\n"
    }
}

rproc xsd::sputce {text} {
    incr ::xsd::nrLookAt
    foreach line [split $text "\n"] {
        append result "[indent]# LOOK_AT $line\n"
    }
}

rproc xsd::out {{nonl 0}} {
    variable output

    if {![info exists result]} {
        return
    }
    if {$output eq "collect"} {
        variable schema
        if {$nonl} {
            append schema $result
        } else {
            append schema $result "\n"
        }
    } else {
        if {$nonl} {
            puts -nonewline $result
        } else {
            puts $result
        }
    }
    set result ""
}

proc xsd::getQuant {node} {
    set min [$node @minOccurs 1]
    set max [$node @maxOccurs 1]

    switch $min {
        0 {
            switch $max {
                0 {
                    # Huch: min and max == 0, so never at all?
                }
                1 {return ?}
                "unbounded" {return *}
                default {return "{0 $max}"}
            }
        }
        1 {
            switch $max {
                0 {
                    # Huch: min == 1 and max == 0, ?
                }
                1 {return "!"}
                "unbounded" {return +}
                default {return "{1 $max}"}
            }
        }
        default {
            if {$max eq "unbounded"} {
                return "{$min *}"
            } else {
                if {$max < $min} {
                    # Huch, contradiction
                    return
                } elseif {$max == $min} {
                    return $min
                } else {
                    return "{$min $max}"
                }
            }
        }
    }
}

rproc xsd::annotation {node} {
    foreach child [$node selectNodes xsd:*] {
        if {[$child localName] eq "documentation"} {
            set lang [$child getAttributeNS \
                          "http://www.w3.org/XML/1998/namespace" lang ""]
            if {$lang ne ""} {
                 sputc "($lang:) [$child text]"
            } else {
                sputc "[$child text]"
            }
        } else {
            sputc "(appinfo:) [$child text]"
        }
    }
}

proc xsd::nsfromprefix {contextNode prefix} {
    return [lindex [lindex [$contextNode selectNodes {namespace::*[name()=$prefix]}] 0] 1]

}

proc xsd::resolveFQ {name node {forAtt 0}} {
    variable targetNS
    variable mainTargetNS

    if {[string first : $name] > -1} {
        lassign [split $name :] prefix name
        return [::list [nsfromprefix $node $prefix] $name]
    } else {
        if {$forAtt} {
            return [::list $targetNS $name]
        } else {
            set docElemNode [[$node ownerDocument] documentElement]
            if {![$docElemNode hasAttribute targetNamespace]} {
                # Include of a "chameleon" xsd
                return [::list $targetNS $name]
            } else {
                return [::list [$docElemNode @targetNamespace] $name]
            }
        }
    }
}

proc xsd::processNamespaces {} {
    variable xsddata
    variable level

    set result "prefixns \{\n"
    set level 1
    set nr 1
    dict for {ns dummy} [dict get $xsddata namespace] {
        if {$ns eq ""} {
            dict set xsddata nsprefix $ns ""
        } else {
            dict set xsddata nsprefix $ns "ns$nr"
            sput "ns$nr $ns"
            incr nr
        }
    }
    if {$nr > 1} {
        append result "\}\n"
        out
    }
    set level 0
}

rproc xsd::import {import} {
    variable xsddata
    variable targetNS
    variable basedir
    variable localCopies
    variable knownNamespaces
    variable schemadocs
    
    set schemaLocation [$import @schemaLocation ""]
    if {$schemaLocation eq ""} {
        set namespace [$import @namespace ""]
        if {[info exists knownNamespaces($namespace)]} {
            set schemaLocation [file normalize $knownNamespaces($namespace)]
        }
        if {$schemaLocation eq ""} {
            sputce "import: skiping because of missing schemaLocation\n[$import asXML]"
            return
        }
    }
    
    if {[dict exists $xsddata imported $schemaLocation]} {
        return
    }
    dict set xsddata imported $schemaLocation ""
    if {[info exists localCopies($schemaLocation)]} {
        set schemaLocation [file normalize $localCopies($schemaLocation)]
    }
    set savedTargetNS $targetNS
    try {
        set xsddoc [dom parse [xmlReadFile [file join $basedir $schemaLocation]]]
        lappend schemadocs $xsddoc
        $xsddoc selectNodesNamespaces {
            xsd "http://www.w3.org/2001/XMLSchema"
        }
        set xsd [$xsddoc documentElement]
        set targetNS [$xsd @targetNamespace ""]
        processToplevel $xsd
    } on error errMsg {
        sputce "Can't access\n[$import asXML]$errMsg"
        if {$::xsd::standalone} {
            out
            exit
        }
    }
    set targetNS $savedTargetNS
}

rproc xsd::include {include} {
    variable xsddata
    variable basedir
    variable schemadocs
    
    set schemaLocation [$include @schemaLocation ""]
    if {$schemaLocation eq ""} {
        sputce "include: skiping because of missing schemaLocation\n[$include asXML]"
        return
    }
    if {[dict exists $xsddata included $schemaLocation]} {
        return
    }
    dict set xsddata included $schemaLocation ""
    try {
        set xsddoc [dom parse [xmlReadFile [file join $basedir $schemaLocation]]]
        lappend schemadocs $xsddoc
        $xsddoc selectNodesNamespaces {
            xsd "http://www.w3.org/2001/XMLSchema"
        }
        set xsd [$xsddoc documentElement]
        processToplevel $xsd
    } on error errMsg {
        sputce "Can't access\n[$include asXML]$errMsg"
        if {$::xsd::standalone} {
            out
            exit
        }
    }        
}

rproc xsd::redefine {redefine} {
    variable xsddata
    variable basedir
    variable redefining
    variable schemadocs
    
    set schemaLocation [$redefine @schemaLocation ""]
    if {$schemaLocation eq ""} {
        sputce "redefine: skiping because of missing schemaLocation\n[$redefine asXML]"
        return
    }
    if {[dict exists $xsddata redefined $schemaLocation]} {
        return
    }
    sputce "Global element redefine just includes, but does not actually redefine, so far"
    dict set xsddata redefined $schemaLocation ""
    set storedredefining $redefining
    try {
        set xsddoc [dom parse [xmlReadFile [file join $basedir $schemaLocation]]]
        lappend schemadocs $xsddoc
        $xsddoc selectNodesNamespaces {
            xsd "http://www.w3.org/2001/XMLSchema"
        }
        set xsd [$xsddoc documentElement]
        processToplevel $xsd
    } on error errMsg {
        sputce "Can't access\n[$redefine asXML]$errMsg"
        if {$::xsd::standalone} {
            out
            exit
        }
    }
    set redefining $storedredefining
}

proc xsd::processToplevel {xsd} {
    variable xsddata
    variable targetNS

    foreach toplevelelm [$xsd selectNodes xsd:*] {
        set localname [$toplevelelm localName]
        incr ::ecount($localname)
        switch $localname {
            "annotation" -
            "import" -
            "include" -
            "redefine" {
                $localname $toplevelelm
            }
            "annotation" {
                annotation $toplevelelm
            }
            "attribute" {
                set name [$toplevelelm @name ""]
                dict set xsddata namespace $targetNS attribute $name xsd $toplevelelm
                dict set xsddata namespace $targetNS attribute $name namespace $targetNS
            }
            default {
                set name [$toplevelelm @name ""]
                incr ::tlc($localname)
                if {$name eq ""} {
                    sputce "Global element $localname without name"
                    continue
                }
                dict set xsddata namespace $targetNS $localname $name xsd $toplevelelm
            }
        }
        out
    }
}

proc xsd::mapXsdTypeToSchema {type} {
    variable xsd2schemaName

    switch $type {
        "base64Binary" -
        "decimal" -
        "ID" -
        "IDREF" -
        "Name" -
        "NCName" -
        "NMTOKEN" -
        "QName" {
            return [dict get $xsd2schemaName $type]
        }
        "anyURI" -
        "byte" -
        "double" -
        "ENTITIES" -
        "ENTITY" -
        "float" -
        "gDay" -
        "gMonth" -
        "gMonthDay" -
        "gYear" -
        "gYearMonth" -
        "int" -
        "long" -
        "NOTATION" -
        "short" -
        "yearMonthDuration" {
            return "tcl ::xsd::tclxsdtypes $type"
        }
        "IDREFS" {
            return "split idref"
        }
        "language" {
            return {regexp {^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$}}
        }
        "NMTOKENS" {
            return "split nmtoken"
        }
        "boolean" -
        "date" -
        "dateTime" -
        "double" -
        "duration" -
        "hexBinary" -
        "integer" -
        "negativeInteger" -
        "nonNegativeInteger" -
        "nonPositiveInteger" -
        "positiveInteger" -
        "unsignedByte" -
        "unsignedInt" -
        "unsignedLong" -
        "unsignedShort" -
        "time" {
            return $type
        }
        "normalizedString" {
            incr count(normalizedString)
        }
        "string" {
            incr count(string)
        }
        "token" {
            incr count(token)
        }
    }
    return ""
}

rproc xsd::restriction {node} {
    variable xsddata
    variable targetNS
    variable currentBase

    set savedCurrentBase $currentBase
    set currentBase [$node @base $currentBase]
    if {[string first : $currentBase] > -1} {
        lassign [split $currentBase :] prefix type
    } else {
        set prefix ""
        set type $currentBase
    }
    set typens [nsfromprefix $node $prefix]
    set context [[$node parentNode] localName] 
    switch $context {
        "simpleContent" -
        "simpleType" {
            if {$typens eq "http://www.w3.org/2001/XMLSchema"} {
                # Derived from an xsd base type
                #
                # The first restiction element child may be an annotation.
                set firstxsdchild [$node selectNodes {xsd:*[1]}]
                if {$firstxsdchild != "" && [$firstxsdchild localName] eq "annotation"} {
                    annotation $firstxsdchild
                }
                set enumerations [$node selectNodes xsd:enumeration]
                if {[llength $enumerations]} {
                    sput "enumeration \{"
                    incr level
                    foreach enumeration $enumerations {
                        sput [::list [$enumeration @value]]
                    }
                    incr level -1
                    sput "\}"
                    set currentBase $savedCurrentBase
                    return
                }
                set tdomtype [mapXsdTypeToSchema $type]
                if {$tdomtype ne ""} {
                    sput $tdomtype
                }
            } else {
                # Derived from another simple Type
                if {$typens eq ""} {
                    set typens $targetNS
                }
                if {$type ne ""} {
                    if {$typens ne ""} {
                        sput "type [prefix $typens]:$type"
                    } else {
                        sput "type $type"
                    }
                }
            }
            if {$context eq "simpleContent"} {
                sputce "simpleContent restriction to finalize"
            }
        }
        "complexContent" {
            sputce "complexType restriction to do"
        }
    }
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    set currentBase $savedCurrentBase
}

rproc xsd::enumeration {node} {
    # Already handled by code in xsd::restriction.
    # Just do nothing
}

rproc xsd::fractionDigits {node} {

}

rproc xsd::length {node} {
    # TODO Special handling of xs:hexBinary and xs:base64Binary and
    # list types.
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    sput "length [$node @value]"
}

rproc xsd::maxExclusive {node} {

}

rproc xsd::maxInclusive {node} {

}

rproc xsd::maxLength {node} {
    # TODO Special handling of xs:hexBinary and xs:base64Binary and
    # list types.
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    sput "maxLength [$node @value]"
}

rproc xsd::minExclusive {node} {

}

rproc xsd::minInclusive {node} {

}

rproc xsd::minLength {node} {
    # TODO Special handling of xs:hexBinary and xs:base64Binary and
    # list types.
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    sput "minLength [$node @value]"
}

rproc xsd::pattern {node} {
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    # The additional xsd pattern character classes (not supported by
    # the tcl regexp):
    # \i [:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]
    # \c [-.0-9:A-Z_a-z\u00B7\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u037D\u037F-\u1FFF\u200C-\u200D\u203F\u2040\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]
    # (and the reverse ones \I und \C)
    # Also not supported by the the tcl repexp:
    # Character class substraction
    set regexp [$node @value]
    if {[catch {regexp -- $regexp "foo"}]} {
        set regexp [string map \
                        [::list {\i} {[:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]} \
                             {\c} {[-.0-9:A-Z_a-z\u00B7\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u037D\u037F-\u1FFF\u200C-\u200D\u203F\u2040\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]}] \
                        $regexp]
        if {[catch {regexp -- $regexp "foo"}]} {
            # It isn't so simple, probably character class substraction is involved
            sputce "The xsd pattern could not be converted."
            sputce "The orignal pattern is:"
            sputce "[$node @value]"
            sput "regexp {^.*$}"
        } else {
            sput "regexp \{^$regexp\$\}"
        }
    } else {
        sput "regexp \{^$regexp\$\}"
    }
}

rproc xsd::totalDigits {node} {
    
}

rproc xsd::whiteSpace {node} {

}

# Debug proc
proc xsd::parents {node} {
    set node [$node parentNode]
    while {$node ne ""} {
        puts [$node localName]
        set node [$node parentNode]
    }
}

rproc xsd::union {node} {
    sput "oneOf \{"
    incr level
    set memberTypes [$node @memberTypes ""]
    if {$memberTypes ne ""} {
        foreach memberType $memberTypes {
            textType $node $memberType
        }
    } else {
        foreach child [$node selectNodes xsd:*] {
            [$child localName] $child
        }
    }
    incr level -1
    sput "\}"
}

rproc xsd::list {node} {
    sput "split \{"
    incr level
    set itemType [$node @itemType ""]
    if {$itemType ne ""} {
        
    } else {
        foreach child [$node selectNodes xsd:*] {
            [$child localName] $child
        }
    }
    incr level -1
    sput "\}"
}

rproc xsd::assert {node} {
    sputce "assert not implemented"
}

rproc xsd::assertion {node} {
    sputce "assertion not implemented"
}

rproc xsd::alternative {node} {
    sputce "alternative not implemented"
}

rproc xsd::explicitTimezone {node} {
    sputce "explicitTimezone not implemented"
}

proc xsd::processGlobalSimpleTypes {} {
    variable xsddata
    variable targetNS
    variable level
    
    incr level
    dict for {targetNS nsdata} [dict get $xsddata namespace] {
        if {![dict exists $nsdata simpleType]} continue
        dict for {simpleType stdata} [dict get $nsdata simpleType] {
            set xsd [dict get $stdata xsd]
            if {$targetNS ne ""} {
                set result "deftexttype [prefix $targetNS]:$simpleType \{\n"
            } else {
                set result "deftexttype $simpleType \{\n"
            }
            foreach child [$xsd selectNodes xsd:*] {
                [$child localName] $child
            }
            append result  "\}"
            out
        }
    }
    incr level -1
}

rproc xsd::sequence {node} {
    set insideChoice 0
    if {[[$node parentNode] localName] eq "choice"} {
        set insideChoice 1
        sput "group [getQuant $node] \{"
        incr level
    }
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    if {$insideChoice} {
        sput "\}"
        incr level -1
    }
}

rproc xsd::choice {node} {
    sput "choice [getQuant $node] \{"
    incr level
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    incr level -1
    sput "\}"
}

rproc xsd::all {node} {
    sput "interleave [getQuant $node] \{"
    incr level
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    incr level
    sput "\}"
}

rproc xsd::group {node} {
    variable targetNS
    
    set ref [$node @ref ""]
    lassign [resolveFQ $ref $node] ns name
    if {$ns eq $targetNS} {
        sput "ref ($name) [getQuant $node]"
    } else {
        sput "namespace $ns \{"
        incr level
        sput "ref ($name) [getQuant $node]"
        incr level -1
        sput "\}"
    }
}

rproc xsd::elementWorker {node} {
    variable xsddata
    variable targetNS

    set type [$node @type ""]
    if {$type ne ""} {
        # Refers to type definition
        # First check, if the type name is full qualified
        if {[string first : $type] > -1} {
            lassign [split $type :] prefix type
        } else {
            set prefix ""
        }
        set thisns [nsfromprefix $node $prefix]
        # Check for text only element with basic data type
        if {$thisns eq "http://www.w3.org/2001/XMLSchema"} {
            set texttypecode [mapXsdTypeToSchema $type]
            if {$texttypecode eq ""} {
                append result "text\n"
            } elseif {[string first " " $texttypecode] > -1} {
                sput "text \{$texttypecode\}"
            } else {
                append result "text $texttypecode"
            }
            return
        }
        if {[dict exists $xsddata namespace $thisns simpleType $type]} {
            set start "text type "
            if {$thisns ne ""} {
                append start "[prefix $thisns]:"
            }
            sput "$start$type"
            return
        } elseif {[dict exists $xsddata namespace $thisns complexType $type]} {
            set xsd [dict get $xsddata namespace $thisns complexType $type xsd]
            # During the first pass of processGlobalComplexTypes there
            # may be no atts key so far.
            set atts ""
            if {[dict exists $xsddata namespace $thisns complexType $type atts]} {
                set atts [dict get $xsddata namespace $thisns complexType $type atts]
            }
            if {$thisns eq $targetNS} {
                generateAttributes $atts
                sput "ref $type"
            } else {
                sput "namespace [prefix $thisns] \{"
                generateAttributes $atts
                sput "ref $type"
                sput "\}"
            }
            return
        } else {
            sputce "# Element type [$node @type] $type $thisns not found"
        }
        return
    } else {
        # Local defined type
        #append result "\{\n"
        #incr level
        foreach child [$node selectNodes xsd:*] {
            [$child localName] $child
        }
        #incr level -1
        #sput "\}"
    }
}

rproc xsd::generateAttributes {_atts} {
    incr level
    dict for {ns nsattdata} $_atts {
        foreach name [dict keys $nsattdata] {
            if {$ns eq ""} {
                set start "attribute $name [dict get $nsattdata $name quant]"
            } else {
                set start "nsattribute $name $ns [dict get $nsattdata $name quant]"
            }
            set content \
                [split [string trim [dict get $nsattdata $name content]] "\n"]
            if {[llength $content] > 1} {
                sput "$start \{"
                incr level
                foreach line $content {
                    sput $line
                }
                incr level -1
                sput "\}"
            } else {
                set thiscontent [lindex $content 0]
                if {[string first " " $thiscontent] > 0} {
                    sput "$start \{$thiscontent\}"
                } else {
                    sput "$start $thiscontent"
                }
            }
        }
    }
    incr level -1
}

rproc xsd::element {node} {
    variable xsddata
    variable targetNS
    variable atts
    
    if {[$node hasAttribute ref]} {
        lassign [resolveFQ [$node @ref] $node] ns name
        if {$targetNS eq $ns} {
            sput "element $name [getQuant $node]"
        } else {
            sput "namespace [prefix $ns] \{"
            incr level
            sput "element $name [getQuant $node]"
            incr level -1
            sput "\}"
        }
        return
    }
    saveReset result atts
    #sputnnl "element [$node @name] [getQuant $node] "
    elementWorker $node
    restoreSaved 1
    if {$_result eq ""} {
        if {$_atts eq ""} {
            sput "element [$node @name] [getQuant $node]"
        } else {
            sput "element [$node @name] [getQuant $node] \{"
            generateAttributes $_atts
            sput "\}"
        }
    } else {
        sput "element [$node @name] [getQuant $node] \{"
        generateAttributes $_atts
        incr level
        sput $_result
        incr level -1
        sput "\}"
    }
}

rproc xsd::simpleType {node} {
    set openbrace 0
    if {[[$node parentNode] localName] eq "element"} {
        set openbrace 1
        sput "text \{"
        incr level
    }
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    if {$openbrace} {
        incr level -1
        sput "\}"
    }
}

rproc xsd::complexType {node} {
    if {[$node @mixed 0]} {
        foreach child [$node selectNodes xsd:*] {
            switch [$child localName] {
                "attribute" -
                "attributeGroup" {
                    [$child localName] $child
                }
                "choice" {
                    set quant [getQuant $child]
                    if {$quant eq "*"} {
                        sput "mixed \{"
                    } else {
                        sput "mixed $quant \{"
                    }
                    incr level
                    foreach childchild [$child selectNodes xsd:*] {
                        [$childchild localName] $childchild
                    }
                    incr level -1
                    sput "\}"
                }
                "sequence" {
                    foreach childchild [$child selectNodes xsd:*] {
                        [$childchild localName] $childchild
                    }
                }
                default {
                    [$child localName] $child
                }
            }
        }
        if {![$node selectNodes {
            count(xsd:*[local-name() != 'annotation'
                        and local-name() != 'attribute'
                        and local-name() != 'attributeGroup'])
        }]} {
            # mixed complexType with attributes only
            sput text
        }
        foreach child [$node selectNodes {
            xsd:*[local-name() = 'attribute' or local-name() = 'attributeGroup']
        }] {
            [$child localName] $child
        }
    } else {
        foreach child [$node selectNodes xsd:*] {
            [$child localName] $child
        }
    }
}

rproc xsd::simpleContent {node} {
    sput "text \{"
    incr level
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    incr level -1
    sput "\}"
}

rproc xsd::complexContent {node} {
    set mixed [$node @mixed 0]
    if {$mixed} {
        sput "mixed \{"
        incr level
    }
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    if {$mixed} {
        incr level -1
        sput "\}"
    }
}

rproc xsd::extension {node} {
    variable xsddata
    variable currentBase
    
    set savedCurrentBase $currentBase
    set currentBase [$node @base $currentBase]
    if {[string first : $currentBase] > -1} {
        lassign [split $currentBase :] prefix type
    } else {
        set type $currentBase
        set prefix ""
    }
    set typens [nsfromprefix $node $prefix]
    set context [[$node parentNode] localName] 
    switch $context {
        "simpleContent" {
            if {![dict exists $xsddata namespace $typens simpleType $type]} {
                sputce "simpleType $type in $typens not found for extension"
                return
            }
            set xsd [dict get $xsddata namespace $typens simpleType $type xsd]
        }
        "complexContent" {
            if {![dict exists $xsddata namespace $typens complexType $type]} {
                sputce "complexType $type in $typens not found for extension"
                return
            }
            set xsd [dict get $xsddata namespace $typens complexType $type xsd]
            
        }
    }
    foreach child [$xsd selectNodes xsd:*] {
        [$child localName] $child
    }
    set currentBase $savedCurrentBase
    sputce "extension not implemented"
}

rproc xsd::key {node} {
    sputce "key not implemented"
}

rproc xsd::keyref {node} {
    sputce "keyref not implemented"
}

rproc xsd::unique {node} {
    sputce "unique not implemented"
}

rproc xsd::openContent {node} {
    sputce "openContent not implemented"
}

rproc xsd::any {node} {
    set quant [getQuant $node]
    # Could be only annotation, in valid schema
    foreach child [$node selectNodes xsd:*] {
        sput [[$child localName] $child]
    }
    set processContents [$node @processContents "strict"]
    if {$processContents ne "skip"} {
        sputce "Loading foreign schemas is not supported."
        sputce "Therefor the any processContents attribute value\
                              \"$processContents\" will be effectively\
                              the same as \"skip\"\n"
    }
    set namespace [$node @namespace "##any"]
    switch $namespace {
        "##any" {
            sput "any $quant"
        }
        "##other" {
            sputce "The any namespace attribute value\
                           \"##other\" not supported.\n"
            sputce  "Translated into \"##any\"\n"
            sput "any $quant"
        }
        default {
            set nsonly {}
            foreach ns $namespace {
                switch $ns {
                    "##targetNamespace" {
                        
                    }
                    "##local" {
                        
                    }
                    default {
                        lappend nsonly $ns
                    }
                }
            }
            if {[llength $nsonly]} {
                if {[llength $nsonly] == 1} {
                    sput "any [lindex $nsonly 0] $quant"
                } else {
                    sput "choice $quant \{\n"
                    incr level
                    foreach ns $nsonly {
                        sput "any $ns\n"
                    }
                    incr level -1
                    sput "\}"
                }
            }
        }
    }
}

rproc xsd::anyAttribute {node} {
    variable atts
    sputce "anyAttribute not implemented"
    dict set atts "" dummyAttForAny content ""
    dict set atts "" dummyAttForAny quant ?
}

rproc xsd::attributeGroup {node} {
    variable xsddata
    variable attgroupStack
    variable targetNamespace
    
    # The limbo dance with attgroupStack is necessary because xsd 1.1
    # allows a recursive attribute reference to itself (xsd 1.0 does
    # not).
    if {[$node hasAttribute ref]} {
        lassign [resolveFQ [$node @ref] $node 1] ns name
        if {"${ns}:$name" in $attgroupStack} {
            return
        }
        set node [dict get $xsddata namespace $ns attributeGroup $name xsd]
    } else {
        lassign [resolveFQ [$node @name] $node 1] ns name
    }
    lappend attgroupStack ${ns}:$name
    foreach child [$node selectNodes xsd:*] {
        [$child localName] $child
    }
    set attgroupStack [lrange $attgroupStack 0 end-1]
}

rproc xsd::textType {node {type ""}} {
    variable xsddata
    
    if {$type eq ""} {
        set type [$node @type]
    }
    lassign [resolveFQ $type $node] typens typename
    # Check for basic data type
    if {$typens eq "http://www.w3.org/2001/XMLSchema"} {
        sput [mapXsdTypeToSchema $typename]
    } else {
        if {$typens eq ""} {
            sput "type $typename"
        } else {
            sput "type [prefix $typens]:$typename"
        }
    }
}    

rproc xsd::attribute {node} {
    variable xsddata
    variable targetNS
    variable atts

    saveReset result
    set prohibited 0
    switch [$node @use "optional"] {
        "prohibited" {
            set prohibited 1
        }
        "optional" {
            set quant ?
        }
        "required" {
            set quant !
            
        }
    }
    if {[$node hasAttribute ref]} {
        lassign [resolveFQ [$node @ref] $node 1] ns name
        set node [dict get $xsddata namespace $ns attribute $name xsd]
    } else {
        lassign [resolveFQ [$node @name] $node 1] ns name
    }
    if {$ns eq $targetNS} {
        set ns ""
    }
    if {$prohibited} {
        if {[dict exists atts $ns $name]} {
            dict delete atts $ns $name
        }
        restoreSaved
        return
    }
    if {[$node hasAttribute fixed]} {
        dict set atts $ns $name content "fixed [::list [$node @fixed]]"
        dict set atts $ns $name quant $quant
        restoreSaved
        return
    }
    if {[$node hasAttribute type]} {
        textType $node 
    } else {
        foreach child [$node selectNodes xsd:*] {
            [$child localName] $child
        }
    }
    dict set atts $ns $name content $result
    dict set atts $ns $name quant $quant
    restoreSaved
}

proc xsd::notation {node} {
    return "# LOOK_AT: notation not supported"
}

proc xsd::processGlobalGroup {} {
    variable xsddata
    variable targetNS
    variable level
    
    incr level
    dict for {targetNS nsdata} [dict get $xsddata namespace] {
        if {![dict exists $nsdata group]} continue
        dict for {group data} [dict get $nsdata group] {
            set xsd [dict get $data xsd]
            set result "\ndefpattern ($group) [prefix $targetNS] \{\n"
            foreach child [$xsd selectNodes xsd:*] {
                [$child localName] $child
            }
            append result "\}"
            out
        }
    }
    incr level -1
}

proc xsd::processGlobalComplexTypes {} {
    variable xsddata
    variable targetNS
    variable level
    variable atts

    # We need two passes for the complex types. The first, to get
    # the attribute definitions of the complex type, which are stored
    # in the xsddata dict. The second pass writes the defpattern (and
    # any element used by the complexType, which itself refers to a
    # complexType via the type attribute can be written with ref and
    # the now know attribute definitions.

    dict for {targetNS nsdata} [dict get $xsddata namespace] {
        if {![dict exists $nsdata complexType]} continue
        dict for {complexType ctdata} [dict get $nsdata complexType] {
            set atts ""
            set result ""
            set xsd [dict get $ctdata xsd]
            foreach child [$xsd selectNodes xsd:*] {
                [$child localName] $child
            }
            dict set xsddata namespace $targetNS complexType $complexType atts $atts
        }
    }
    
    incr level
    dict for {targetNS nsdata} [dict get $xsddata namespace] {
        if {![dict exists $nsdata complexType]} continue
        dict for {complexType ctdata} [dict get $nsdata complexType] {
            set atts ""
            set xsd [dict get $ctdata xsd]
            set result "defpattern $complexType [prefix $targetNS] \{\n"
            foreach child [$xsd selectNodes xsd:*] {
                [$child localName] $child
            }
            append result "\}"
            out
        }
    }
    incr level -1
}

proc xsd::processGlobalElements {} {
    variable xsddata
    variable targetNS
    variable level
    variable atts
    
    incr level
    dict for {targetNS nsdata} [dict get $xsddata namespace] {
        if {![dict exists $nsdata element]} continue
        dict for {element elmdata} [dict get $nsdata element] {
            set atts ""
            set xsd [dict get $elmdata xsd]
            set result ""
            elementWorker $xsd
            set storedContent $result
            set result "defelement $element "
            if {$targetNS ne ""} {
                append result "[prefix $targetNS] "
            }
            append result "\{\n"
            generateAttributes $atts
            append result $storedContent
            append result "\}\n"
            out 1
        }
    }
}

proc xsd::generateSchema {file} {
    variable level 0
    variable targetNS ""
    variable mainTargetNS ""
    variable basedir
    variable xsddata  [dict create]
    variable output
    variable schema ""
    variable currentBase ""
    variable atts ""
    variable redefining 0
    variable schemadocs ""
    variable attgroupStack ""
    variable nrLookAt 0
    
    set basedir [file dirname [file normalize [lindex $file 0]]]
    set xsddoc [dom parse [xmlReadFile [lindex $file 0]]]
    lappend schemadocs $xsddoc
    $xsddoc selectNodesNamespaces {
        xsd "http://www.w3.org/2001/XMLSchema"
    }
    set xsd [$xsddoc documentElement]
    set targetNS [$xsd @targetNamespace ""]
    set mainTargetNS $targetNS
    
    xsd::prolog
    xsd::processToplevel $xsd
    if {[dict exists $xsddata namespace]} {
        xsd::processNamespaces
        xsd::processGlobalSimpleTypes
        xsd::processGlobalGroup
        xsd::processGlobalComplexTypes
        xsd::processGlobalElements
    }
    foreach doc $schemadocs {
        $doc delete
    }
    if {$output eq "collect"} {
        return $schema
    }
}

foreach {ref localCopy} {
    http://www.w3.org/2001/xml.xsd xml.xsd
    http://www.w3.org/XML/2008/06/xlink.xsd xlink.xsd
} {
    set xsd::localCopies($ref) [file join [file dir [info script]] $localCopy]
}

foreach {namespace localCopy} {
    http://www.w3.org/XML/1998/namespace xml.xsd
    http://www.w3.org/1999/xlink xlink.xsd
} {
    set xsd::knownNamespaces($namespace) [file join [file dir [info script]] $localCopy]
}

set xsd::standalone 0
if {[info exists argv0] && [file tail [info script]] eq [file tail $argv0]} {
    if {$argc != 1} {
        puts stderr "Usage: $argv0 <xsd-file>"
        exit 1
    }
    set ::xsd::output ""
    xsd::generateSchema [lindex $argv 0]
    set xsd::standalone 1
} 
