proc to_index {values n k} {
    set index 0
    foreach value $values {
        set index [expr $index * $n + $value]
    }
    return $index
}

proc from_index {index n k} {
    set values {}
    set remainder $index
    for {set jj 0} {$jj < $k} {incr jj} {
        set curr_digit [expr $remainder % $n]
        lappend values $curr_digit
        set remainder [expr ($remainder - $curr_digit) / $n]
    }
    return [lreverse $values]
}

proc lequal {a b} {
    set a_length [llength $a]
    set b_length [llength $b]
    if {$a_length != $b_length} { return 0 }
    for {set ii 0} {$ii < $a_length} {incr ii} {
        if {[lindex $a $ii] != [lindex $b $ii]} {
            return 0
        }
    }
    return 1
}

proc find_all_groups {n k} {
    for {set ii 0} {$ii < $n ** $k} {incr ii} {
        set discovered($ii) 0
    }
    set groups {}

    while {1} {
        set group {}

        set index -1
        for {set ii 0} {$ii < $n ** $k} {incr ii} {
            if {$discovered($ii) == 0} {
                set index $ii
                break
            }
        }
        if {$index == -1} {break}

        set curr [from_index $index $n $k]
        set start $curr

        proc record {} {
            uplevel {
                lappend group $curr
                set discovered([to_index $curr $n $k]) 1
            }
        }
        proc step {} {
            upvar curr curr n n
            set new_value [::tcl::mathop::+ {*}$curr]
            set new_value [expr $new_value % $n]
            lappend curr $new_value
            set curr [lreplace $curr 0 0]
        }

        record
        step
        while {![lequal $curr $start]} {
            record
            step
        }

        lappend groups $group
    }
    
    return $groups
}

set n [lindex $argv 0]
set k [lindex $argv 1]

set ii 0
foreach cycle [find_all_groups $n $k] {
    puts "Cycle #$ii:"

    set first 1
    foreach el $cycle {
        if {$first} {
            set first 0
            puts -nonewline " $el"
        } else {
            puts -nonewline " [lindex $el 0]"
        }
    }
    puts "\n  length: [llength $cycle]"

    incr ii
}
puts "$ii cycles found for k = $k, n = $n"

# for {set n 2} {$n <= 10} {incr n} {
#     for {set k 2} {$k <= 6} {incr k} {
#         puts -nonewline "$n $k"
#         set freqs [dict create]
# 
#         set elapsed [time {
#             set groups [find_all_groups $n $k]
#         }]
#         foreach g $groups {
#             set l [llength $g]
#             dict incr freqs $l
#         }
# 
#         foreach key [lsort -integer [dict keys $freqs]] {
#             puts -nonewline " ($key [dict get $freqs $key])"
#         }
# 
#         set total 0
#         dict for {l freq} $freqs {
#             incr total [expr $l * $freq]
#         }
#         puts " \[$total / [expr $n ** $k], $elapsed\]"
#     }
# }

# set arr [concat [string repeat "0 " 99] " 0"]
# set g [lindex [find_all_groups 10 2] 1]
# foreach i $g {
#     lset arr [to_index [lreverse $i] 10 2] 1
# }
# puts $arr

