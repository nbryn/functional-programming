module MultiSet
    type MultiSet : Map<'a, uint32> when a' : comparison

    val empty : MultiSet<'a>
    val isEmpty : MultiSet<'a> -> bool
    val size : MultiSet<'a> -> uint32

