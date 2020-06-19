type 'a set
val empty: 'a set
val singleton : 'a -> 'a set
val mem: 'a -> 'a set -> bool
val add: 'a -> 'a set -> 'a set
val union: 'a set -> 'a set -> 'a set
val intersection: 'a set -> 'a set -> 'a set
