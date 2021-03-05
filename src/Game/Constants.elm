module Game.Constants exposing (consonants, largeNumberLimit, largeNumbers, letterLimit, numberLimit, smallNumbers, vowels)


consonants : List String
consonants =
    [ "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z" ]


vowels : List String
vowels =
    [ "a", "e", "i", "o", "u" ]


largeNumbers : List Int
largeNumbers =
    [ 25, 50, 75, 100 ]


smallNumbers : List Int
smallNumbers =
    [ 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 ]


letterLimit : Int
letterLimit =
    9


largeNumberLimit : Int
largeNumberLimit =
    4


numberLimit : Int
numberLimit =
    6
