module StopVowels where

stops = "pbtdkg"
vowels = "aeiou"

letterCombos :: [(Char, Char, Char)]
letterCombos = [(s1, v1, s2) | s1 <- stops, v1 <- vowels, s2 <- stops]

letterCombosP :: [(Char, Char, Char)]
letterCombosP = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

nouns = ["dog", "man", "chair", "cat", "poop"]
verbs = ["ate", "ran", "bled"]

wordCombos = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]
