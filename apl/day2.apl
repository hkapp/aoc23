]box on
]rows on
⎕IO ← 0

td ← 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green' 'Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue' 'Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red' 'Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red' 'Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green'
rd←⊃⎕NGET 'data/day2.data.txt' 1

ccnt ← {⍎¨1↓⍵⊆⍨∨⌿↑(⍵∊⊢)¨⍕¨⍳10}
cval ← {⌈⌿↑(12×'red' ⍷ ⍵) (13×'green' ⍷ ⍵) (14×'blue' ⍷ ⍵)}
ccol ← {c ← cval ⍵ ⋄ c[⍸0≠c]}
impgames ← {∨/(↑ccnt¨⍵)>↑ccol¨⍵}
posgames ← ~impgames

d2p1 ← {+/e×1+⍳≢e←posgames ⍵}
d2p1 td
d2p1 rd

colmat ← {⊃¨g⊆⍨∨⌿g←↑('red' ⍷ ⍵) ('green' ⍷ ⍵) ('blue' ⍷ ⍵)}
chancnt←{3 (≢j)⍴j←ccnt ⍵}

d2p2←{+/×/⌈/(↑chancnt¨⍵)×↑colmat¨⍵}
d2p2 td
d2p2 rd
