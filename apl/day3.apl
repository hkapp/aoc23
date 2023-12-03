]box on
]rows on
⎕IO ← 0

td←⊃⎕NGET 'data/day3.test.txt' 1
rd←⊃⎕NGET 'data/day3.data.txt' 1

digits←∊⍕¨⍳10
symbols←{~(↑⍵)∊'.',digits}
sq←∘.,⍨ ¯1 0 1
sqsyms←{d+(↑sq)⍴⍨⍴d←9⌿↑⍸symbols ⍵}
enabled←{(1@(↓sqsyms ⍵))0⍴⍨⍴↑⍵}
kept ← {∨/¨(∊enabled ⍵)⊆⍨∊(↑⍵)∊digits}
allnums←{⍎¨(∊↑⍵)⊆⍨∊(↑⍵)∊digits}

d3p1 ← {+/(kept ⍵)/allnums ⍵}
d3p1 td
d3p1 rd

gears←{(↑⍵)∊'*'}
gsq←{(9 2⍴⍵)+9 2⍴↑sq}
gena ← {(1@(↓gsq ⍵))0⍴⍨⍴↑⍺}
gneighs←{∨/¨(∊⍺ gena ⍵)⊆⍨∊(↑⍺)∊digits}
gkept ← {m×(⍴m)⍴2=+/m←⍺ gneighs ⍵}
gvals ← {(⍺ gkept ⍵)/allnums ⍺}

d3p2←{+/×/↑(⍵ gvals ⊢)¨⍸gears ⍵}
d3p2 td
d3p2 rd
