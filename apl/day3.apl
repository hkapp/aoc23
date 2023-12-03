]box on
]rows on
⎕IO ← 0

td←⊃⎕NGET 'data/day3.test.txt' 1
rd←⊃⎕NGET 'data/day3.data.txt' 1

digits←∊⍕¨⍳10
proxmat ← {(1@(↓⍵))0⍴⍨⍴↑⍺}
flatmask ← {∨/¨(∊⍺ proxmat ⍵)⊆⍨∊(↑⍺)∊digits}

symbols←{~(↑⍵)∊'.',digits}
sq←∘.,⍨ ¯1 0 1
sqsyms←{d+(↑sq)⍴⍨⍴d←9⌿↑⍸symbols ⍵}
kept ← {⍵ flatmask (sqsyms ⍵)}
allnums←{⍎¨(∊↑⍵)⊆⍨∊(↑⍵)∊digits}

d3p1 ← {+/(kept ⍵)/allnums ⍵}
d3p1 td
d3p1 rd

gears←{(↑⍵)∊'*'}
gsq←{(9 2⍴⍵)+9 2⍴↑sq}
gneighs←{⍺ flatmask (gsq ⍵)}
gkept ← {m×(⍴m)⍴2=+/m←⍺ gneighs ⍵}
gvals ← {(⍺ gkept ⍵)/allnums ⍺}

d3p2←{+/×/↑(⍵ gvals ⊢)¨⍸gears ⍵}
d3p2 td
d3p2 rd
