]box on
]rows on
⎕IO ← 0

td←⊃⎕NGET 'data/day4.test.txt' 1
rd←⊃⎕NGET 'data/day4.data.txt' 1

digits←∊⍕¨⍳10
extract←{1↓⍵⊆⍨⍵∊'|',digits}
split←{⍎¨¨b⊆⍨⊃¨~'|'=b←extract ⍵}
wnums←{(⊃d)∩⊃1↓d←split ⍵}
value←{⌊2*(≢wnums ⍵)-1}

d4p1←{+/value¨⍵}
d4p1 td
d4p1 rd

scmap←{(1+⍳≢⍵)+⍳¨≢¨wnums¨⍵}
dpread ← (+/⍺[recsc])+≢recsc←⊃g[⍵]}
dprec ← {⍵<0:⍺ ⋄ (((⍺ dpread ⍵)@⍵)⍺) ∇ ⍵-1}
dprun ← {g←(1+⍳≢⍵)+⍳¨≢¨wnums¨⍵ ⋄ ((≢⍵)/0) dprec 1-⍨≢⍵}

d4p2 ← {+/1+dprun ⍵}
g ← scmap td
d4p2 td
g ← scmap rd
d4p2 rd
