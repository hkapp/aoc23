]box on
]rows on
⎕IO ← 0

td ← '1abc2' 'pqr3stu8vwx' 'a1b2c3d4e5f' 'treb7uchet'
rd←⊃⎕NGET 'data/day1.data.txt' 1

digits ← {{(⍵∊⍕⍳10)/⍵}¨⍵}
fl←{{⍵[0],⍵[1-⍨⍴⍵]}¨⍵}
d1p1 ← {+/⍎¨fl digits ⍵}
d1p1 td
d1p1 rd

td2 ← 'two1nine' 'eightwothree' 'abcone2threexyz' 'xtwone3four' '4nineeightseven2' 'zoneight234' '7pqrstsixteen'

dt ← 'zero' 'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' , ⍕¨⍳10
adiw←{w←⍵ ⋄ ⌈⌿((≢w)/20 1⍴((⍳10),(⍳10)))×↑{⍵⍷w}¨dt}
adn←{⍵[(⌊/⍸⍵),(⌈/⍸⍵)]}
d1p2 ← {+/+/((≢⍵)2⍴10 1)×↑(adn∘adiw)¨⍵}
d1p2 td2
d1p2 rd
