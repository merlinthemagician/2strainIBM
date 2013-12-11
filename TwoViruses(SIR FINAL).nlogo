;; Two-strain model - Extension of the Virus model by Uri Wilenski developed by Samantha Ireland and Ivo Siekmann
turtles-own
  [ sicka?           ;; turtles infected with disease A
    sickb?           ;; turtles infected with disease B
    immune?          ;; turtles are immune to both diseases
    sick-count       ;; how long a turtle has been infected
    age ]            ;; how old the turtle is in weeks

globals
[
  totalsus           ;; susceptibles
  totalinfa          ;; infected (strain a)
  totalinfb          ;; infected (strain b)
  totalimm           ;; immune
  totalpop           ;; total population
  
  %infecteda         ;; what percent of the population has infection A  
  %infectedb         ;; what percent of the population has infection B
  %susceptible       ;; what percent of the population is healthy and susceptible
  %immune            ;; what percent of the population is immune
  lifespan           ;; average lifespan of turtles
  average-offspring  ;; average amount of offspring a turtle can have  
  carrying-capacity  ;; carrying capacity of world  
  
  tofile?            ;; shall we print to file?
]

to create-file
  
  let file user-new-file
  ;; We check to make sure we actually got a string just in case
  ;; the user hits the cancel button.
  if is-string? file
  [
    ;; If the file already exists, we begin by deleting it, otherwise
    ;; new data would be appended to the old contents.
    if file-exists? file
      [ file-close-all ;; better make sure nothing is open
        file-delete file 
        ]
    file-open file
    set tofile? true
    
    let ftitle  "Tick S Ia Ib R P S% Ia% Ib% R%"
    let parstring1 (word "# Parameters: infectiousness_a = " infectiousness_a "% , infectiousness_b = " infectiousness_b "%")
    let parstring2 (word "# chance-recover_a = " chance-recover_a "% , chance-recover_b = " chance-recover_b "%")
    let parstring3 (word "# duration_a = " duration_a ", duration_b = " duration_b)
    let parstring4 (word "# initial population = " people)
    file-print ftitle
    file-print parstring1
    file-print parstring2
    file-print parstring3
    file-print parstring4
  ]
end

to file-print-populations
  let out (word ticks " " totalsus " " totalinfa " " totalinfb " " totalimm " " totalpop " " %susceptible " " %infecteda " " %infectedb " " %immune)
  file-print out
end

to setup
  ca
  random-seed seed
  setup-constants
  setup-turtles
  update-global-variables
  reset-ticks
  ifelse fileoutput [create-file] [set tofile? false]
end

to setup-turtles     ;; setup-turtles creates 150 people, 5 of which have infection A, and 5 which have infection B
  set-default-shape turtles "person"
  crt people
    [ setxy random-xcor random-ycor
      set age random lifespan
      set sick-count 0
      set immune? false
      set size 1.5
      get-healthy ]
  ask n-of 5 turtles
    [ get-sicka ]
  ask n-of 5 turtles
    [ get-sickb ]  
end

to get-sicka ;; turtle procedure
  set sicka? true
  set sickb? false
  set immune? false
  set color red
end

to get-sickb ;; turtle procedure
  set sickb? true
  set sicka? false
  set immune? false
  set color yellow
end
  
to get-healthy ;; turtle procedure
  set sicka? false
  set sickb? false
  set immune? false
  set sick-count 0
  set color green
end

to become-immune ;; turtle procedure
  set sicka? false
  set sickb? false
  set sick-count 0
  set immune? true
  set color gray
end

to get-sickAny [sick-a?] ;; get-sickAny tests if infecting turtle is infected by disease A
  ifelse sick-a? [if (not sickb?) and ((random-float 100) < infectiousness_a) [get-sicka]] 
  [if (not sicka?) and (random-float 100) < infectiousness_b [get-sickb]]
end

to setup-constants
  set tofile? false
  set lifespan 100
  set carrying-capacity 750
  set average-offspring 4
end

to go
  get-older
  move
  infect
  recover
  reproduce
  update-global-variables
  if tofile? [  file-print-populations ]
  tick
end

to update-global-variables
  set totalpop (count turtles)
  set totalsus (count turtles with [not (sicka? or sickb? or immune?)])
  set totalinfa (count turtles with [sicka?])
  set totalinfb (count turtles with [sickb?])
  set totalimm (count turtles with [immune?])
    
  if totalpop > 0
  [ 
    set %infecteda totalinfa / totalpop * 100
    set %infectedb totalinfb / totalpop * 100
    set %susceptible totalsus / totalpop * 100
    set %immune totalimm / totalpop * 100
  ]
end

to get-older
  ask turtles
  [
    set age age + 1
    if sicka? or sickb?
      [ set sick-count (sick-count + 1) ]
    if age > lifespan
      [ die ]
  ]
end

to move ;; turtles move in random directions around the space
  ask turtles
  [ rt random 100
    lt random 100
    fd 1 ]
end

to infect ;; infectious turtles pass the disease onto other turtles who are not immune
  ask turtles with [sicka? or sickb?] 
  [ask other turtles-here with [not immune?]
  [get-sickAny [sicka?] of myself]
  ]
end

to recover ;; after a period of time, turtles may either recover and become immune, or die
   ask turtles with [sicka?]
     [ if (random sick-count) > (lifespan * (duration_a / 100)) 
         [ ifelse ((random-float 100) < chance-recover_a)     
             [ become-immune ]
             [ die ] ] ]

   ask turtles with [sickb?]
     [ if (random sick-count) > (lifespan * (duration_b / 100))  
         [ ifelse ((random-float 100) < chance-recover_b)        
             [ become-immune ]
             [ die ] ] ]
end

to reproduce ;; healthy and immune turtles can reproduce when the population is below the carrying-capacity
  ask turtles with [not sicka? and not sickb?]
    [ if (count turtles) < carrying-capacity
         and (random lifespan) < average-offspring
       [ hatch 1
           [ set age 1
             lt 45 fd 1
             get-healthy ] ] ]
end
;; Copyright 2013 Ivo Siekmann & Samantha Ireland
;; Extension of the Virus model by Uri Wilensky (1998), supplied with NetLogo as an example.
;; See Info tab for further details on licensing
@#$#@#$#@
GRAPHICS-WINDOW
388
22
962
562
20
18
13.76
1
10
1
1
1
0
1
1
1
-20
20
-18
18
1
1
1
ticks
30.0

SLIDER
99
131
365
164
chance-recover_a
chance-recover_a
0.0
99.0
70
1.0
1
%
HORIZONTAL

SLIDER
99
94
365
127
infectiousness_a
infectiousness_a
0.0
99.0
60
1.0
1
%
HORIZONTAL

BUTTON
219
17
289
52
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
293
17
364
52
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
17
333
365
561
Populations
Weeks
People
0.0
52.0
0.0
200.0
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot count turtles"
"healthy" 1.0 0 -14439633 true "" "plot count turtles with [not (sicka? or sickb? or immune?)]"
"infected a" 1.0 0 -2674135 true "" "plot count turtles with [sicka?]"
"infected b" 1.0 0 -1184463 true "" "plot count turtles with [sickb?]"
"immune" 1.0 0 -9276814 true "" "plot count turtles with [immune?]"

SLIDER
161
57
365
90
people
people
10
750
300
1
1
NIL
HORIZONTAL

MONITOR
16
285
87
330
%Infected A
%infecteda
1
1
11

MONITOR
311
285
365
330
Years
ticks / 52
1
1
11

INPUTBOX
15
94
96
164
duration_a
12
1
0
Number

MONITOR
165
285
242
330
% Susceptible
%susceptible
1
1
11

TEXTBOX
14
70
164
88
INFECTION A (RED)
14
15.0
1

SLIDER
101
195
366
228
infectiousness_b
infectiousness_b
0
99.0
70
1
1
%
HORIZONTAL

SLIDER
101
233
366
266
chance-recover_b
chance-recover_b
0
99.0
40
1
1
%
HORIZONTAL

INPUTBOX
17
195
98
267
duration_b
10
1
0
Number

TEXTBOX
16
171
166
189
INFECTION B (YELLOW)
14
44.0
1

MONITOR
90
285
163
330
%Infected B
%infectedb
1
1
11

MONITOR
244
285
309
330
% Immune
%immune
1
1
11

INPUTBOX
13
10
76
70
seed
666
1
0
Number

SWITCH
85
17
202
50
fileoutput
fileoutput
0
1
-1000

@#$#@#$#@
#INFECTIOUS DISEASES - TWO STRAINED

## WHAT IS IT?

This model simulates the transmission and perpetuation of two infectious diseases in a human population.

It shows how two infectious diseases may compete with each other in order to be the stronger disease. The strength of these diseases is determined by a number of different factors, including the transmissibility of the disease, the duration, and the chance that a person may recover from the disease.

## HOW IT WORKS

The model is initialized with 150 people, of which 10 are infected, 5 with infection A and 5 with infection B.  People move randomly about the world in one of three states: healthy but susceptible to infection (green), sick and infectious with Infection A (red), or Infection B (yellow), or immune (gray). People may die of infection or old age.  When the population dips below the environment's "carrying capacity" (set at 750 in this model) healthy people may reproduce healthy and susceptible offspring.

Some of these factors are summarized below with an explanation of how each one is treated in this model.

### The density of the population

Population density affects how often infected and susceptible individuals come into contact with each other. You can change the size of the initial population through the PEOPLE slider.

### Population turnover

As individuals die, some who die will be infected, some will be immune, and some will be susceptible.  All the new individuals who are born, replacing those who die, will be susceptible.  People may die from an infectious disease, the chances of which are determined by the sliders CHANCE-RECOVER_A and CHANCE-RECOVER_B, or they may die of old age.  In this model, people die of old age at the age of approximately 100 weeks.  Reproduction rate is constant in this model.  Each turn, every healthy individual has a chance to reproduce.  That chance is set so that each person will on average reproduce four times if they live 100 weeks.

### Infectiousness

How easily does the infectious disease spread?  Some viruses with which we are familiar spread very easily.  Some infectious diseases spread from the smallest contact every time.  Others (the HIV virus, which is responsible for AIDS, for example) require significant contact, perhaps many times, before the virus is transmitted.  In this model, infectiousness is determined by a slider.

### Duration of infectiousness

How long is a person infected before they either recover or die?  This length of time is essentially the diseases's window of opportunity for transmission to new hosts. In this model, duration of infectiousness is determined by a slider.

## HOW TO USE IT

Each "tick" represents a week in the time scale of this model.

The INFECTIOUSNESS sliders determine how great the chance is that virus transmission will occur when an infected person and susceptible person occupy the same patch.  For instance, when a slider is set to 50, the virus will spread roughly once every two chance encounters.

The DURATION boxes determine the percent of the average life-span (which is 100 weeks in this model) that an infected person goes through before the infection ends in either death or recovery.  Note that although zero is a slider possibility, it produces an infection of very short duration (approximately 2 weeks) not an infection with no duration at all.

The CHANCE-RECOVERY sliders control the likelihood that an infection will end in recovery.  When this slider is set at zero, for instance, the infection is always deadly.

The SETUP button resets the graphics and plots and randomly distributes 140 green susceptible people and 10 red and yellow infected people (of randomly distributed ages). The GO button starts the simulation and the plotting function.

Four output monitors show the percent of the population that is infected with Infection A, the percent of the population that is infected with Infection B, the percent that is susceptible, and the percent that is immune.  The plot shows (in their respective colors) the number of susceptible, immune, and infected people.  It also shows the number of individuals in the total population in black.

## THINGS TO NOTICE

The factors controlled by the three sliders interact to influence how likely a virus is to thrive in this population.  Notice that in all cases, these factors must create a balance in which an adequate number of potential hosts remain available to the virus and in which the virus can adequately access those hosts. Notive how both infections compete with each other, what factors allow an infectious disease to survive?

Often there will initially be an explosion of infection since the population density is at its maximum.  This approximates the initial "outbreak" of a viral infection in a population, one that often has devastating consequences for the humans concerned. Soon, however, the viruses becomes less common as the population dynamics change.  What ultimately happens to the viruses is determined by the factors controlled by the sliders.

Notice that viruses that are too successful at first (infecting almost everyone) may not survive in the long term.  Since everyone infected generally dies as a result, the potential number of hosts is often limited.  The exception to the above is when the DURATION slider is set so high that population turnover (reproduction) can keep up and provide new hosts.

## THINGS TO TRY

Think about how different slider values might approximate the dynamics of real-life viruses.  The famous Ebola virus in central Africa has a very short duration, a very high infectiousness value, and an extremely low recovery rate. For all the fear this virus has raised, how successful is it?  Set the sliders appropriately and watch what happens.

The HIV virus which causes AIDS, has an extremely long duration, an extremely low recovery rate, but an extremely low infectiousness value.  How does a virus with these slider values fare in this model?

When competing with each other, which infectious disease appears to be stronger, the Ebola virus or the HIV virus?

## License

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

The code is an extension of the Virus model by Uri Wilensky, supplied with the NetLogo software.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
