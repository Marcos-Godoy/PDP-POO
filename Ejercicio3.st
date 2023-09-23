 3- Indicar qué se obtiene al evaluar los siguientes trozos de código con Display it:

       a) | cadena n p |
          n := p := 0.
          cadena :=  #(12 4 7 9 4 6 3 1 2 5 11).
          1 to: cadena size do: [ :i | ( cadena at:i) odd
                                                         ifTrue:[ n := n + 1 ]
                                                         ifFalse:[ p := p + 1 ]].
          n > p
               ifTrue:[ ^true ]
               ifFalse:[ ^false ]
true

       b) | cadena |
          cadena := Prompter prompt:'Cual es tu nombre?'.
          cadena at: 1 put: (cadena at:1) asUppercase.
          2 to: cadena size do:[ :j | cadena at:j put: (cadena at:j) asLowercase].
          ^cadena
Marcos
