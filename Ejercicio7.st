  7- a) Crear la clase Ecuación y definir los métodos necesarios para resolver una ecuación de segundo grado en el campo real. Los coeficientes a, b y c de la ecuación se ingresan por teclado. Considerar que el coeficiente de segundo grado (a) debe ser distinto de cero. Si la ecuación no tiene raíces reales mostrar un mensaje aclaratorio.

| ecu r1 r2 |
ecu := EcuacionCuadratica new. 
ecu cargaDatos.
ecu discriminante < 0 ifTrue:[ MessageBox notify:'La ecuación no tiene solución en el campo real']
				ifFalse:[ r1:= ecu raizUno. MessageBox notify: 'La primera raíz es:', r1 printString.
						r2:= ecu raizDos. MessageBox notify: 'La segunda raíz es:', r2 printString.]

       b) Modificar el programa del ítem a) para poder resolver cuatro ecuaciones.
|contador|
contador := 0.
[contador < 4] whileTrue: [
contador := contador + 1.
ecu := EcuacionCuadratica new. 
ecu cargaDatos.
ecu discriminante < 0 ifTrue:[ MessageBox notify:'La ecuación no tiene solución en el campo real']
				ifFalse:[ r1:= ecu raizUno. MessageBox notify: 'La primera raíz es:', r1 printString.
						r2:= ecu raizDos. MessageBox notify: 'La segunda raíz es:', r2 printString.]
].

       c) Modificar el programa del ítem a) para poder resolver un número indeterminado de ecuaciones.
|n|
contador := 0.
n := (Prompter prompt: 'Ingrese un numero:') asNumber .
[contador < n] whileTrue: [
contador := contador + 1.
ecu := EcuacionCuadratica new. 
ecu cargaDatos.
ecu discriminante < 0 ifTrue:[ MessageBox notify:'La ecuación no tiene solución en el campo real']
				ifFalse:[ r1:= ecu raizUno. MessageBox notify: 'La primera raíz es:', r1 printString.
						r2:= ecu raizDos. MessageBox notify: 'La segunda raíz es:', r2 printString.]
].
