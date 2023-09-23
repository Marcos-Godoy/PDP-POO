| package |
package := Package name: 'practica'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alumno;
	add: #Circunferencia;
	add: #Comision;
	add: #Triangulo;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Alumno
	instanceVariableNames: 'legajo nombre nota'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Circunferencia
	instanceVariableNames: 'radio'
	classVariableNames: 'Pi'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Comision
	instanceVariableNames: 'coleccionAlumnos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Triangulo
	instanceVariableNames: 'ladoA ladoB ladoC'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Alumno guid: (GUID fromString: '{90f4c07f-4844-45cf-b02e-ac9d74977340}')!
Alumno comment: ''!
!Alumno categoriesForClass!Kernel-Objects! !
!Alumno methodsFor!

cargaDatos
"establece los datos de los alumnos"
legajo := Prompter prompt: 'Ingrese legajo'.
nombre := Prompter prompt: 'Ingrese nombre'.
nota := (Prompter prompt: 'Ingrese legajo') asNumber .
^self!

muestraLegajo
"devuelve el legajo del alumno"
^legajo!

muestraNombre
"devuelve el nombre del alumno"
^nombre!

muestraNota
"devuelve la nota del alumno"
^nota! !
!Alumno categoriesForMethods!
cargaDatos!public! !
muestraLegajo!public! !
muestraNombre!public! !
muestraNota!public! !
!

Circunferencia guid: (GUID fromString: '{0adb3001-5e1e-4104-8b98-174953184d67}')!
Circunferencia comment: ''!
!Circunferencia categoriesForClass!Kernel-Objects! !
!Circunferencia methodsFor!

calcArea
"calcula el area de una circunferencia"
^ radio squared * Pi.!

calcLongitud
"calcula el valor de la longitud"
^ 2 * Pi * radio!

iRadio
"asigna valor al radio"
radio := (Prompter prompt: 'Ingrese el valor del radio' ) asNumber.! !
!Circunferencia categoriesForMethods!
calcArea!public! !
calcLongitud!public! !
iRadio!public! !
!

!Circunferencia class methodsFor!

asignaPi
"establece el valor de Pi"
Pi := Float pi.! !
!Circunferencia class categoriesForMethods!
asignaPi!public! !
!

Comision guid: (GUID fromString: '{2ef28085-2633-482c-ba46-e7d5dd83f09e}')!
Comision comment: ''!
!Comision categoriesForClass!Kernel-Objects! !
!Comision methodsFor!

calculaPromedio!

cargaAlumnos!

consulta
|leg aux|
leg:= Prompter prompt: 'Ingrese el numero de legajo del alumno'.
aux := coleccionAlumnos detect:[:i | i muestraLegajo = leg]
ifNone:[ aux := nil].!

inicio
"contener los mensajes que resuelvan aquellas cuestiones
necesarias para arrancar con la ejecución."

coleccionAlumnos := OrderedCollection new.
!

listaA
|aux|
aux:= coleccionAlumnos select:[:i | i muestraNota > self calculaPromedio].!

listaB!

listaC

|aux|
aux:= coleccionAlumnos asSortedCollection: [:a :b | a muestraNota> b muetraNota].!

menu

|op rta|
rta:= true.
[rta] whileTrue: [ op:= Prompter prompt: '1- lista1 2- consulta 3- salir'.
op=='1' ifTrue:[ self listaA ].
op=='2' ifTrue:[ self consulta].
op=='3' ifTrue:[ rta:= false]].! !
!Comision categoriesForMethods!
calculaPromedio!public! !
cargaAlumnos!public! !
consulta!public! !
inicio!public! !
listaA!public! !
listaB!public! !
listaC!public! !
menu!public! !
!

Triangulo guid: (GUID fromString: '{3ae30f3f-5623-4f04-9710-80dfc804065b}')!
Triangulo comment: ''!
!Triangulo categoriesForClass!Kernel-Objects! !
!Triangulo methodsFor!

cargaDatos
"carga los tres lados de un triangulo"
ladoA := (Prompter prompt: 'Ingrese el primer lado' ).
ladoB := (Prompter prompt: 'Ingrese el segundo lado' ).
ladoC := (Prompter prompt: 'Ingrese el tercer lado' ).!

clasificar
"Clasifica a un triangulo"
((ladoA = ladoB) and: [ladoB = ladoC ] )
ifTrue: [^'Equilatero']
ifFalse: [(ladoA = ladoB or: [(ladoB = ladoC) or: [ladoA = ladoC ] ] )
	ifTrue: [^'Isosceles']
	ifFalse: [^'Escaleno'].
].! !
!Triangulo categoriesForMethods!
cargaDatos!public! !
clasificar!public! !
!

"Binary Globals"!

