| package |
package := Package name: 'practica'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Circunferencia;
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

Object subclass: #Circunferencia
	instanceVariableNames: 'radio'
	classVariableNames: 'Pi'
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

