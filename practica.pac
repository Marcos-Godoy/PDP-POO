| package |
package := Package name: 'practica'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Circunferencia;
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

"Binary Globals"!

