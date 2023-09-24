| package |
package := Package name: 'ejercicio para aprender POO'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Persona;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time').

package!

"Class Definitions"!

Object subclass: #Persona
	instanceVariableNames: 'nombre genero fechaNacimiento estadoCivil'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Persona guid: (GUID fromString: '{cc8b39b6-4125-4a04-baed-0a6987d6c996}')!
Persona comment: ''!
!Persona categoriesForClass!Kernel-Objects! !
!Persona methodsFor!

casado
estadoCivil := 'Casado/a'.
!

divorciado
estadoCivil := 'Divorciado/a'
!

edad
^ Date today yearsSince: fechaNacimiento.!

edadEn: anio
^ anio - fechaNacimiento year!

esMayorDeEdad
^ self edad >= 18!

estadoCivil
^ estadoCivil!

fechaNacimiento
^ fechaNacimiento!

fechaNacimiento: unaFecha
fechaNacimiento := unaFecha!

genero
^ genero!

genero: unString
genero := unString!

mayorQue: otraPersona
^ fechaNacimiento > otraPersona fechaNacimiento!

nombre
^ nombre!

nombre: unString
nombre := unString!

saludar
Transcript show: 'Hola, mi nombre es ', nombre, '!!'.!

soltero
estadoCivil := 'Soltero/a'.
! !
!Persona categoriesForMethods!
casado!public! !
divorciado!public! !
edad!public! !
edadEn:!public! !
esMayorDeEdad!public! !
estadoCivil!public! !
fechaNacimiento!public! !
fechaNacimiento:!public! !
genero!public! !
genero:!public! !
mayorQue:!public! !
nombre!public! !
nombre:!public! !
saludar!public! !
soltero!public! !
!

"Binary Globals"!

