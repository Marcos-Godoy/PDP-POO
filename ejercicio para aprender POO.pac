| package |
package := Package name: 'ejercicio para aprender POO'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alumno;
	add: #Docente;
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
Persona subclass: #Alumno
	instanceVariableNames: 'fechaInscripcion carrera'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Docente
	instanceVariableNames: 'fechaAlta cargo'
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

Alumno guid: (GUID fromString: '{dc75323c-84f7-419a-a6a9-24c23d8358e2}')!
Alumno comment: ''!
!Alumno categoriesForClass!Kernel-Objects! !
!Alumno methodsFor!

carrera
^ carrera!

carrera: unString
carrera := unString!

diasEstudiando
^ Date today subtractDate: fechaInscripcion
!

fechaInscripcion
^ fechaInscripcion!

fechaInscripcion: unaFecha
fechaInscripcion := unaFecha!

saludar
^ Transcript show: 'Hola, mi nombre es ', nombre, 'y estudio la carrera ', carrera.
! !
!Alumno categoriesForMethods!
carrera!public! !
carrera:!public! !
diasEstudiando!public! !
fechaInscripcion!public! !
fechaInscripcion:!public! !
saludar!public! !
!

Docente guid: (GUID fromString: '{dd2f9715-47d6-434e-92d2-004e2be83c93}')!
Docente comment: ''!
!Docente categoriesForClass!Kernel-Objects! !
!Docente methodsFor!

antiguedad
^ Date today yearsSince: fechaAlta!

cargo
^ cargo!

cargo: unString
cargo := unString!

fechaAlta
^ fechaAlta!

fechaAlta: unaFecha
fechaAlta := unaFecha!

saludar
^ Transcript show: 'Hola, mi nombre es ', nombre, ' y soy ', cargo, ' desde hace', self antiguedad, ' años'.! !
!Docente categoriesForMethods!
antiguedad!public! !
cargo!public! !
cargo:!public! !
fechaAlta!public! !
fechaAlta:!public! !
saludar!public! !
!

"Binary Globals"!

