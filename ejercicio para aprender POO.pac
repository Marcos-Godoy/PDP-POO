| package |
package := Package name: 'ejercicio para aprender POO'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alumno;
	add: #Docente;
	add: #Materia;
	add: #Persona;
	add: #Universidad;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Materia
	instanceVariableNames: 'nombre anioCursado horario nota'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Persona
	instanceVariableNames: 'nombre genero fechaNacimiento estadoCivil'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Universidad
	instanceVariableNames: 'alumnos docentes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Alumno
	instanceVariableNames: 'fechaInscripcion carrera materias legajo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Docente
	instanceVariableNames: 'codigo fechaAlta cargo'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Materia guid: (GUID fromString: '{99af4f86-fce2-40c5-98b0-0398ebed4f03}')!
Materia comment: ''!
!Materia categoriesForClass!Kernel-Objects! !
!Materia methodsFor!

anioCursado
^ anioCursado!

anioCursado: unEntero
anioCursado := unEntero!

horario
^ horario!

horario: unString
horario := unString!

nombre
^ nombre!

nombre: unString
nombre := unString!

nota
^ nota!

nota: unNumero
nota := unNumero! !
!Materia categoriesForMethods!
anioCursado!public! !
anioCursado:!public! !
horario!public! !
horario:!public! !
nombre!public! !
nombre:!public! !
nota!public! !
nota:!public! !
!

Persona guid: (GUID fromString: '{cc8b39b6-4125-4a04-baed-0a6987d6c996}')!
Persona comment: ''!
!Persona categoriesForClass!Kernel-Objects! !
!Persona methodsFor!

cargaDatos
|estado|
nombre := Prompter prompt: 'Ingrese nombre de la persona'.
genero := Prompter prompt: 'Ingrese genero'.
fechaNacimiento := Date fromString: (Prompter prompt: 'Ingrese fecha de Nacimiento DD/MM/YYYY').
estado := (Prompter prompt: 'Ingrese estado civil (C/D/S)') asUppercase.
estado = 'C'
ifTrue: [ self casado ]
ifFalse: [ estado = 'D'
ifTrue: [ self divorciado ]
ifFalse: [ self soltero ]].!

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
cargaDatos!public! !
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

Universidad guid: (GUID fromString: '{7e645b28-6351-46ef-b2ea-7caba809db21}')!
Universidad comment: ''!
!Universidad categoriesForClass!Kernel-Objects! !
!Universidad methodsFor!

altaDocente
|docente|
docente := Docente new.
docente cargaDatos.
docentes add: docente.!

inicializa
alumnos := OrderedCollection new.
docentes := OrderedCollection new.
Docente inicializaUltimoCodigo.!

inscribirAlumno
|alumno|
alumno := Alumno new.
alumno inicializa.
alumno cargaDatos.
alumnos add: alumno.
!

listadoAlumnos
|alumnosOrd|
alumnosOrd := alumnos asSortedCollection: [ :unAlumno :otroAlumno | unAlumno promedio > unAlumno promedio ].
Transcript show: 'Listado de todos los alumnos ordenado por promedio ascendente'; cr.
alumnosOrd do: [:unAlumno | Transcript show: 'Nombre: ', unAlumno nombre, ' - promedio: ', unAlumno promedio printString; cr ].
!

listadoDocentes
|docentesOrd|
docentesOrd := docentes asSortedCollection: [ :unDocente :otroDocente | unDocente nombre > unDocente nombre].
Transcript show: 'Listado de docentes ordenado por nombre'; cr.
docentesOrd do: [:unDocente | Transcript show: 'Nombre: ', unDocente nombre, ' - cargo: ', unDocente cargo; cr ].
!

menu
| op |
op := 5.
[ op = 0 ] whileFalse: [
MessageBox notify: 'MENU:
1- Inscribir alumno
2- Alta docente
3- Listado de alumnos
4- Listado de docentes
0- Salir'.
op:= (Prompter prompt:'Ingrese opción:') asNumber
asInteger.
( op = 1 ) ifTrue:[ self inscribirAlumno ].
( op = 2 ) ifTrue:[ self altaDocente].
( op = 3 ) ifTrue: [ self listadoAlumnos ].
( op = 4 ) ifTrue: [ self listadoDocentes ]] .! !
!Universidad categoriesForMethods!
altaDocente!public! !
inicializa!public! !
inscribirAlumno!public! !
listadoAlumnos!public! !
listadoDocentes!public! !
menu!public! !
!

Alumno guid: (GUID fromString: '{dc75323c-84f7-419a-a6a9-24c23d8358e2}')!
Alumno comment: ''!
!Alumno categoriesForClass!Kernel-Objects! !
!Alumno methodsFor!

asignarNota: unEntero a: unNombreMateria
|mat|
mat := materias detect: [:unaMateria| unaMateria nombre = unNombreMateria] ifNone: [nil].
mat isNil ifTrue: [^ MessageBox notify: 'El alumno no cursa la materia solicitada'].
mat nota: unEntero.
!

cargaDatos
super cargaDatos.
legajo := Prompter prompt: 'Ingrese legajo'.
fechaInscripcion := Date today.
carrera := Prompter prompt: 'Ingrese la carrera'.!

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

inicializa
materias := OrderedCollection new.!

inscribirEn: unaMateria
materias add: unaMateria!

listarMateriasAprobadas
|aprobadas ordenadas|
aprobadas := materias select: [:unaMateria | unaMateria nota >= 6].
ordenadas := aprobadas asSortedCollection: [:unaMateria :otraMateria| unaMateria nota > otraMateria nota ].
Transcript show: 'Listado de materias aprobadas: '; cr.
ordenadas do: [:unaMateria |
Transcript show: unaMateria nombre, ' - nota: ',
unaMateria nota printString; cr ].!

listarMateriasReprobadas
|reprobadas ordenadas|
reprobadas := materias select: [:unaMateria | unaMateria nota < 6].
ordenadas := reprobadas asSortedCollection: [:unaMateria :otraMateria| unaMateria nota > otraMateria nota ].
Transcript show: 'Listado de materias reprobadas: '; cr. 
ordenadas do: [:unaMateria |
	Transcript show: unaMateria nombre, ' - nota: ', unaMateria nota printString; cr ].!

listarTodasLasMaterias
|ordenadas|
ordenadas := materias asSortedCollection: [:unaMateria :otraMateria| unaMateria nombre < otraMateria nombre ].
Transcript show: 'Listado de todas las materias: '; cr.
 ordenadas do: [:unaMateria | Transcript show: unaMateria nombre, ' - nota: ', unaMateria nota printString; cr ].!

materiasCursando
^ materias size!

promedio
|acum|
acum := 0.
materias do: [:unaMateria | acum := acum + unaMateria nota].
^ acum / materias size asFloat!

saludar
^ Transcript show: 'Hola, mi nombre es ', nombre, 'y estudio la carrera ', carrera.
! !
!Alumno categoriesForMethods!
asignarNota:a:!public! !
cargaDatos!public! !
carrera!public! !
carrera:!public! !
diasEstudiando!public! !
fechaInscripcion!public! !
fechaInscripcion:!public! !
inicializa!public! !
inscribirEn:!public! !
listarMateriasAprobadas!public! !
listarMateriasReprobadas!public! !
listarTodasLasMaterias!public! !
materiasCursando!public! !
promedio!public! !
saludar!public! !
!

Docente guid: (GUID fromString: '{dd2f9715-47d6-434e-92d2-004e2be83c93}')!
Docente comment: ''!
!Docente categoriesForClass!Kernel-Objects! !
!Docente methodsFor!

antiguedad
^ Date today yearsSince: fechaAlta!

cargaDatos
super cargaDatos.
Docente incrementaUltimoCodigo.
codigo := Docente ultimoCodigo.
fechaAlta := Date fromString: (Prompter prompt: 'Ingrese fecha de alta (dd/mm/yyyy)').
cargo := Prompter prompt: 'Ingrese el cargo'.!

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
cargaDatos!public! !
cargo!public! !
cargo:!public! !
fechaAlta!public! !
fechaAlta:!public! !
saludar!public! !
!

!Docente class methodsFor!

incrementaUltimoCodigo
UltimoCodigo := UltimoCodigo + 1!

inicializaUltimoCodigo
UltimoCodigo := 0!

ultimoCodigo
^ UltimoCodigo! !
!Docente class categoriesForMethods!
incrementaUltimoCodigo!public! !
inicializaUltimoCodigo!public! !
ultimoCodigo!public! !
!

"Binary Globals"!

