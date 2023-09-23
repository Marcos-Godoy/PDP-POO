| package |
package := Package name: 'practica'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alumno;
	add: #Articulo;
	add: #Circunferencia;
	add: #Comision;
	add: #Importado;
	add: #Nacional;
	add: #Negocio;
	add: #Triangulo;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\..\..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Alumno
	instanceVariableNames: 'legajo nombre nota'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Articulo
	instanceVariableNames: 'codigo descripcion pBasico'
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
Object subclass: #Negocio
	instanceVariableNames: 'articulos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Triangulo
	instanceVariableNames: 'ladoA ladoB ladoC'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Articulo subclass: #Importado
	instanceVariableNames: ''
	classVariableNames: 'ValorDolar'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Articulo subclass: #Nacional
	instanceVariableNames: ''
	classVariableNames: 'Dto'
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

Articulo guid: (GUID fromString: '{6b08d45b-8f23-47d4-bea8-19eae696ac54}')!
Articulo comment: ''!
!Articulo categoriesForClass!Kernel-Objects! !
!Articulo methodsFor!

cargaDatos
"establece los datos de un articulo"

codigo := (Prompter prompt: 'Ingrese codigo de un articulo') asNumber  asInteger .
descripcion := Prompter prompt: 'Ingrese descripcion de un articulo'.
pBasico := (Prompter prompt: 'Ingrese precio basico de un articulo') asNumber asFloat .
^self!

devCodigo
^codigo!

devDescripcion
^descripcion! !
!Articulo categoriesForMethods!
cargaDatos!public! !
devCodigo!public! !
devDescripcion!public! !
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

Negocio guid: (GUID fromString: '{104a0144-d8a6-4a02-9f81-08f799fb8ba6}')!
Negocio comment: ''!
!Negocio categoriesForClass!Kernel-Objects! !
!Negocio methodsFor!

cargaArticulos

|op arti resp|
resp := true.
[ resp ] whileTrue: [op := (Prompter prompt: 'Ingrese opción: 1- Nacional 2-Importado') asNumber asInteger.
(op = 1) ifTrue: [arti := Nacional new. ].
(op = 2) ifTrue: [arti := Importado new. ].
arti cargaDatos.
articulos add: arti.
resp := MessageBox confirm: 'Cargar otro?'.
]!

listadoImportados

| temp |
temp := articulos reject: [ :x | x esNacional ].
"se deberia ordenar la coleccion temp aca para el enunciado c)"
( temp isEmpty ) ifTrue:[ MessageBox notify: 'NO HAY ARTICULOS IMPORTADOS']
ifFalse:[Transcript clear; cr;show: 'LISTADO DE ARTICULOS IMPORTADOS' ; cr; show:'CODIGO DESCRIPCION PRECIO UNITARIO'; cr.
temp do: [ :x | Transcript show: x devCodigo printString; tab; tab;
show: x devDescripcion; tab; tab;
show: x calculaPUnitario printString ] ].!

listadoNacionales

| temp |
temp := articulos select: [ :x | x esNacional ].
( temp isEmpty ) ifTrue:[ MessageBox notify: 'NO HAY ARTICULOS NACIONALES']
ifFalse:[Transcript clear; cr;show: 'LISTADO DE ARTICULOS NACIONALES' ; cr; show:'CODIGO DESCRIPCION PRECIO UNITARIO'; cr.
temp do: [ :x | Transcript show: x devCodigo printString; tab; tab;
show: x devDescripcion; tab; tab;
show: x calculaPUnitario printString ] ].!

listados

| op |
op:= (Prompter prompt: 'Ingrese opción:' ) asNumber asInteger.
( op = 1 ) ifTrue:[ self listadoNacionales ].
( op = 2 ) ifTrue:[ self listadoImportados ].!

menu

| op |
op := 5.
[ op = 0 ] whileFalse:[ MessageBox notify:'MENU:
1- Inicializar valores generales
2- Carga de artículos
3- Listado de artículos
0- Salir'.
op:= (Prompter prompt: 'Ingrese opción:' ) asNumber asInteger.
( op = 1 ) ifTrue:[ self valoresGenerales ].
( op = 2 ) ifTrue:[ self cargaArticulos ].
( op = 3 ) ifTrue: [ self listados ] ] .!

valoresGenerales

articulos := OrderedCollection new.
Nacional ingresaDto.
Importado ingresaValorDolar.! !
!Negocio categoriesForMethods!
cargaArticulos!public! !
listadoImportados!public! !
listadoNacionales!public! !
listados!public! !
menu!public! !
valoresGenerales!public! !
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

Importado guid: (GUID fromString: '{017db5f4-b3db-48d6-b57e-8c04e92ed18b}')!
Importado comment: ''!
!Importado categoriesForClass!Kernel-Objects! !
!Importado methodsFor!

calculaPUnitario
"calcula el precio unitario para un producto importado"

^ pBasico * ValorDolar!

esNacional
^false! !
!Importado categoriesForMethods!
calculaPUnitario!public! !
esNacional!public! !
!

!Importado class methodsFor!

ingresaValorDolar
"establece el valor del dolar para todas las instancias"

ValorDolar := (Prompter  prompt: 'Ingrese cotizacion del dolar' ) asNumber asFloat.! !
!Importado class categoriesForMethods!
ingresaValorDolar!public! !
!

Nacional guid: (GUID fromString: '{bb7cae63-4c41-49cf-bb8e-c027111fc016}')!
Nacional comment: ''!
!Nacional categoriesForClass!Kernel-Objects! !
!Nacional methodsFor!

calculaPUnitario
"calcula el precio unitario por producto nacional"

^ pBasico * (1 - (Dto / 100))
!

esNacional
^true! !
!Nacional categoriesForMethods!
calculaPUnitario!public! !
esNacional!public! !
!

!Nacional class methodsFor!

ingresaDto

Dto := (Prompter  prompt: 'Ingrese descuento nacional' ) asNumber asFloat .! !
!Nacional class categoriesForMethods!
ingresaDto!public! !
!

"Binary Globals"!

