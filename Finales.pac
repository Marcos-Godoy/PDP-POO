| package |
package := Package name: 'Finales'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alojamiento;
	add: #ArbolFrutal;
	add: #ArbolMayorPorte;
	add: #Box;
	add: #Caballo;
	add: #Campo;
	add: #ClienteVivero;
	add: #Duenio;
	add: #Establecimiento;
	add: #PedidoVivero;
	add: #Planta;
	add: #Plantin;
	add: #Vivero;
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

Object subclass: #Alojamiento
	instanceVariableNames: 'codigo fecha caballo'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Caballo
	instanceVariableNames: 'nombre raza edad duenio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ClienteVivero
	instanceVariableNames: 'dni nombre apellido direccion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Duenio
	instanceVariableNames: 'nya dni domicilio contacto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Establecimiento
	instanceVariableNames: 'alojamientos duenios caballos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #PedidoVivero
	instanceVariableNames: 'numero fecha cliente plantas embalaje'
	classVariableNames: 'UltimoNumero'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Planta
	instanceVariableNames: 'nombre descripcion precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Vivero
	instanceVariableNames: 'platas pedidos clientes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Alojamiento subclass: #Box
	instanceVariableNames: 'nombre costo ubicacion tamanio estado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Alojamiento subclass: #Campo
	instanceVariableNames: ''
	classVariableNames: 'Costo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Planta subclass: #ArbolFrutal
	instanceVariableNames: ''
	classVariableNames: 'Descuento'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Planta subclass: #ArbolMayorPorte
	instanceVariableNames: 'descuento'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Planta subclass: #Plantin
	instanceVariableNames: 'tipo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Alojamiento guid: (GUID fromString: '{5cb3a155-ad45-4e2e-919e-448f0465966d}')!
Alojamiento comment: ''!
!Alojamiento categoriesForClass!Kernel-Objects! !
!Alojamiento methodsFor!

asignarCaballo: unCaballo
caballo := unCaballo.!

cargaDatos
codigo := Alojamiento IncDevUltimoCodigo.
fecha := Date today.
! !
!Alojamiento categoriesForMethods!
asignarCaballo:!public! !
cargaDatos!public! !
!

!Alojamiento class methodsFor!

IncDevUltimoCodigo
UltimoCodigo := UltimoCodigo + 1.
^UltimoCodigo.!

iniciarCodigo
UltimoCodigo := 0.! !
!Alojamiento class categoriesForMethods!
IncDevUltimoCodigo!public! !
iniciarCodigo!public! !
!

Caballo guid: (GUID fromString: '{c162c460-7d50-4df1-b87d-1b43fac862a9}')!
Caballo comment: ''!
!Caballo categoriesForClass!Kernel-Objects! !
!Caballo methodsFor!

asignarDuenio: unD
duenio := unD.!

cargaDatos
nombre := Prompter prompt: 'Ingrese nombre:'.
raza := Prompter prompt: 'Ingrese raza'.
edad := (Prompter prompt: 'Ingrese edad' ) asNumber  asInteger .!

edad
^edad!

mostrarCaballo

Transcript show: nombre; tab; edad; tab; duenio;cr! !
!Caballo categoriesForMethods!
asignarDuenio:!public! !
cargaDatos!public! !
edad!public! !
mostrarCaballo!public! !
!

ClienteVivero guid: (GUID fromString: '{25802d97-24b9-4d79-8bac-65ccccada215}')!
ClienteVivero comment: ''!
!ClienteVivero categoriesForClass!Kernel-Objects! !
!ClienteVivero methodsFor!

cargaDatos

dni := (Prompter prompt: 'Ingrese dni' ).
nombre := Prompter prompt: 'Ingrese nombre'.
apellido := Prompter prompt: 'Ingrese nombre'.
direccion := Prompter prompt: 'Ingrese nombre'.! !
!ClienteVivero categoriesForMethods!
cargaDatos!public! !
!

Duenio guid: (GUID fromString: '{bd82c73d-154a-46ab-ab62-ec8e829a6fc4}')!
Duenio comment: ''!
!Duenio categoriesForClass!Kernel-Objects! !
!Duenio methodsFor!

cargaDatos
nya := (Prompter prompt: 'Ingrese nombre y apellido:' ).
dni := (Prompter  prompt: 'Ingrese dni' ) asNumber asInteger.
domicilio := (Prompter prompt: 'Ingrese domicilio' ).
contacto := Prompter prompt: 'Ingrese telefono de contacto'.! !
!Duenio categoriesForMethods!
cargaDatos!public! !
!

Establecimiento guid: (GUID fromString: '{eb806050-334b-495e-b86d-031513785c0f}')!
Establecimiento comment: ''!
!Establecimiento categoriesForClass!Kernel-Objects! !
!Establecimiento methodsFor!

crearAlojamiento
|nomDuenio duenio caballito op tipo boxito campito|
op = true.
nomDuenio := Prompter prompt: 'Ingrese nombre del dueño'.
duenio := duenios detect: [:each | each nya = nomDuenio ] ifNone: [nil].
(duenio isNil ) ifTrue: [
duenio := Duenio new.
duenio cargaDatos.
].
caballito := Caballo new.
caballito cargaDatos.
caballito asignarDuenio: duenio.
(op = true) whileTrue: [
tipo := (Prompter prompt: 'Ingrese tipo de alojamiento (c o b)' ) asUppercase.
(tipo = 'B' or: [tipo = 'C']) ifTrue: [op := false ].
].
(tipo = 'B') ifTrue: [
boxito := Box new.
boxito cargaDatos.
]
ifFalse: [
campito := Campo new.

].

!

inicializa
duenios := OrderedCollection new.
alojamientos := OrderedCollection new.
caballos := OrderedCollection new.
Alojamiento iniciarCodigo.
Campo ICosto.!

listadoCaballos
|caballitos|
caballitos := caballos select: [:each | each edad > 10] ifNone: [nil].
(caballitos isEmpty) ifTrue: [MessageBox notify: 'No hay caballos mayores a 10 anios'. ]
ifFalse: [
Transcript clear; show: 'Nombre'; tab; show: 'Edad'; tab; show: 'Nombre dueño'; cr.
caballitos do: [:each | each mostrarCaballo].
].! !
!Establecimiento categoriesForMethods!
crearAlojamiento!public! !
inicializa!public! !
listadoCaballos!public! !
!

PedidoVivero guid: (GUID fromString: '{f1471a71-6f6e-4fd7-ab0b-1968293a10b0}')!
PedidoVivero comment: ''!
!PedidoVivero categoriesForClass!Kernel-Objects! !
!PedidoVivero methodsFor!

asignarCliente: cli
cliente := cli.!

asignarPlanta: pl
plantas add: pl.!

cargaDatos

fecha := Date today.
embalaje := (Prompter prompt: 'Ingrese costo del embalaje' ).
numero := PedidoVivero IncDevUltimoNumero.! !
!PedidoVivero categoriesForMethods!
asignarCliente:!public! !
asignarPlanta:!public! !
cargaDatos!public! !
!

!PedidoVivero class methodsFor!

IncDevUltimoNumero

UltimoNumero := UltimoNumero + 1.
^UltimoNumero! !
!PedidoVivero class categoriesForMethods!
IncDevUltimoNumero!public! !
!

Planta guid: (GUID fromString: '{328e652e-775f-4dda-9665-fd48d477d5fd}')!
Planta comment: ''!
!Planta categoriesForClass!Kernel-Objects! !
!Planta methodsFor!

cargaDatos

nombre := Prompter prompt: 'Ingrese nombre'.
descripcion := Prompter prompt: 'Ingrese descripcion'.
precio := (Prompter prompt: 'Ingrese precio basico') asNumber asInteger .
! !
!Planta categoriesForMethods!
cargaDatos!public! !
!

Vivero guid: (GUID fromString: '{6b87997d-276e-40d0-806a-069c6ef0c779}')!
Vivero comment: ''!
!Vivero categoriesForClass!Kernel-Objects! !
!Vivero methodsFor!

altaPedido
|pedi cli op pl|
op := true.
cli := self buscarOCrearCliente.

pedi := PedidoVivero new.
pedi cargaDatos.
pedi asignarCliente: cli.
(op) whileTrue: [
self mostrarPlantas.
pl := Prompter prompt: 'ingrese nombre de planta'.
pedi asignarPlanta: pl.
op := MessageBox confirm: 'Desea seguir ingresando plantas?'.
].
pedi add: pedidos.
pedi montoTotal.!

buscarOCrearCliente
|cli nom|
nom := (Prompter prompt: 'Ingrese nombre del cliente').
cli := clientes detect: [:unCliente | unCliente nombre = nom ] ifNone: [nil].
(cli isNil) ifTrue: [ cli := ClienteVivero new.
cli cargaDatos.
clientes add: cli. ].
^ cli!

mostrarPlantas
||
(platas isNil ) ifTrue: [MessageBox notify: 'No hay plantas...' ].
platas do: [:each | each ]! !
!Vivero categoriesForMethods!
altaPedido!public! !
buscarOCrearCliente!public! !
mostrarPlantas!public! !
!

Box guid: (GUID fromString: '{9929d360-af5d-4990-b6c7-211203732023}')!
Box comment: ''!
!Box categoriesForClass!Kernel-Objects! !
!Box methodsFor!

cargaDatos

nombre := Prompter prompt: 'Ingrese nombre:'.
ubicacion := (Prompter prompt: 'Ingrese ubicacion:' ) asNumber asInteger.
costo := (Prompter prompt: 'Ingrese costo:' ) asNumber asInteger.
estado := Prompter prompt: 'Ingrese estado:'.
tamanio := Prompter prompt: 'Ingrese tamaño:'.!

costo
^costo! !
!Box categoriesForMethods!
cargaDatos!public! !
costo!public! !
!

Campo guid: (GUID fromString: '{affeb171-580b-49c8-a212-60c183b2d59d}')!
Campo comment: ''!
!Campo categoriesForClass!Kernel-Objects! !
!Campo methodsFor!

costo
^Costo! !
!Campo categoriesForMethods!
costo!public! !
!

!Campo class methodsFor!

ICosto
Costo := Prompter prompt: 'Ingrese costo fijo para campo:'.! !
!Campo class categoriesForMethods!
ICosto!public! !
!

ArbolFrutal guid: (GUID fromString: '{e0c1322d-9f69-4e76-a8b8-5e15d07b341e}')!
ArbolFrutal comment: ''!
!ArbolFrutal categoriesForClass!Kernel-Objects! !
!ArbolFrutal methodsFor!

precioFinal
^precio - Descuento! !
!ArbolFrutal categoriesForMethods!
precioFinal!public! !
!

!ArbolFrutal class methodsFor!

Descuento: unDesc
Descuento := unDesc.! !
!ArbolFrutal class categoriesForMethods!
Descuento:!public! !
!

ArbolMayorPorte guid: (GUID fromString: '{94ba0f2f-abad-4116-b311-af985a12cee9}')!
ArbolMayorPorte comment: ''!
!ArbolMayorPorte categoriesForClass!Kernel-Objects! !
!ArbolMayorPorte methodsFor!

cargaDatos

super cargaDatos.
descuento := (Prompter prompt: 'Ingrese descuento:' ) asNumber asInteger.!

precioFinal
^precio - descuento.! !
!ArbolMayorPorte categoriesForMethods!
cargaDatos!public! !
precioFinal!public! !
!

Plantin guid: (GUID fromString: '{157f7596-06d5-4c93-a74d-8e931691666e}')!
Plantin comment: ''!
!Plantin categoriesForClass!Kernel-Objects! !
!Plantin methodsFor!

precioFinal
^precio! !
!Plantin categoriesForMethods!
precioFinal!public! !
!

"Binary Globals"!

