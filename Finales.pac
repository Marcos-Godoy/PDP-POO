| package |
package := Package name: 'Finales'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #ArbolFrutal;
	add: #ArbolMayorPorte;
	add: #ClienteVivero;
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

Object subclass: #ClienteVivero
	instanceVariableNames: 'dni nombre apellido direccion'
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

