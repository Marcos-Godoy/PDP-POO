| package |
package := Package name: 'paquete distribuidora'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alimento;
	add: #Bebida;
	add: #Distribuidoras;
	add: #Ingrediente;
	add: #Producto;
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

Object subclass: #Distribuidoras
	instanceVariableNames: 'productos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Ingrediente
	instanceVariableNames: 'nombre cantidad estaCertificado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Producto
	instanceVariableNames: 'codigo nombre descripcion precio stock cantidadMinima ingredientes esDestacado'
	classVariableNames: 'Descuento UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Producto subclass: #Alimento
	instanceVariableNames: 'peso fechaVencimiento diasAvisoCaducidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Producto subclass: #Bebida
	instanceVariableNames: 'cantidad costoEnvaseRetornable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Distribuidoras guid: (GUID fromString: '{6178d986-a443-429a-a6c7-00c0fc1762e4}')!
Distribuidoras comment: ''!
!Distribuidoras categoriesForClass!Kernel-Objects! !
!Distribuidoras methodsFor!

altaProducto

|tipo prod|
tipo := '-'.
[ tipo asUppercase = 'A' or: [ tipo asUppercase = 'B' ]] 
whileFalse: [
tipo := Prompter prompt: 'Ingrese tipo (Alimento o Bebida)'
].
(tipo asUppercase = 'A') ifTrue: [prod := Alimento new].
(tipo asUppercase = 'B' ) ifTrue: [prod := Bebida new ].
prod cargaDatos .
productos add: prod.
!

decrementarStock
|nombre prod cant|

nombre := Prompter prompt: 'Ingrese el nombre del producto a buscar: '.
prod := productos detect: [:unProd | unProd nombre = nombre ].

prod isNil ifTrue: [^ MessageBox  notify: 'No se encuentra el producto seleccionado...'].
cant := (Prompter  prompt: 'Ingrese cantidad de stock: ' ) asNumber asInteger.
prod disminuirStock: cant.!

incrementarStock
|nombre prod cant|

nombre := Prompter prompt: 'Ingrese el nombre del producto a buscar: '.
prod := productos detect: [:unProd | unProd nombre = nombre ].

prod isNil ifTrue: [^ MessageBox  notify: 'No se encuentra el producto seleccionado...'].
cant := (Prompter  prompt: 'Ingrese cantidad de stock: ' ) asNumber asInteger.
prod incrementarStock: cant.!

inicializa
productos := OrderedCollection new.
!

listadoAlimentosProximosVencer

|alimentos|
alimentos := productos select: [:unProd | unProd esAlimento and: [unProd proximoAVencer  and: [unProd estaVencido not ] ] ].
Transcript show: 'Alimentos próximos a vencer: '; cr.
alimentos do: [:unAlim| unAlim mostrar.
Transcript cr.
].
!

listadoBebidasRetornables
|bebidas|

bebidas := productos select: [:unProd | unProd esBebida and: [unProd costoEnvaseRetornable > 0] ].
bebidas := bebidas asSortedCollection: [:unProd :otroProd | unProd precio > otroProd precio ].

Transcript show: 'Bebidas ordenadas por precio'; cr.
bebidas do: [:unProd |
unProd mostrar. 
Transcript cr.
].!

listadoProductos
|ordenados|

ordenados := productos asSortedCollection: [:unProd :otroProd | unProd stock < otroProd stock ].

Transcript show: 'Productos ordenados por stock'; cr.
ordenados do: [:unProd |
unProd mostrar. 
Transcript cr.
].!

listadoProductosPocoStock
|prods|

prods := productos select: [:unProd | unProd verificarMinima not ].
Transcript show: 'Productos con poco stock: '; cr.
prods do: [:unProd| unProd mostrar.
].
!

menu
|op|

[op = 0] whileFalse: [
MessageBox notify: 'MENU:
1- Alta producto.
2- Incrementar Stock producto.
3- Decrementar Stock producto.
4- Listado productos.
5- Listado bebidas retornables
6- Listado alimentos proximos a vencer.
7- Listado productos con poco stock.
0- Salir'.
op:= (Prompter prompt:'Ingrese opción:') asNumber asInteger.
( op = 1 ) ifTrue:[ self altaProducto ].
( op = 2 ) ifTrue:[ self incrementarStock].
( op = 3 ) ifTrue: [ self decrementarStock ].
( op = 4 ) ifTrue: [ self listadoProductos ].
( op = 5 ) ifTrue: [ self listadoBebidasRetornables ].
( op = 6 ) ifTrue: [ self listadoAlimentosProximosVencer
].
( op = 7 ) ifTrue: [ self listadoProductosPocoStock ].
]
! !
!Distribuidoras categoriesForMethods!
altaProducto!public! !
decrementarStock!public! !
incrementarStock!public! !
inicializa!public! !
listadoAlimentosProximosVencer!public! !
listadoBebidasRetornables!public! !
listadoProductos!public! !
listadoProductosPocoStock!public! !
menu!public! !
!

Ingrediente guid: (GUID fromString: '{b7394461-7f28-4c1d-b69a-5079eae5322d}')!
Ingrediente comment: ''!
!Ingrediente categoriesForClass!Kernel-Objects! !
!Ingrediente methodsFor!

cantidad
^ cantidad!

cantidad: unNumero
cantidad := unNumero!

cargaDatos
nombre := Prompter prompt: 'Ingrese el nombre del ingrediente'.
cantidad := (Prompter prompt: 'Ingrese la cantidad del ingrediente') asNumber.
estaCertificado := MessageBox confirm: '¿El ingrediente esta certificado?'.!

estaCertificado
^ estaCertificado!

estaCertificado: unBooleano
estaCertificado := unBooleano!

nombre
^ nombre!

nombre: unString
nombre := unString! !
!Ingrediente categoriesForMethods!
cantidad!public! !
cantidad:!public! !
cargaDatos!public! !
estaCertificado!public! !
estaCertificado:!public! !
nombre!public! !
nombre:!public! !
!

Producto guid: (GUID fromString: '{2926b73a-e497-4d3e-920e-39c835ac12d7}')!
Producto comment: ''!
!Producto categoriesForClass!Kernel-Objects! !
!Producto methodsFor!

cantidadMinima: unaCantidad
cantidadMinima := unaCantidad.!

cargaDatos
Producto incrementaUltimoCodigo.
codigo := Producto ultimoCodigo.
nombre := Prompter prompt: 'Ingrese el nombre del producto'.
 descripcion := Prompter prompt: 'Ingrese la descripcion del producto'.
precio := (Prompter prompt: 'Ingrese el precio del producto') asNumber.
stock := (Prompter prompt: 'Ingrese el stock del producto') asNumber.
esDestacado := MessageBox confirm: 'Es destacado?' .
!

descripcion
^descripcion !

descripcion: unaDesc
descripcion := unaDesc .!

disminuirStock: cantidad
(stock >= cantidad)
ifTrue: [stock := stock - cantidad].!

disponibilidad: requeridos
^ (stock>requeridos)!

esAlimento
^false!

esBebida
^false!

esDestacado
^ esDestacado!

esDestacado: unBooleano
esDestacado := unBooleano!

incrementarStock: cantidad
stock := stock + cantidad.!

inicializa
ingredientes := OrderedCollection new.!

montoTotal
^ precio * stock!

nombre
^nombre!

nombre: unNombre
nombre := unNombre.!

precio
^precio!

precio: unPrecio
precio := unPrecio.!

productosAComprar
(self verificarMinima) ifFalse: [^ ((cantidadMinima - stock) negated)]
ifTrue: [^0].!

stock
^stock!

stock: unStock
stock := unStock.!

verificarMinima
^ (cantidadMinima < stock)! !
!Producto categoriesForMethods!
cantidadMinima:!public! !
cargaDatos!public! !
descripcion!public! !
descripcion:!public! !
disminuirStock:!public! !
disponibilidad:!public! !
esAlimento!public! !
esBebida!public! !
esDestacado!public! !
esDestacado:!public! !
incrementarStock:!public! !
inicializa!public! !
montoTotal!public! !
nombre!public! !
nombre:!public! !
precio!public! !
precio:!public! !
productosAComprar!public! !
stock!public! !
stock:!public! !
verificarMinima!public! !
!

!Producto class methodsFor!

descuento
^ Descuento!

incializaDescuento
Descuento := (Prompter prompt: 'Ingrese el monto del descuento' ) asNumber.!

incrementaUltimoCodigo
UltimoCodigo := UltimoCodigo + 1 .!

inicializaUltimoCodigo
UltimoCodigo := 0.!

ultimoCodigo
^ UltimoCodigo
! !
!Producto class categoriesForMethods!
descuento!public! !
incializaDescuento!public! !
incrementaUltimoCodigo!public! !
inicializaUltimoCodigo!public! !
ultimoCodigo!public! !
!

Alimento guid: (GUID fromString: '{af4dc7e1-c136-4641-9c1c-7831cc70b898}')!
Alimento comment: ''!
!Alimento categoriesForClass!Kernel-Objects! !
!Alimento methodsFor!

agregarIngredientes
|ing continua|
continua := true.
[ continua ] whileTrue: [
ing := Ingrediente new.
ing cargaDatos.
ingredientes add: ing.
continua := MessageBox confirm: 'desea continuar?'
].!

cantidadGramosIngredientes
|gramosTotales|
gramosTotales := 0.
ingredientes do: [:ing | gramosTotales := gramosTotales  + ing cantidad ].
^ gramosTotales!

cantidadIngredientes
^ ingredientes size!

cargaDatos
super cargaDatos.
peso := (Prompter prompt: 'Ingrese el peso del producto en gramos') asNumber.
fechaVencimiento := Date fromString: (Prompter prompt: 'Ingrese fecha de vencimiento en formato DD/MM/AAAA').
diasAvisoCaducidad := (Prompter prompt: 'Ingrese los dias para aviso de caducidad') asNumber.
self agregarIngredientes.

!

diasAvisoCaducidad
^ diasAvisoCaducidad
!

diasAvisoCaducidad: unNumero
diasAvisoCaducidad := unNumero!

esAlimento
^true!

estaCertificado
ingredientes detect: [:ing | ing estaCertificado ] ifNone: [^false].
^true!

estaVencido
^ (Date today > fechaVencimiento )!

fechaVencimiento
^ fechaVencimiento!

fechaVencimiento: unaFecha
fechaVencimiento := unaFecha!

mostrar
Transcript show: nombre, peso printString, precio printString, stock printString, self proximoAVencer.!

mostrarIngredientes
Transcript show: 'Ingredientes: '; cr.
ingredientes do: [:ing | 
Transcript show: ing nombre, ing cantidad printString; cr .

]!

mostrarIngredientesCertificados

|certificados|
Transcript show: 'Ingredientes Certificados en Calidad: '; cr.
certificados := ingredientes select: [:ing | ing estaCertificado ].
certificados do: [:unIng| Transcript show: unIng cantidad printString,' grs.', unIng nombre; cr ].
!

mostrarIngredientesNoCertificados

|noCertificados|
Transcript show: 'Ingredientes No Certificados en Calidad: '; cr.
noCertificados := ingredientes reject: [:ing | ing estaCertificado ].
noCertificados do: [:unIng| Transcript show: unIng cantidad printString,' grs.', unIng nombre; cr ].


!

peso
^ peso!

peso: unNumero
peso := unNumero!

proximoAVencer
^ Date today > (fechaVencimiento subtractDays: diasAvisoCaducidad)! !
!Alimento categoriesForMethods!
agregarIngredientes!public! !
cantidadGramosIngredientes!public! !
cantidadIngredientes!public! !
cargaDatos!public! !
diasAvisoCaducidad!public! !
diasAvisoCaducidad:!public! !
esAlimento!public! !
estaCertificado!public! !
estaVencido!public! !
fechaVencimiento!public! !
fechaVencimiento:!public! !
mostrar!public! !
mostrarIngredientes!public! !
mostrarIngredientesCertificados!public! !
mostrarIngredientesNoCertificados!public! !
peso!public! !
peso:!public! !
proximoAVencer!public! !
!

Bebida guid: (GUID fromString: '{b37d1bae-b26a-40d8-a5f0-5b18d3b4230c}')!
Bebida comment: ''!
!Bebida categoriesForClass!Kernel-Objects! !
!Bebida methodsFor!

cantidad
^ cantidad!

cantidad: unNumero
cantidad := unNumero!

cargaDatos
super cargaDatos.
cantidad := (Prompter prompt: 'Ingrese el peso del producto en mililitros') asNumber.
costoEnvaseRetornable := (Prompter prompt: 'Ingrese el costo del envase retornable') asNumber.!

costoEnvaseRetornable
^ costoEnvaseRetornable!

costoEnvaseRetornable: unNumero
costoEnvaseRetornable := unNumero!

esBebida
^true!

mostrar
Transcript show: nombre, cantidad printString, self precioTotal, stock printString.!

precioTotal
^ precio + costoEnvaseRetornable! !
!Bebida categoriesForMethods!
cantidad!public! !
cantidad:!public! !
cargaDatos!public! !
costoEnvaseRetornable!public! !
costoEnvaseRetornable:!public! !
esBebida!public! !
mostrar!public! !
precioTotal!public! !
!

"Binary Globals"!

