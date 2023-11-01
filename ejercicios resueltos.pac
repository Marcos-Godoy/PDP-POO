| package |
package := Package name: 'ejercicios resueltos'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Alquiler;
	add: #Alumnos;
	add: #Cliente;
	add: #Clientes;
	add: #Curso;
	add: #CursoOnline;
	add: #CursoPresencial;
	add: #Distribuidora;
	add: #Docentes;
	add: #Ferreteria;
	add: #Granel;
	add: #Herramienta;
	add: #HerramientaManual;
	add: #HerramientaPesada;
	add: #Instituto;
	add: #ItemPedido;
	add: #Mayorista;
	add: #Minorista;
	add: #Paquete;
	add: #Pedido;
	add: #Personas;
	add: #Productos;
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

Object subclass: #Alquiler
	instanceVariableNames: 'codigo fecha cantidadDias cliente herramientas'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Cliente
	instanceVariableNames: 'codigo razonSocial telefono direccion nombreApellido'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Clientes
	instanceVariableNames: 'dni nombre apellido'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Curso
	instanceVariableNames: 'id titulo descripcion especialidad diaCursado costo docente alumnos'
	classVariableNames: 'UltimoId'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Distribuidora
	instanceVariableNames: 'clientes productos pedidos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Ferreteria
	instanceVariableNames: 'herramientas clientes alquileres'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Herramienta
	instanceVariableNames: 'codigo descripcion costoPorDia disponible'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Instituto
	instanceVariableNames: 'docentes alumnos cursos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ItemPedido
	instanceVariableNames: 'producto cantidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Pedido
	instanceVariableNames: 'fecha cliente items'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Personas
	instanceVariableNames: 'dni nombre apellido telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Productos
	instanceVariableNames: 'codigo descripcion precio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Cliente subclass: #Mayorista
	instanceVariableNames: 'credito'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Cliente subclass: #Minorista
	instanceVariableNames: ''
	classVariableNames: 'Incremento'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Curso subclass: #CursoOnline
	instanceVariableNames: 'cantidadEntregas'
	classVariableNames: 'CostoEntrega'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Curso subclass: #CursoPresencial
	instanceVariableNames: 'costoMateriales'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Herramienta subclass: #HerramientaManual
	instanceVariableNames: 'cantidadPiezas costoPorPieza'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Herramienta subclass: #HerramientaPesada
	instanceVariableNames: ''
	classVariableNames: 'Seguro'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Personas subclass: #Alumnos
	instanceVariableNames: 'fechaInscripcion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Personas subclass: #Docentes
	instanceVariableNames: 'especialidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Productos subclass: #Granel
	instanceVariableNames: 'pesoMinimoCompra'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Productos subclass: #Paquete
	instanceVariableNames: 'cantidadUnidades'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Alquiler guid: (GUID fromString: '{480e1cf1-6b4a-48c4-b7ca-70c048938a2c}')!
Alquiler comment: ''!
!Alquiler categoriesForClass!Kernel-Objects! !
!Alquiler methodsFor!

agregarHerramienta: unaHerr

herramientas add: unaHerr.!

cargaDatos

codigo := UltimoCodigo.
Alquiler incrementarUltimoCodigo.
fecha := Date Today.
cantidadDias := (Prompter  prompt:  'Ingrese cantidad de dias:' ).
!

cliente: unCli

cliente := unCli.!

codigo
^codigo!

costoTotal
|tot|
tot := 0.
herramientas do: [:each | tot := tot + each costoTotal].
^tot * cantidadDias!

imprimirDetalle
|costoTotalHerramienta|
Transcript
show: 'Detalle del alquiler: ', codigo printString; cr;
show: 'Cliente: ', cliente nombreYApellido, ' - ', cliente dni; cr;
show: 'Fecha: ', fecha printString; cr;
show: 'Dias: ', cantidadDias printString; cr;
show: 'Herramientas: '; cr.
Transcript
show: 'Codigo'; tab;
show: 'Descripcion'; tab;
show: 'Costo total por dia'; tab;
show: 'Costo total'; cr.
herramientas do: [:unaHerramienta | costoTotalHerramienta := unaHerramienta costoTotal * cantidadDias.
Transcript
show: unaHerramienta codigo printString; tab;
show: unaHerramienta descripcion; tab;
show: unaHerramienta costoTotal printString; tab;
show: costoTotalHerramienta printString; cr.
].
Transcript show: 'Costo total del alquiler: ', self costoTotal.
!

inicializa

herramientas := OrderedCollection  new .! !
!Alquiler categoriesForMethods!
agregarHerramienta:!public! !
cargaDatos!public! !
cliente:!public! !
codigo!public! !
costoTotal!public! !
imprimirDetalle!public! !
inicializa!public! !
!

!Alquiler class methodsFor!

IncrementaUltimoCodigo

UltimoCodigo := UltimoCodigo + 1.! !
!Alquiler class categoriesForMethods!
IncrementaUltimoCodigo!public! !
!

Cliente guid: (GUID fromString: '{20c82c4c-c74b-40d9-8bbd-09945ba7696f}')!
Cliente comment: ''!
!Cliente categoriesForClass!Kernel-Objects! !
!Cliente methodsFor!

cargaDatos
codigo := UltimoCodigo .
Cliente incrementaUltimoCodigo.
razonSocial := Prompter prompt: 'Ingrese la razon social'.
telefono := Prompter prompt: 'Ingrese el telefono'.
direccion := Prompter prompt: 'Ingrese la direccion'.
nombreApellido := Prompter prompt: 'Ingrese nombre y apellido'.!

codigo
^codigo!

razonSocial
^razonSocial! !
!Cliente categoriesForMethods!
cargaDatos!public! !
codigo!public! !
razonSocial!public! !
!

!Cliente class methodsFor!

IncrementaUltimoCodigo

UltimoCodigo := UltimoCodigo + 1.! !
!Cliente class categoriesForMethods!
IncrementaUltimoCodigo!public! !
!

Clientes guid: (GUID fromString: '{647b4859-9660-4429-a532-cf67b3b31c81}')!
Clientes comment: ''!
!Clientes categoriesForClass!Kernel-Objects! !
Curso guid: (GUID fromString: '{f9222644-47a3-4b09-acad-e20e1d68df3d}')!
Curso comment: ''!
!Curso categoriesForClass!Kernel-Objects! !
!Curso methodsFor!

cargaDatos
id := UltimoId.
Curso incrementaUltimoId.
especialidad := Prompter prompt: 'Ingrese especialidad:'.
diaCursado := Prompter prompt: 'Ingrese dia de cursado:'.
costo := Prompter prompt: 'Ingrese costo inicial:'.
!

docente: unDocente

(unDocente especialidad = especialidad) ifTrue: [docente := unDocente]
ifFalse: [MessageBox notify: 'Especialidad no coincide' ].!

inicializa
UltimoId := 0.
!

inscribirAlumno: unAlum

alumnos add: unAlum.! !
!Curso categoriesForMethods!
cargaDatos!public! !
docente:!public! !
inicializa!public! !
inscribirAlumno:!public! !
!

!Curso class methodsFor!

incrementaUltimoId
UltimoId := UltimoId + 1.! !
!Curso class categoriesForMethods!
incrementaUltimoId!public! !
!

Distribuidora guid: (GUID fromString: '{6e639c18-793f-4c0a-b4eb-091e767a745c}')!
Distribuidora comment: ''!
!Distribuidora categoriesForClass!Kernel-Objects! !
!Distribuidora methodsFor!

altaPedido
|cli ped seguir item prod|
seguir := true.
cli := self buscarOCrearCliente.
ped := Pedido new.
ped fecha: Date today.
ped asignarCliente: cli.
[seguir] whileTrue: [
item := ItemPedido new.
prod := self buscarProducto.
item esCantidadCorrecta
	ifTrue: [ped agregarItem: item].
seguir := MessageBox confirm: 'Desea seguir?'.
].
ped mostrar.
pedidos add: ped.

!

buscarOCrearCliente
|cod cli|
cod := (Prompter prompt: 'Ingrese código del cliente') asNumber.
cli := clientes detect: [:unCliente | unCliente codigo = cod ] ifNone: [nil].
cli isNil ifTrue: [ cli := self nuevoCliente ].
^ cli
!

buscarProducto
|cod prod|
[prod isNil] whileTrue: [
cod := (Prompter prompt: 'Ingrese el código del producto') asNumber.
prod := productos detect: [:unProducto | unProducto codigo = cod] ifNone: [nil].
].
^prod!

nuevoCliente
|tipo cli|

tipo := Prompter prompt: 'Ingrese tipo de cliente: '.
(tipo = 1) ifTrue: [cli := Mayorista new].
(tipo = 2) ifTrue: [cli := Minorista new].
cli cargaDatos.
^cli ! !
!Distribuidora categoriesForMethods!
altaPedido!public! !
buscarOCrearCliente!public! !
buscarProducto!public! !
nuevoCliente!public! !
!

Ferreteria guid: (GUID fromString: '{055e0104-f7d2-4a9a-93e6-7d6a9c4892d5}')!
Ferreteria comment: ''!
!Ferreteria categoriesForClass!Kernel-Objects! !
!Ferreteria methodsFor!

altaAlquiler

|cont cli alqui cod herr|
cli := self buscarOCrearCliente.

alqui := Alquiler new.
alqui cargaDatos.
alqui cliente: cli.
alqui inicializa.
cont := true.
[cont] whileTrue: [
self mostrarHerramientasDisponibles.
cod := (Prompter prompt: 'Ingrese el codigo de la herramienta:' ) asNumber asInteger.
herr := herramientas detect: [:each | each codigo = cod] ifNone: [nil].
(herr notNil ) ifTrue: [
herr ocupar.
alqui agregarHerramienta: herr.
].
cont := MessageBox confirm: 'Desea continuar?'.
].

alquileres add: alqui.!

buscarOCrearCliente

|cli dni |
dni := (Prompter prompt: 'Ingrese dni a buscar:') asNumber asInteger.
cli := clientes detect: [:each | each dni = dni ].
(cli isNil) ifTrue: [
cli := Clientes new.
cli cargaDatos.
].
^ cli

!

imprimirAlquiler
| cod alq |
cod := (Prompter prompt: 'Ingrese el codigo de la herramienta') asNumber.
alq := alquileres detect: [:unAlquiler | unAlquiler codigo = cod] ifNone: [ nil ].
alq notNil
ifTrue: [ alq imprimirDetalle ]
ifFalse: [ MessageBox notify: 'No se encontro el alquiler' ].
!

mostrarHerramientasDisponibles

|herr|
herr := herramientas select: [:each | each estaDisponible].
herr := herr asSortedCollection: [:unaHerr :otraHerr | unaHerr codigo < otraHerr codigo].

Transcript
show: 'Listado de Herramientas disponibles:'; cr;
show: 'Codigo'; tab;
show: 'Descripcion'; cr.
herr do: [:unaHerramienta |
Transcript
show: unaHerramienta codigo; tab;
show: unaHerramienta descripcion; cr ].
! !
!Ferreteria categoriesForMethods!
altaAlquiler!public! !
buscarOCrearCliente!public! !
imprimirAlquiler!public! !
mostrarHerramientasDisponibles!public! !
!

Herramienta guid: (GUID fromString: '{4732fa4a-8f0f-44e8-85fa-076259e7dc3b}')!
Herramienta comment: ''!
!Herramienta categoriesForClass!Kernel-Objects! !
!Herramienta methodsFor!

codigo
^codigo!

descripcion
^descripcion!

disponibilizar
disponible := true.!

estaDisponible
^disponible!

ocupar
disponible := false .! !
!Herramienta categoriesForMethods!
codigo!public! !
descripcion!public! !
disponibilizar!public! !
estaDisponible!public! !
ocupar!public! !
!

Instituto guid: (GUID fromString: '{982eab25-ff85-4365-a635-8d67b65f2cfc}')!
Instituto comment: ''!
!Instituto categoriesForClass!Kernel-Objects! !
!Instituto methodsFor!

altaCurso
| tipo curso doc|
[ curso isNil ] whileTrue: [
tipo := Prompter prompt: 'Ingrese el tipo de curso a crear (1) Presencial, (2) Online'.
tipo = '1' ifTrue: [ curso := CursoPresencial new].
tipo = '2' ifTrue: [ curso := CursoOnline new].
].
curso cargaDatos.
doc := self buscarDocente.
curso docente: doc.
cursos add: curso.!

buscarCurso
| cur id|
[ cur isNil ] whileTrue: [
id := (Prompter prompt: 'Ingrese Identificador del curso') asNumber.
cur := cursos detect: [:unCurso | unCurso id = id ] ifNone: [ nil ]
].
^ cur
!

buscarDocente

|doc dni|
dni := (Prompter prompt: 'Ingrese dni del docente:' ) asNumber asInteger.
doc := docentes detect: [:each | dni = each dni].
^doc!

buscarOCrearAlumno

|alum dni|
dni := (Prompter prompt: 'Ingrese dni del alumno:' ) asNumber asInteger.
alum := alumnos detect: [:each | dni = each dni].
[alum isNil] ifTrue: [
self nuevoAlumno. ]
ifFalse: [^alum ].!

inscripcionCurso
| esp cursosEncontrados cur alu|
esp := Prompter prompt: 'Ingrese la especialidad del curso a inscribirse'.
cursosEncontrados = cursos select: [:unCurso | unCurso especialidad = esp ].
cur := self buscarCurso.
alu := self buscarOCrearAlumno.
cur inscribirAlumno:  alu.!

nuevoAlumno
|alum|

alum := Alumnos new.
alum cargaDatos.! !
!Instituto categoriesForMethods!
altaCurso!public! !
buscarCurso!public! !
buscarDocente!public! !
buscarOCrearAlumno!public! !
inscripcionCurso!public! !
nuevoAlumno!public! !
!

ItemPedido guid: (GUID fromString: '{30f6c3ca-6ead-40b1-80a8-4329d25d129f}')!
ItemPedido comment: ''!
!ItemPedido categoriesForClass!Kernel-Objects! !
!ItemPedido methodsFor!

asignarProducto: unProd
producto := unProd!

cantidad
^cantidad!

cargaDatos
cantidad := (Prompter prompt: 'Ingrese una cantidad de producto: ' ) asNumber asInteger.!

esCantidadCorrecta
^ producto verificarCantidad: cantidad!

producto
^producto! !
!ItemPedido categoriesForMethods!
asignarProducto:!public! !
cantidad!public! !
cargaDatos!public! !
esCantidadCorrecta!public! !
producto!public! !
!

Pedido guid: (GUID fromString: '{27da936f-a15c-49fb-b778-05d63169828b}')!
Pedido comment: ''!
!Pedido categoriesForClass!Kernel-Objects! !
!Pedido methodsFor!

asignarCliente: unCli
cliente := unCli!

cargaDatos
fecha := Date today.!

fecha: unaFecha
fecha := unaFecha!

mostrar
Transcript
show: 'Fecha: ', fecha printString; cr; cr;
show: 'Cliente: ', cliente razonSocial; cr;
show: 'Tipo cliente', cliente tipo; cr; cr.
Transcript
show: 'Codigo Prod.'; tab;
show: 'Descripcion Prod.'; tab;
show: 'Cantidad'; tab;
show: 'Precio Total'; tab; cr.
items do: [:unItem |
Transcript show: unItem producto codigo printString; tab;
Transcript show: unItem producto descripcion; tab;
Transcript show: unItem cantidad printString; tab;
Transcript show: (cliente precioTotalPara: unItem producto) printString;
tab; cr.
].
Transcript show: 'Total: ', self total printString.!

total
|total|
total := 0.
items do: [:unItem | total := total + (cliente precioTotalPara: unItem producto )].
^ total! !
!Pedido categoriesForMethods!
asignarCliente:!public! !
cargaDatos!public! !
fecha:!public! !
mostrar!public! !
total!public! !
!

Personas guid: (GUID fromString: '{75c352a8-b588-48bd-a70d-936d8f29a3ee}')!
Personas comment: ''!
!Personas categoriesForClass!Kernel-Objects! !
!Personas methodsFor!

cargaDatos

dni := (Prompter prompt: 'Ingrese dni:' ) asNumber asInteger.
nombre := Prompter prompt: 'Ingrese nombre:'.
apellido := Prompter prompt: 'Ingrese apellido:'.
telefono := Prompter prompt: 'Ingrese telefono:'.!

dni
^dni! !
!Personas categoriesForMethods!
cargaDatos!public! !
dni!public! !
!

Productos guid: (GUID fromString: '{24e7800b-43a2-42f8-9172-04aaabc9becf}')!
Productos comment: ''!
!Productos categoriesForClass!Kernel-Objects! !
!Productos methodsFor!

cargaDatos

codigo := (Prompter prompt: 'Ingrese un codigo: ') asNumber asInteger.
descripcion := Prompter prompt: 'Ingrese descripcion: '.
precio := (Prompter  prompt:  'Ingrese precio: ' ) asNumber asFloat.
!

codigo
^codigo!

descripcion
^descripcion! !
!Productos categoriesForMethods!
cargaDatos!public! !
codigo!public! !
descripcion!public! !
!

Mayorista guid: (GUID fromString: '{c953b8ce-6037-461a-8e26-2dc7e7e8e378}')!
Mayorista comment: ''!
!Mayorista categoriesForClass!Kernel-Objects! !
!Mayorista methodsFor!

cargaDatos
super cargaDatos.
credito := (Prompter prompt: 'Ingrese cretido al mayorista') asNumber.
!

precioTotalPara: unProd
^ unProd precio!

tipo
^ 'Mayorista'
! !
!Mayorista categoriesForMethods!
cargaDatos!public! !
precioTotalPara:!public! !
tipo!public! !
!

Minorista guid: (GUID fromString: '{046aa639-ac90-4db9-bdf8-26a152bce47b}')!
Minorista comment: ''!
!Minorista categoriesForClass!Kernel-Objects! !
!Minorista methodsFor!

precioTotalPara: unProducto
^ unProducto precio + Incremento
!

tipo
^ 'Minorista'
! !
!Minorista categoriesForMethods!
precioTotalPara:!public! !
tipo!public! !
!

CursoOnline guid: (GUID fromString: '{8ae1a227-d3dc-4859-ab20-7880f6c1bd9a}')!
CursoOnline comment: ''!
!CursoOnline categoriesForClass!Kernel-Objects! !
!CursoOnline methodsFor!

cargaDatos
super cargaDatos.
cantidadEntregas := (Prompter  prompt: 'Ingrese cantidad de entregas:' ) asNumber asInteger.
!

costoTotal
^ costo + (CostoEntrega * cantidadEntregas)
!

tipo
^ 'Online'
! !
!CursoOnline categoriesForMethods!
cargaDatos!public! !
costoTotal!public! !
tipo!public! !
!

CursoPresencial guid: (GUID fromString: '{bd1ddae5-cbf6-4e3c-b277-e9998cb9dcf3}')!
CursoPresencial comment: ''!
!CursoPresencial categoriesForClass!Kernel-Objects! !
!CursoPresencial methodsFor!

cargaDatos
super cargaDatos.
costoMateriales := (Prompter  prompt: 'Ingrese costo de materiales:' ) asNumber asFloat.
!

costoTotal
^ costoMateriales + costo!

tipo
^ 'Presencial'! !
!CursoPresencial categoriesForMethods!
cargaDatos!public! !
costoTotal!public! !
tipo!public! !
!

HerramientaManual guid: (GUID fromString: '{879ece93-9cac-40db-ad61-924c0098004c}')!
HerramientaManual comment: ''!
!HerramientaManual categoriesForClass!Kernel-Objects! !
!HerramientaManual methodsFor!

costoTotal

^ costoPorDia + (cantidadPiezas * costoPorPieza).! !
!HerramientaManual categoriesForMethods!
costoTotal!public! !
!

HerramientaPesada guid: (GUID fromString: '{71c48bff-4520-4e4f-afb0-ee9ee8d884a9}')!
HerramientaPesada comment: ''!
!HerramientaPesada categoriesForClass!Kernel-Objects! !
!HerramientaPesada methodsFor!

costoTotal
^ costoPorDia + Seguro.! !
!HerramientaPesada categoriesForMethods!
costoTotal!public! !
!

Alumnos guid: (GUID fromString: '{8b46ab65-9cf4-4ded-928a-4e2ce79abf4e}')!
Alumnos comment: ''!
!Alumnos categoriesForClass!Kernel-Objects! !
!Alumnos methodsFor!

cargaDatos
super cargaDatos.
fechaInscripcion := Date today.!

fechaInscripcion
^fechaInscripcion! !
!Alumnos categoriesForMethods!
cargaDatos!public! !
fechaInscripcion!public! !
!

Docentes guid: (GUID fromString: '{5f9c79e3-bb44-4355-ab0d-75af8bc8d295}')!
Docentes comment: ''!
!Docentes categoriesForClass!Kernel-Objects! !
!Docentes methodsFor!

cargaDatos
super cargaDatos.
especialidad := Prompter prompt: 'Ingrese especialidad:'.!

especialidad
^especialidad! !
!Docentes categoriesForMethods!
cargaDatos!public! !
especialidad!public! !
!

Granel guid: (GUID fromString: '{a7852755-e116-4e9f-826f-9d06ea9c6df8}')!
Granel comment: ''!
!Granel categoriesForClass!Kernel-Objects! !
!Granel methodsFor!

pesoMinimoCompra
^pesoMinimoCompra!

verificarCantidad: unNumero
^ pesoMinimoCompra < unNumero! !
!Granel categoriesForMethods!
pesoMinimoCompra!public! !
verificarCantidad:!public! !
!

Paquete guid: (GUID fromString: '{e0b98475-1db8-4f52-a5f3-3eb3844be1a7}')!
Paquete comment: ''!
!Paquete categoriesForClass!Kernel-Objects! !
!Paquete methodsFor!

cantidadUnidades
^cantidadUnidades! !
!Paquete categoriesForMethods!
cantidadUnidades!public! !
!

"Binary Globals"!

