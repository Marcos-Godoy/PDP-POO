persona := Persona new.
persona printString.
persona nombre: 'Ana'.
persona nombre.
persona edad: 25.
persona genero: 'Femenino'.
persona saludar.

p1 := Persona new.
p1 nombre: 'Juan'.
p1 edad: 18.
p1 genero: 'Masculino'.
p1 saludar .

persona fechaNacimiento: (Date fromString: '18/03/1998').
p1 fechaNacimiento: (Date fromString: '25/08/2005').

persona edad.
p1 edad.18
persona esMayorDeEdad.true
p1 esMayorDeEdad.true

persona edadEn: 2047.49
p1 edadEn: 2047.42

persona mayorQue: p1.
p1 mayorQue: persona .

persona soltero.
persona estadoCivil.
p1 casado.
p1 estadoCivil.