# pdepreludat-ejemplo
Repo con el pdepreludat ya setupeado como para usar de ejemplo para empezar proyectos en haskell nuevos.

# ¿Como uso esto?

## Prerrequisito

[Tener instalados stack y git.](https://github.com/pdep-utn/enunciados-miercoles-noche/blob/master/pages/haskell/entorno.md)

## Ahora sí

1. Clonar el repo: `https://github.com/JuanFdS/pdepreludat-ejemplo.git` o `git@github.com:JuanFdS/pdepreludat-ejemplo.git`

2. Moverse a la carpeta del repo: `cd pdepreludat-ejemplo`.

2. Compilar lo que necesita el proyecto: `stack build`

3. Correr los tests para verificar que todo está bien: `stack test`.
  
   Si ves este mensaje significa que todo ya está instalado y andando:
   ```
   Test de ejemplo
   El pdepreludat se instaló correctamente
   ```

A partir de acá, podés:

- Abrir el interprete para probar cosas: `stack ghci`
  ![Algunos ejemplos en ghci: 2 + 2 retorna 4, "hello" ++ "!" retorna "hello!"](https://i.imgur.com/43CPlAm.png)

- Escribir nuevo código en `src/Library.hs` y probarlo con tests que se escriben en `test/Spec.hs`
  ![Agregado un test en test/Spec.hs que dice que el cuadruple de un numero es el doble de su doble, y testeado que 4 `shouldBe` 1. Corrido el test con `stack test` (que falla), luego definida una función en src/Library.hs que hace pasar ese test, la definición es `cuadruple numero = doble (doble numero)`. Tras haber hecho eso, se corren de nuevo los tests, que esta vez pasan.](https://i.imgur.com/8nbJ7RO.gif)
  
# ¿Qué es el PdePreludat?

Es una librería que cambia un poco lo que sería la librería estandar que usa Haskell automáticamente (la cual se llama Prelude). Idealmente simplifica algunas cosas que no son necesarias a la materia y mejora algunos mensajes de error.
Para leer más sobre esto: https://github.com/10Pines/pdepreludat
