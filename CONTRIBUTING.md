# Contribuir a PlantillaBs4Dash

Gracias por ayudar a mejorar la plantilla. Procura que cada cambio sea pequeño,
reproducible y fácil de revisar.

## Preparar el entorno

1. Clona el repositorio y abre R desde su directorio raíz.
2. Si el proyecto todavía no contiene `renv/activate.R`, instala `renv` y ejecuta
   `renv::init()`. Si ya existe un archivo `renv.lock`, ejecuta `renv::restore()`.
3. Configura fuera del repositorio las credenciales necesarias para instalar los
   paquetes privados `racafe*`; nunca confirmes tokens ni archivos `.Renviron`.
4. Ejecuta la aplicación con `shiny::runApp("APP")`.

## Flujo de trabajo

1. Crea una rama desde la rama principal actual.
2. Mantén la lógica reutilizable en `APP/misc/` y la interfaz en
   `APP/misc/ui/`. Añade módulos nuevos en `APP/misc/modules/`.
3. Usa nombres descriptivos, `TRUE` y `FALSE` en lugar de `T` y `F`, y limita
   las líneas a 120 caracteres cuando sea razonable.
4. Si cambian las dependencias, ejecuta `renv::snapshot()` e incluye el
   `renv.lock` actualizado en el mismo cambio.
5. Antes de enviar el cambio, ejecuta:

   ```r
   lintr::lint_dir("APP")
   shiny::runApp("APP")
   ```

## Pull requests

- Explica el propósito y el comportamiento anterior y nuevo.
- Relaciona el issue correspondiente, si existe.
- Incluye capturas para cambios visuales.
- Indica las verificaciones realizadas y cualquier limitación del entorno.
- No incluyas datos sensibles, credenciales, binarios generados ni bibliotecas
  locales de `renv`.

Al contribuir, aceptas que tu trabajo se distribuya bajo la licencia MIT del
proyecto.
