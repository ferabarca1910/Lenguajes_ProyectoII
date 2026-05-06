# Lenguajes_ProyectoII
En este repositorio trabajamos el **Proyecto 2 (Programación funcional en Haskell)** del curso de Lenguajes de Programación.

## Requisitos

- **Windows 10/11**
- **GHC (Haskell)** instalado y disponible en la terminal (PowerShell o CMD)
  - Recomendado: instalar con **GHCup** o con el instalador de Haskell para Windows.

## Instalación y ejecución (Windows)

1. Abrir una terminal en la carpeta del proyecto.
2. Compilar:

```bash
ghc --make Main.hs -o Main.exe
```

3. Ejecutar:

```bash
.\Main.exe
```

Si vas a recompilar después de cambios, podés usar:

```bash
ghc --make Main.hs -fforce-recomp -o Main.exe
```

## Uso (menú)

El programa corre en consola y permite:

- Agregar **ingreso**, **gasto**, **ahorro** e **inversión**
- Ver registros y ver balance
- Presupuestos por categoría + alertas por exceso
- Reglas del sistema + evaluación de alertas/advertencias
- **Análisis avanzado (2.3)**: flujo mensual, tendencia, proyección, top categorías
- **Simulación (2.4)**: reducción de gastos (%) y proyección de ahorro por meses
- **Reportes (2.7)**: resumen mensual, comparación entre periodos (mes vs mes), top categorías

## Persistencia (archivos generados)

El sistema guarda y carga datos desde archivos de texto en la carpeta del proyecto (si no existen, se crean al guardar):

- `records.txt`: lista de `FinancialRecord`
- `budgets.txt`: lista de `Budget`
- `rules.txt`: lista de `Rule`

Nota: el formato actual usa serialización con `show`/`read`. Por eso es importante **no editar a mano** estos archivos, para evitar errores al cargar.

## Estructura del proyecto

- `Main.hs`: interfaz de consola (menús, entradas, salida a pantalla)
- `Types.hs`: definición de tipos (`FinancialRecord`, `Budget`, `Rule`, etc.)
- `Logic.hs`: lógica funcional (balance, presupuestos, reglas, análisis, simulación, reportes)
- `Storage.hs`: guardar/cargar desde archivos

## Cierre de entrega (TEC Digital)

Además del repositorio, se debe entregar un documento en TEC Digital. Sugerencia de contenido:

### Manual de usuario (recomendado)

- Cómo ejecutar el programa en Windows (comandos)
- Explicación del menú (qué hace cada opción)
- Ejemplos cortos de uso (ej. agregar gasto, correr análisis, correr simulación, ver reportes)
- Qué archivos se generan (persistencia) y advertencia de no editarlos a mano

### Arquitectura / diseño (recomendado)

- Descripción por módulos (`Types`, `Storage`, `Logic`, `Main`)
- Decisiones importantes:
  - Convención de balance (gastos restan; ingresos/ahorro/inversión suman)
  - Presupuestos por categoría (normalización de categoría)
  - Reglas configurables y evaluación
  - Análisis mensual (agrupación por mes) y simulación

### Enlace al repositorio

- Incluir el link de GitHub del proyecto y evidencia de commits del equipo.
