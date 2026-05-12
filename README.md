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
- **Presupuestos (2.2)**: opción **7** (definir/actualizar tope por categoría, en memoria); opción **8** (comparar gastos reales vs tope y listar **alertas** si hay exceso). Tras la 7 se muestran alertas inmediatas con los registros cargados.
- **Sistema de reglas (2.5)**: opción **9** — definir reglas en memoria: (1) si los **gastos** (`Expense`) en una **categoría** superan un **monto** → **alerta**; (2) si el **ahorro total** (suma de `Saving`) es **menor** a un valor → **advertencia**. Opción **10** — ejecutar `evaluarReglas` sobre los registros cargados y mostrar mensajes de las reglas que se disparan.
- **Análisis avanzado (2.3)**: opción **11** — flujo de caja mensual (`resumenMensual`), tendencia de gastos mes a mes (`tendenciaGastoMensual`), proyección del próximo mes como promedio histórico de gastos mensuales (`proyeccionGastoSiguienteMes`), top 5 categorías por monto de **gasto** (`topCategoriasGasto`).
- **Simulación (2.4)**: reducción de gastos (%) y proyección de ahorro por meses
- **Reportes (2.7)**: resumen mensual, comparación entre periodos (mes vs mes), top categorías

## Persistencia (archivos generados)

- **`records.txt`**: un **registro por línea** (cada línea es el `show` de un `FinancialRecord`). Si el archivo viejo es una sola línea que empieza con `[`, el programa aún lo puede leer como lista. Conviene no editar a mano para no romper la carga.
- **Presupuestos y reglas** (opciones 7–10): solo en **memoria** mientras el programa corre; al cerrar no se guardan en disco.

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
