# Estado del proyecto — Programación funcional (Haskell)

Resumen frente al enunciado **Proyecto 2: Programación funcional** y al código actual del repositorio.

---

## Lo que ya está hecho

- **Modelo de datos** (`Types.hs`): tipos de registro `Income`, `Expense`, `Saving`, `Investment` y `FinancialRecord` con monto, categoría, fecha, descripción y lista de etiquetas.
- **Interfaz básica** (`Main.hs`): menú con agregar ingreso, agregar gasto, ver registros, ver balance, guardar y salir.
- **Persistencia** (`Storage.hs`): guardar y cargar la lista de registros en `records.txt` usando serialización con `show` / `read`.
- **Lógica mínima** (`Logic.hs`): función `calcularBalance` (nota: hoy todo lo que no es `Income` se resta; conviene definir bien el signo para ahorro e inversión según el criterio del curso).

---

## Lo que falta respecto al enunciado

### 1. Registro avanzado de registros financieros

- El menú solo expone **ingreso** y **gasto**; faltan flujos para **ahorro** e **inversión**.
- Al crear registros, **categoría**, **fecha** y **etiquetas** están fijos en código; el usuario debería poder ingresarlos (con validación razonable de monto/fecha).

### 2. Presupuestos

- Definir **presupuestos por categoría**.
- **Comparar** gastos (o movimientos) reales vs presupuesto.
- **Alertas** cuando se exceda el presupuesto.

### 3. Sistema de reglas

- Reglas del tipo: si los gastos en una categoría superan un monto → **alerta**.
- Si el ahorro es menor a un valor → **advertencia**.
- Modelo de reglas + forma de evaluarlas (por ejemplo desde una opción del menú).

### 4. Análisis financiero avanzado

- **Flujo de caja mensual**.
- **Tendencias** de gasto.
- **Proyección** de gastos a partir de datos históricos.
- **Categorías** con mayor impacto financiero.

### 5. Simulación financiera

- Simular **reducción de gastos** en un porcentaje dado.
- **Proyección de ahorro** en el tiempo.

### 6. Persistencia estructurada (refinar)

- Ya hay archivo y reconstrucción vía `read`. Opcional: formato más explícito (CSV, líneas por registro, etc.) y documentación del formato en el README.

### 7. Reportes

- **Resumen mensual**.
- **Comparación entre periodos**.
- **Categorías con mayor gasto** (u otro criterio alineado al enunciado).

### 8. Aspectos administrativos y entrega

- Documento en **TEC Digital**: portada, índice, enlace a **GitHub**, instalación, manual de usuario, arquitectura lógica.
- **Commits** regulares en GitHub con contribuciones visibles del grupo.
- Si se usan **librerías** además de lo estándar: indicarlo en documentación (y en proyecto con `cabal` si aplica) para evitar descuentos.

**Fecha de entrega (enunciado):** martes **12 de mayo de 2026**.

---

## Qué conviene hacer a continuación (orden sugerido)

1. Completar **registro** para los cuatro tipos y campos reales (categoría, fecha, tags).
2. Ajustar **balance** y convenciones de signo para ingreso, gasto, ahorro e inversión.
3. Implementar **presupuestos por categoría** + comparación + alertas.
4. Implementar **reglas** configurables y evaluación desde el menú.
5. Bloque de **análisis** (agrupación por mes/categoría usando `map`, `filter`, `fold`, etc.) + **simulación**.
6. **Reportes** en consola (resumen mensual, comparación de periodos, top categorías).
7. Cerrar **README** (compilar/ejecutar en Windows) y el informe para TEC Digital.

---

## Peso de la evaluación (recordatorio del enunciado)

| Aspecto | Porcentaje |
|--------|------------:|
| Documentación | 5% |
| Registro y gestión de registros | 20% |
| Presupuestos y reglas | 15% |
| Análisis financiero y simulación | 25% |
| Lógica funcional y modelado | 25% |
| Manejo de archivos persistentes | 10% |

Priorizar **análisis + simulación** y **modelado limpio** alinea bien con los porcentajes más altos una vez cubierto el registro completo y la persistencia.

---

*Generado como seguimiento del estado del código frente al documento del proyecto del curso.*
