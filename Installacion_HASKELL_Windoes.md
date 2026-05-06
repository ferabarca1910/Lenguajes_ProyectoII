# Instalación de Haskell en Windows con GHCup

[GHCup](https://www.haskell.org/ghcup/) es la forma recomendada de instalar y administrar GHC, Cabal, Stack y Haskell Language Server (HLS) en Windows.

Documentación oficial: [Installation - GHCup](https://www.haskell.org/ghcup/install/).

---

## 1. Requisitos

- Abrir **PowerShell** (no es obligatorio “Ejecutar como administrador”; la guía oficial indica ejecutar el instalador como usuario normal).
- Conexión a internet estable.
- Varios gigabytes de espacio libre (GHC, MSYS2 y herramientas asociadas).

---

## 2. Instalación con un solo comando (oficial)

En **PowerShell**, copia y ejecuta **toda esta línea** (es un solo comando):

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

Ese script descarga el binario de `ghcup` (por defecto en `C:\ghcup\bin` en Windows) y lanza el asistente para instalar el resto del entorno.

---

## 3. Durante el asistente

- Acepta instalar **GHC**; la versión etiquetada como **recommended** suele ser la opción más segura para compatibilidad con librerías y HLS.
- Permite la instalación o configuración de **MSYS2** si el asistente la ofrece (en Windows es habitual para compilar código nativo).
- Instala **Cabal**.
- **Stack** y **HLS** son opcionales pero recomendables:
  - **Stack**: proyectos y reproducibilidad.
  - **HLS (Haskell Language Server)**: análisis estático, autocompletado y errores en el editor (por ejemplo VS Code o Cursor con la extensión de Haskell).

---

## 4. Después de instalar

1. **Cierra y vuelve a abrir** PowerShell, o reinicia el editor (Cursor / VS Code), para que se carguen las variables de entorno y el `PATH` actualizado.
2. Comprueba que las herramientas responden:

```powershell
ghcup --version
ghc --version
cabal --version
```

3. Si aún no instalaste HLS desde el asistente, puedes instalarlo después:

```powershell
ghcup install hls --set recommended
```

---

## 5. Si algo falla

- **Antivirus o firewall**: pueden bloquear descargas o el script; revisa alertas y permite la ejecución si confías en el origen (`haskell.org`).
- **Política de ejecución**: el comando anterior ya usa `Bypass` solo para **esa sesión** de PowerShell (`-Scope Process`), sin cambiar la política del sistema de forma permanente.
- **Instalación manual**: si el script no funciona, en la misma página oficial hay un apartado para Windows con pasos manuales (descargar `ghcup.exe`, MSYS2, variables `GHCUP_MSYS2`, `CABAL_DIR`, etc.): [Installation - GHCup — Windows (manual)](https://www.haskell.org/ghcup/install/).

---

## 6. Compilar este proyecto (sin `.cabal` todavía)

Cuando `ghc` esté en el PATH, desde la carpeta del repositorio puedes probar:

```powershell
cd C:\Users\BM\Desktop\gits\Lenguajes_ProyectoII
ghc Main.hs -o finanzas
```

Luego ejecutar:

```powershell
.\finanzas.exe
```

**Nota:** Si el proyecto pasa a usar un archivo `.cabal` o Stack, lo habitual será compilar y ejecutar con `cabal run` o `stack run`; conviene documentarlo en el README del curso.

---

## 7. Enlaces útiles

| Recurso | URL |
|--------|-----|
| Instalación GHCup | https://www.haskell.org/ghcup/install/ |
| Primeros pasos (Hello World, REPL) | https://www.haskell.org/ghcup/steps/ |
| Guía de usuario GHCup | https://www.haskell.org/ghcup/guide/ |
| Extensión VS Code / Cursor (Haskell) | https://github.com/haskell/vscode-haskell |

---

## 8. Desinstalación (referencia)

En Windows, la documentación de GHCup indica que suele haber un script **Uninstall Haskell.ps1** en el Escritorio tras la instalación; ejecútalo con “Run with PowerShell” si necesitas quitar todo el entorno.
