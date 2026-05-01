## Scala Compilation

**ONLY use scala-metals tools to compile**, do not use Mill directly for basic compilation tasks and running tests.

To Investigate signatures of library dependencies, use the Cellar CLI - see below.

## Code Formatting
CI testing will fail if code is not correctly formatted. Use the following command to format code

```bash
scala fmt core && scala fmt demo
```
