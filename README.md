# MiniPascal-Compiler
Compilador de Pascal con funcionalidad reducida. Este lenguaje trabaja con enteros y strings como tipo de datos, la funcionalidad se ha visto reducida por la complejidad de este proyecto, por lo que solo contaremos con variables, constantes y sentencias de control como if/else y while. El compilador puede dividirse en dos partes: el analizador léxico implementado en flex, que se encarga de evitar los errores gramaticales y estructurales en los tokens que posteriormente serán recibidos para ser procesados por el analizador sintáctico/semántico implementado en bison. Si unimos estas dos herramientas podemos obtener un compilador que traduce de lenguaje miniPascal a ensamblador, posteriormente haciendo uso de herramientas como Mars o spim podremos ejecutar dichas instrucciones y probar pequeños programas.
