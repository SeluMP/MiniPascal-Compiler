%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "listaSimbolos.h"
#include "listaCodigo.h"
#include <assert.h>

extern int yylex();
extern int yylineno;


char registros[10] = {0};
int errores_sintaticos = 0;
int errores_semanticos  = 0;
int contador_strings = 1;
int errores = 0;                // Errores con funcionalidad no implementada
int contador_etiq = 0;
extern int errores_lexicos;
void yyerror(const char *msg);
int ok();
void imprimirLS();
void imprimirCodigo(ListaC l);
char *obtenerReg();
char *nuevaEtiqueta();
char *concatena(char *arg1, char *arg2);
char *concatenaNum(char *arg1, int num);
void liberaReg(char *reg);

// Declaramos una lista de símbolos
Lista l;

%}

// Para añadir listaCodigo.h a tab.h
%code requires{
    #include "listaCodigo.h"
}

/* Definición de tipos de datos de la gramática */

%union {
        char *str;
        ListaC codigo;
}


/* Definición de tokens */

%token PROGRAM
%token FUNCTION
%token CONST
%token VAR
%token INTEGER
%token BEGINN
%token END
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token FOR
%token TO
%token WRITE
%token READ
%token LPAREN
%token PUNTO
%token ASTERISCO
%token BARRA
%token RPAREN
%token SEMICOLON
%token COMMA
%token ASSIGNOP
%token DOSPUNTOS
%token PLUSOP
%token MINUSOP
%token <str> ID
%token <str> NUM
%token <str> STRING

%type <codigo> expression statement print_item print_list read_list declarations constants compound_statement optional_statements statements FOR TO DO

/* Flags de funcionamiento de BISON */

%define parse.error verbose     // genera mensajes de error extensos
%define parse.trace             // genera trazas con yydebug = 1

/* Precedencia de los operadores */

%left PLUSOP MINUSOP
%left ASTERISCO BARRA
%left UMENOS
%right THEN ELSE


%%

/* Reglas de produccion */

program : { l = creaLS(); } PROGRAM ID LPAREN RPAREN SEMICOLON functions declarations compound_statement PUNTO { 
            if(ok()){
                imprimirLS();
                concatenaLC($8, $9);
                imprimirCodigo($8);
                liberaLC($8);
				liberaLC($9);
                liberaLS(l);
            }
        }

functions   : functions function SEMICOLON
            | %empty 

function    : FUNCTION ID LPAREN CONST identifiers DOSPUNTOS type RPAREN DOSPUNTOS type declarations compound_statement { 
                printf("Error: funciones no soportadas\n");
                errores++;
            }

declarations    : declarations VAR identifiers DOSPUNTOS type SEMICOLON {
                        if(ok()){
                            $$ = $1;
                        }
                }
                | declarations CONST constants SEMICOLON { 
                    if(ok()){
                        $$ = $1;
                        concatenaLC($$, $3);
                        liberaLC($3);
                    }
                }
                | %empty {
                        if(ok()){
                            $$ = creaLC();
                        }
                 }

identifiers : ID { 
                // Comprobar si $1 está en la lista
                PosicionLista p = buscaLS(l,$1);
                if(p != finalLS(l)){
                    // Redeclaración de identificador existente
                    printf("Error en linea %d: %s redeclarado\n",yylineno,$1);
                }
                else{
                    // Primera declaración de $1
                    Simbolo aux;
                    aux.nombre = $1;
                    aux.tipo = VARIABLE;
                    insertaLS(l,finalLS(l),aux);
                }
            }       
            | identifiers COMMA ID {
                // Comprobar si $3 está en la lista
                PosicionLista p = buscaLS(l,$3);
                if(p != finalLS(l)){
                    // Redeclaración de identificador existente
                    printf("Error en linea %d: %s redeclarado\n",yylineno,$3);
                }
                else{
                    // Primera declaración de $3
                    Simbolo aux;
                    aux.nombre = $3;
                    aux.tipo = VARIABLE;
                    insertaLS(l,finalLS(l),aux);
                }
            }

type    : INTEGER

constants   : ID ASSIGNOP expression {
                // Comprobar si $1 está en la lista
                PosicionLista p = buscaLS(l,$1);
                if(p != finalLS(l)){
                    // Redeclaración de identificador existente
                    printf("Error en linea %d: %s redeclarado\n",yylineno,$1);
                }
                else{
                    // Primera declaración de $1
                    Simbolo aux;
                    aux.nombre = $1;
                    aux.tipo = CONSTANTE;
                    insertaLS(l,finalLS(l),aux);
                }
                if(ok()){
                    $$ = creaLC();
                    Operacion oper;
                    oper.op = "sw";
                    oper.res = recuperaResLC($3);
                    oper.arg1 = concatena("_", $1);
                    oper.arg2 = NULL;
                    insertaLC($$, finalLC($$), oper);
                    liberaReg(recuperaResLC($3));
                }
        
            }
            | constants COMMA ID ASSIGNOP expression {
                // Comprobar si $3 está en la lista
                PosicionLista p = buscaLS(l,$3);
                if(p != finalLS(l)){
                    // Redeclaración de identificador existente
                    printf("Error en linea %d: %s redeclarado\n",yylineno,$3);
                }
                else{
                    // Primera declaración de $3
                    Simbolo aux;
                    aux.nombre = $3;
                    aux.tipo = CONSTANTE;
                    insertaLS(l,finalLS(l),aux);
                }
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$5);
                    Operacion oper;
                    oper.op = "sw";
                    oper.res = recuperaResLC($5);
                    oper.arg1 = concatena("_", $3);
                    oper.arg2 = NULL;
                    insertaLC($$, finalLC($$), oper);
                    liberaReg(recuperaResLC($5));
                    liberaLC($5);
                }
            }

compound_statement  : BEGINN optional_statements END { 
                        if(ok()){
                            $$ = $2;
                        }
                    }

optional_statements : statements { 
                        if(ok()){
                            $$ = $1;
                        }
                    }
                    | %empty {
                        if(ok()){
                            $$ = creaLC();
                        }
                    }

statements  : statement {
                if(ok()){
                    $$ = $1;
                }
            }
            | statements SEMICOLON statement {
                if(ok()){
                    concatenaLC($1, $3);
                    liberaLC($3);
                }
            }

statement   : ID ASSIGNOP expression{
            // Comprobamos si el identificador está en la lista de símbolos
            PosicionLista p = buscaLS(l,$1);
            if(p == finalLS(l)){
                printf("Error en linea %d: %s símbolo no declarado\n",yylineno,$1);
                errores_semanticos++;
            }
            else{
                    Simbolo aux = recuperaLS(l,p);
                    if(aux.tipo == CONSTANTE){
                        printf("Error en linea %d: %s es una constante, no se puede modificar\n",yylineno,$1);
                        errores_semanticos++;
                    } 
            }
            if(ok()){
                $$ = $3;
                Operacion oper;
                oper.op = "sw";
                oper.res = recuperaResLC($3);
                oper.arg1 = concatena("_",$1);
                oper.arg2 = NULL;
                insertaLC($$,finalLC($$),oper);
                liberaReg(oper.res);
            }
            }
            | IF expression THEN statement {
                if(ok()){
                    $$ = $2;

                    // expression
                    char *reg = recuperaResLC($2);

                    // IF
                    Operacion oper;
                    oper.op = "beqz";
                    oper.res = reg;
                    oper.arg1 = nuevaEtiqueta();
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    // statement
                    concatenaLC($$,$4);
                    Operacion oper1;
                    oper1.op = "etiq";
                    oper1.res = oper.arg1;
                    oper1.arg1 = NULL;
                    oper1.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper1);
                    liberaLC($4);
                    liberaReg(reg);
                }
            }
            | IF expression THEN statement ELSE statement {
                if(ok()){
                    $$ = $2;
				    char *etiqueta1 = nuevaEtiqueta();
				    char *etiqueta2 = nuevaEtiqueta();

				    // expression
				    char *reg = recuperaResLC($2);

				        // IF
				    Operacion oper;
				    oper.op = "beqz";
				    oper.res = reg;
				    oper.arg1 = etiqueta1;
				    oper.arg2 = NULL;
				    insertaLC($$, finalLC($$), oper);

				    // statement
				    concatenaLC($$, $4);

				    // b(Salto incondicional)
				    Operacion b;
				    b.op = "b";
				    b.res = etiqueta2;
				    b.arg1 = NULL;
				    b.arg2 = NULL;
				    insertaLC($$, finalLC($$), b);

				    // Primera etiqueta (no se cumple el IF)
				    Operacion etiq1;
				    etiq1.op = "etiq";
				    etiq1.res = etiqueta1;
				    etiq1.arg1 = NULL;
				    etiq1.arg2 = NULL;
				    insertaLC($$, finalLC($$), etiq1);

				    // statement
				    concatenaLC($$, $6);

				    // Segunda etiqueta (se cumple el IF y debemos saltarnos las sentencias del ELSE)
				    Operacion etiq2;
				    etiq2.op = "etiq";
				    etiq2.res = etiqueta2;
				    etiq2.arg1 = NULL;
				    etiq2.arg2 = NULL;
				    insertaLC($$, finalLC($$), etiq2);

                    // Liberamos los registros
				    liberaLC($4);
				    liberaLC($6);
				    liberaReg(reg);
                }
            }
            | WHILE expression DO statement {
                if(ok()){
                    $$ = creaLC();
                    char *reg = recuperaResLC($2);
				    char *etiqueta1 = nuevaEtiqueta();
				    char *etiqueta2 = nuevaEtiqueta();
				
				    // Etiqueta 1 (Para volver al inicio)
				    Operacion etiq1;
				    etiq1.op = "etiq";
				    etiq1.res = etiqueta1;
				    etiq1.arg1 = NULL;
				    etiq1.arg2 = NULL;
				    insertaLC($$, finalLC($$), etiq1);
                
                    // expression
                    concatenaLC($$,$2);

				    // IF
				    Operacion salto;
				    salto.op = "beqz";
				    salto.res = reg;
				    salto.arg1 = etiqueta2;
				    salto.arg2 = NULL;
				    insertaLC($$, finalLC($$), salto);

				    // statement
				    concatenaLC($$, $4);

				    // Salto incondicional para volver al comienzo (ejecución normal WHILE)
				    Operacion b;
				    b.op = "b";
				    b.res = etiqueta1;
				    b.arg1 = NULL;
				    b.arg2 = NULL;
				    insertaLC($$, finalLC($$), b);

				    //Etiqueta 2 (Para salir del bucle)
				    Operacion etiq2;
				    etiq2.op = "etiq";
				    etiq2.res = etiqueta2;
				    etiq2.arg1 = NULL;
				    etiq2.arg2 = NULL;
				    insertaLC($$, finalLC($$), etiq2);

				    liberaLC($2);
				    liberaLC($4);
				    liberaReg(reg);
                }
            }
            | FOR ID ASSIGNOP expression TO expression DO statement
            | WRITE LPAREN print_list RPAREN { 
                if(ok()){
                     $$ = $3;
                }
            }
            | READ LPAREN read_list RPAREN {
                if(ok()){
                    $$  = $3;
                }
            }
            | compound_statement {
                if(ok()){
                    $$ = $1;
                }
            }
        
print_list  : print_item{
                if(ok()){
                    $$ = $1;
                }
            }
            | print_list COMMA print_item { 
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$3);
                    liberaLC($3);
                }
            }

print_item  : expression {
                if(ok()){
                    $$ = $1;
                    Operacion oper;
                    oper.op = "li";
                    oper.res = "$v0";
                    oper.arg1 = "1";
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "move";
                    oper.res = "$a0";
                    oper.arg1 = recuperaResLC($1);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "syscall";
                    oper.res = NULL;
                    oper.arg1 = NULL;
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                    liberaReg(recuperaResLC($1));
                }
            }
            | STRING {
                Simbolo aux;
                aux.nombre = $1;
                aux.tipo = CADENA;
                aux.valor = contador_strings;
                insertaLS(l,finalLS(l),aux);
                contador_strings++;

                if(ok()){
                    $$ = creaLC();
                    Operacion oper;
                    oper.op = "li";
                    oper.res = "$v0";
                    oper.arg1 = "4";
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "la";
                    oper.res = "$a0";
                    oper.arg1 = concatenaNum("$str",aux.valor);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "syscall";
                    oper.res = NULL;
                    oper.arg1 = NULL;
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                }
            }

read_list   : ID {
                // Comprobamos si el identificador está en la lista de símbolos
                PosicionLista p = buscaLS(l,$1);
                // Simbolo no encontrado
                if(p == finalLS(l)){
                    printf("Error en linea %d: %s símbolo no declarado\n",yylineno,$1);
                    errores_semanticos++;
                }
                else{
                Simbolo aux = recuperaLS(l,p);
                if(aux.tipo == CONSTANTE){
                    printf("Error en linea %d: %s no es una variable\n",yylineno,$1);
                    errores_semanticos++;
                }
                }
                if(ok()){
                    $$ = creaLC();
                    Operacion oper;
                    oper.op = "li";
                    oper.res = "$v0";
                    oper.arg1 = "5";
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "syscall";
                    oper.res = NULL;
                    oper.arg1 = NULL;
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "sw";
                    oper.res = "$v0";
                    oper.arg1 = concatena("_",$1);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                }
            }
            | read_list COMMA ID {
                // Comprobamos si el $3 está en la lista de símbolos
                PosicionLista p = buscaLS(l,$3);
                // Simbolo encontrado
                if(p == finalLS(l)){
                    printf("Error en linea %d: %s símbolo no declarado\n",yylineno,$3);
                    errores_semanticos++;
                }
                else{    
                    Simbolo aux = recuperaLS(l,p);
                    if(aux.tipo == CONSTANTE){
                        printf("Error en linea %d: %s no es una variable\n",yylineno,$3);
                       errores_semanticos++;
                    }
                }
                if(ok()){
                    $$ = $1;
                    Operacion oper;
                    oper.op = "li";
                    oper.res = "$v0";
                    oper.arg1 = "5";
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "syscall";
                    oper.res = NULL;
                    oper.arg1 = NULL;
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);

                    oper.op = "sw";
                    oper.res = "$v0";
                    oper.arg1 = concatena("_",$3);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                }
            }

expression  : expression PLUSOP expression { 
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$3);
                    Operacion oper;
                    oper.op = "add";
                    oper.res = recuperaResLC($1);
                    oper.arg1 = recuperaResLC($1);
                    oper.arg2 = recuperaResLC($3);
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                    liberaReg(oper.arg2);
                    liberaLC($3);
                }
            }
            | expression MINUSOP expression { 
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$3);
                    Operacion oper;
                    oper.op = "sub";
                    oper.res = recuperaResLC($1);
                    oper.arg1 = recuperaResLC($1);
                    oper.arg2 = recuperaResLC($3);
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                    liberaReg(oper.arg2);
                    liberaLC($3);
                }
            }
            | expression ASTERISCO expression {
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$3);
                    Operacion oper;
                    oper.op = "mul";
                    oper.res = recuperaResLC($1);
                    oper.arg1 = recuperaResLC($1);
                    oper.arg2 = recuperaResLC($3);
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                    liberaReg(oper.arg2);
                    liberaLC($3);
                }
            }
            | expression BARRA expression { 
                if(ok()){
                    $$ = $1;
                    concatenaLC($$,$3);
                    Operacion oper;
                    oper.op = "div";
                    oper.res = recuperaResLC($1);
                    oper.arg1 = recuperaResLC($1);
                    oper.arg2 = recuperaResLC($3);
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                    liberaReg(oper.arg2);
                    liberaLC($3);
                }
            }
            | MINUSOP expression %prec UMENOS {
                if(ok()){
                    $$ = $2;
                    Operacion oper;
                    oper.op = "neg";
                    oper.res = recuperaResLC($2);
                    oper.arg1 = recuperaResLC($2);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                }
             }
            | LPAREN expression RPAREN { 
                if(ok()){
                    $$ = $2;
                }
            }
            | ID {
                // Comprobamos si el identificador está en la lista de símbolos
                PosicionLista p = buscaLS(l,$1);
                // Simbolo no encontrado
                if(p == finalLS(l)){
                    printf("Error en linea %d: %s símbolo no declarado\n",yylineno,$1);
                    errores_semanticos++;
                } 
                if(ok()){
                    $$ = creaLC();
                    Operacion oper;
                    oper.op = "lw";
                    oper.res = obtenerReg();
                    oper.arg1 = concatena("_",$1);
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                }
            }
            | NUM { 
                if(ok()){
                    $$ = creaLC();
                    Operacion oper;
                    oper.op = "li";
                    oper.res = obtenerReg();
                    oper.arg1 = $1;
                    oper.arg2 = NULL;
                    insertaLC($$,finalLC($$),oper);
                    guardaResLC($$,oper.res);
                }
            }
            | ID LPAREN optional_arguments RPAREN { 
                printf("Error: funciones no soportadas\n");
                errores++;
            }

optional_arguments  : expressions
                    | %empty

expressions : expression
            | expressions COMMA expression { }
%%

void yyerror(const char *msg){
    printf("Sintactic error: %s at line %d\n",msg,yylineno);
    errores_sintaticos++;
}

int ok(){
    return !(errores_lexicos + errores_sintaticos + errores_semanticos);
}

char *obtenerReg(){
    for(int i = 0; i < 10; i++){
        if(registros[i] == 0){
            registros[i] = 1;
            char aux[16];
            sprintf(aux,"$t%d",i);
            return strdup(aux);     // Duplicado en memoria dinámica para evitar                           
        }                           // pérdida de información
    }
    printf("Error: registros agotados\n");
    exit(1);
}

char *concatena(char *arg1, char *arg2){
    char *aux = (char*)malloc(strlen(arg1)+strlen(arg2)+1);
    sprintf(aux,"%s%s",arg1,arg2);
    return aux;
}

char *concatenaNum(char *arg1, int num){
    char *aux = (char*)malloc(strlen(arg1)+4+1);
    sprintf(aux,"%s%d",arg1,num);
    return aux;
}

void liberaReg(char *reg){
    if(reg[0] == '$' && reg[1] == 't'){
        int aux = reg[2] - '0';
        assert(aux >= 0);   // Comprobamos que está dentro de los límites
        assert(aux < 10);   // evitando así entrar a direcciones de memoria erroneas
        registros[aux] = 0;
    }
}

char *nuevaEtiqueta(){
    char aux[16];
    sprintf(aux,"$l%d",contador_etiq++);
    return strdup(aux);
}

void imprimirLS(){
    // Recorremos la lista de símbolos para generar .data
    printf("#########################\n");
	printf("# Sección de datos\n");
    printf("      .data\n");
    PosicionLista p = inicioLS(l);
     while (p != finalLS(l)) {
        Simbolo aux = recuperaLS(l,p);
        // Volcar información de símbolo
        if (aux.tipo == CADENA){
            printf("$str%d:\n       .asciiz %s\n",aux.valor,aux.nombre);
        }
        p = siguienteLS(l,p);
    }
    printf("\n");
    p = inicioLS(l);
    while (p != finalLS(l)) {
        Simbolo aux = recuperaLS(l,p);
        // Volcar información de símbolo
        if (aux.tipo == VARIABLE || aux.tipo == CONSTANTE){
            printf("_%s:\n    .word 0\n",aux.nombre);
        }
        p = siguienteLS(l,p);
    }
    printf("\n");
}

void imprimirCodigo(ListaC l) {
	printf("##################\n");
	printf("# Sección de código\n");
	printf("    .text\n");
	printf("	.globl main\n");
	printf("main:\n");
	PosicionListaC p = inicioLC(l);
	Operacion oper;
	while (p != finalLC(l)) {
		oper = recuperaLC(l, p);
		if (oper.op == "etiq") {
			printf("%s:", oper.res);
		} else {
			printf("	%s", oper.op);
			if (oper.res) printf("	%s", oper.res);
			if (oper.arg1) printf(", %s", oper.arg1);
			if (oper.arg2) printf(", %s", oper.arg2);
		}
		printf("\n");
		p = siguienteLC(l, p);
	}
	printf("\n##############\n");
	printf("# Fin\n");
	printf("	jr  $ra\n");
}