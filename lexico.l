%{
#include "sintactico.tab.h"
void error_lexico();
void comentarioSinCerrar();
int entero_valido();
int identificador_valido();
void cadenaSinCerrar();
int errores_lexicos = 0;
int inicioComentario = 0;
%}

%x condComentario
%option yylineno
digito               [0-9]
letra                [a-zA-Z]
cadena               \"[^"]*\"
cadena2              \"[^"\n]*((\\\")|[\n]{1})   
entero               {digito}+
incorrecto	         [^a-zA-Z0-9;:\._" \n\t,+\-*/()=]+

%%

"(*"                                               {BEGIN(condComentario);inicioComentario = yylineno;}
<condComentario>.                                  ;
<condComentario>[ \n\t]+                           ;
<condComentario><<EOF>>                            {BEGIN(0);comentarioSinCerrar(inicioComentario); return 0;}
<condComentario>"*)"                               {BEGIN(0);}
[ \n\t]+                                           ;
\/\/[^\n]*[\n]                                     ;
(?i:program)                                       return PROGRAM;
(?i:function)                                      return FUNCTION;
(?i:const)                                         return CONST;
(?i:var)                                           return VAR;
(?i:integer)                                       return INTEGER;
(?i:begin)                                         return BEGINN;
(?i:end)                                           return END;
(?i:if)                                            return IF;
(?i:then)                                          return THEN;
(?i:else)                                          return ELSE;
(?i:while)                                         return WHILE;
(?i:do)                                            return DO;
(?i:for)                                           return FOR;
(?i:to)                                            return TO;
(?i:write)                                         return WRITE;
(?i:read)                                          return READ;
({letra}|_)(({letra}|_|{digito})*)                 return identificador_valido();
{entero}                                           return entero_valido();
"("                                                return LPAREN;
"."                                                return PUNTO;
"*"                                                return ASTERISCO;
"/"                                                return BARRA;
")"                                                return RPAREN;
";"                                                return SEMICOLON;
","                                                return COMMA;
":="                                               return ASSIGNOP;
":"                                                return DOSPUNTOS;
"+"                                                return PLUSOP;
"-"                                                return MINUSOP;
{cadena2}                                          cadenaSinCerrar();                        
{cadena}                                           {yylval.str = strdup(yytext); return STRING;}
{incorrecto}			                              {printf("Error en linea %d de la cadena %s",yylineno,yytext); errores_lexicos++;}
.                                                  error_lexico();
%%

int entero_valido()
{
 if (atoll(yytext) >=-2147483648 && atoll(yytext) <=2147483648) {
    yylval.str = strdup(yytext);
    return NUM;
   }
 else {
    printf("\nERROR, entero no válido %s. Error en linea: %d\n", yytext,yylineno);
    return NUM;
   }
}

int identificador_valido()
{
 if (yyleng <= 16) {
    yylval.str = strdup(yytext);
    return ID;
   }
 else {
    printf("\nERROR, identificador %s demasiado largo, en linea: %d\n", yytext,yylineno);
    return ID;
   }
}
void error_lexico() {
  printf("\nERROR, símbolo no reconocido %s\n",yytext);
  errores_lexicos++;
}

void comentarioSinCerrar(int inicio) {
   printf("\nERROR, comentario multilinea sin cerrar %s, que empieza en linea %d y acaba en la linea %d\n",yytext,inicio,yylineno);
   errores_lexicos++;
}


void cadenaSinCerrar() {
   printf("\nERROR, cadena sin cerrar %s, que acaba en linea %d\n",yytext,yylineno);
   errores_lexicos++;
}