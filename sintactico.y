%code requires {
    #include "types.h"
}

%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "symtab.h"

    int yylex();
    extern FILE* yyin;
    int yyerror(const char *s);
%}

%define parse.error verbose


%union { 
    Variable var;
    char *str;
    NodeRow *nr;
    NodeCol *nc;
};

%token<var> ID
%token<var> INT
%token<var> FLOAT
%token<var> STRING 
%token<var> BOOL
%token EQUALS
%token ENTER
%token OPEN_M
%token CLOSE_M
%token SEMICOLON
%token BOOL_EQUALS
%token BOOL_AND
%token BOOL_OR
%token BOOL_NOT
%token BOOL_LOWER_EQUAL
%token BOOL_HIGHER_EQUAL
%token BOOL_LOWER_THAN
%token BOOL_HIGHER_THAN
%token BOOL_DIFF
%token ARITHMETIC_POW
%token ARITHMETIC_MOD
%token ARITHMETIC_DIV
%token ARITHMETIC_MULT
%token ARITHMETIC_SUB
%token ARITHMETIC_ADD
%token OPEN_P
%token CLOSE_P

%type<var> int_expression;
%type<var> float_expression;
%type<var> string_expression;
%type<var> expression;
%type<var> id_expression;
%type<var> number;
%type<nc> number_list;
%type<nr> row_list;
%type<nc> row;
%type<var> m;
%type<var> boolean_expression;
%type<var> and_list;
%type<var> or_list;
%type<var> not;
%type<var> bool_equals_list;
%type<var> pow_list;
%type<var> mult_list;
%type<var> add_list;
%type<var> value;
%type<var> parenthesis;

%%
prog : sentence_list;
sentence_list : sentence_list sentence ENTER | sentence ENTER;
sentence : assignation_sentence | expression {print_var("Expression", $1);};
assignation_sentence : ID EQUALS expression {
    Variable v;
    v.var_name = $1.var_name;
    v.type = $3.type;
    switch(v.type) {
        case Int64:
            v.val.Int64 = $3.val.Int64;
            break;
        case Float64:
            v.val.Float64 = $3.val.Float64;
            break;
        case String:
            v.val.String = $3.val.String;
            break;
        case Bool:
            v.val.Bool = $3.val.Bool;
            break;
        case Int64Vector:
            v.val.Int64Vector = $3.val.Int64Vector;
            break;
        case Float64Vector:
            v.val.Float64Vector = $3.val.Float64Vector;
            break;
        case Int64Matrix:
            v.val.Int64Matrix = $3.val.Int64Matrix;
            break;
        case Float64Matrix:
            v.val.Float64Matrix = $3.val.Float64Matrix;
            break;
        default:
            yyerror("Unknown type\n");
    }
    store_val(v);
};


int_expression : INT {
    $$ = $1;
};

float_expression : FLOAT {
    $$ = $1;
};

string_expression : STRING {
    $$ = $1;
};

expression : add_list {$$ = $1;}

add_list : add_list ARITHMETIC_ADD mult_list {
    if(DEBUG) printf("add\n");

    do_add($1, $3, &$$);
} | add_list ARITHMETIC_SUB mult_list {
    if(DEBUG) printf("sub\n");

    do_sub($1, $3, &$$);
} | mult_list {$$ = $1;}


mult_list : mult_list ARITHMETIC_MULT pow_list {
    if(DEBUG) printf("mult\n");

    do_mult($1, $3, &$$);

} | mult_list ARITHMETIC_DIV pow_list {
    if(DEBUG) printf("div\n");

    do_div($1, $3, &$$);
} | mult_list ARITHMETIC_MOD pow_list {
    if(DEBUG) printf("mod\n");

    if(!is_int($1) || !is_int($3)) yyerror("Ilegal type in mod!");

    $$.type = Int64;
    $$.val.Int64 = $1.val.Int64 % $3.val.Int64;
} | pow_list { 
    $$ = $1;
}


pow_list : pow_list ARITHMETIC_POW value {
    if(DEBUG) printf("Pow list\n");
    if(!is_int_or_float($1) || !is_int_or_float($3)) yyerror("Ilegal type in pow!");

    do_pow($1, $3, &$$);
} | value {
    if(DEBUG) printf("expression\n");
    $$ = $1;
}

value : int_expression {$$ = $1;} | float_expression {$$ = $1;} | 
            string_expression {$$ = $1;} | boolean_expression {$$ = $1;} | 
            id_expression {get_val($1.var_name, &$$);} | m {$$ = $1;} | parenthesis {$$ = $1;} |
            ARITHMETIC_SUB parenthesis {do_chs($2, &$$);} |
            ARITHMETIC_SUB id_expression {get_val($2.var_name, &$$); do_chs($$, &$$);}


parenthesis : OPEN_P add_list CLOSE_P {$$ = $2;};


boolean_expression : or_list {
    $$ = $1;
    printf("Bool res: %i\n", $1.val.Bool);
};

or_list : or_list BOOL_OR and_list {
    $$.type = Bool;
    $$.val.Bool = $1.val.Bool || $3.val.Bool;
} | and_list {
    $$ = $1;
    printf("And res: %i\n", $1.val.Bool);
};

and_list : and_list BOOL_AND bool_equals_list {
    $$.type = Bool;
    $$.val.Bool = $1.val.Bool && $3.val.Bool;
    printf("And list\n");
} | bool_equals_list {
    $$ = $1;
};

bool_equals_list : bool_equals_list BOOL_EQUALS not {
    $$.type = Bool;
    $$.val.Bool = $1.val.Bool == $3.val.Bool;
} | not {
    $$ = $1;
}

not : BOOL_NOT BOOL {
    $$.type = Bool;
    $$.val.Bool = !$2.val.Bool;
    printf("Bool not: %i\n", $$.val.Bool);
} | BOOL {
    $$ = $1;
    printf("Not list: %i\n", $$.val.Bool);
};


id_expression : ID {
    $$.var_name = $1.var_name;
};

m : OPEN_M row_list CLOSE_M {
    if(DEBUG) print_node_row($2);
    store_matrix($2, &$$);
    if(DEBUG) print_var("matrix declaration", $$);
};

row_list : row SEMICOLON row_list {
    $$ = (NodeRow*) malloc(sizeof(NodeRow));
    $$->val = $1;
    $$->next = $3;
    $$->n_elem_row = $3->n_elem_row + 1;
    $$->row_type = $1->col_type == Float64 || $3->row_type == Float64 ? Float64 : Int64;
} | row {
    $$ = (NodeRow*) malloc(sizeof(NodeRow));
    $$->val = $1;
    $$->next = NULL;
    $$->row_type = $1->col_type;
    $$->n_elem_row = 1;
};

row : number_list {
    $$ = $1;

};

number_list : number number_list {
    //Creamos toda la lista
    $$ = (NodeCol*) malloc(sizeof(NodeCol));
    $$->val = $1;
    $$->next = $2;
    $$->n_elem_col = $2->n_elem_col + 1;
    $$->col_type = $1.type == Float64 || $2->col_type == Float64 ? Float64 : Int64;
} | number {
    //Creamos un nodo con toda la info de un numero
    $$ = (NodeCol*) malloc(sizeof(NodeCol));
    $$->val = $1;
    $$->next = NULL;
    $$->n_elem_col = 1;
    $$->col_type = $1.type;
};

number : INT {
    $$ = $1;
} | FLOAT {
    $$ = $1;
};

%%

int yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n",  s);
    exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
    //yyin = fopen(argv[1], "r");
    yyparse();
    return 1;
}