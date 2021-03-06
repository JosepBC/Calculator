#ifndef TYPES_HEADER
#define TYPES_HEADER
#define DEBUG 0
#include <stdbool.h>
#include <stdio.h>

typedef enum Type_t {Int64, Float64, String, Bool, Int64Vector, Float64Vector, Int64Matrix, Float64Matrix} Type;
typedef struct Variable_t {
    Type type;
    char *var_name;
    union {
        char *String;
        int Int64;
        float Float64;
        unsigned char Bool;
        struct Int64Vector {
            int *v;
            int n_elem;
        } Int64Vector;
        struct Float64Vector {
            float *v;
            int n_elem;
        } Float64Vector;
        struct Int64Matrix {
            int *m;
            int n_rows;
            int n_cols;
        } Int64Matrix;
        struct Float64Matrix {
            float *m;
            int n_rows;
            int n_cols;
        } Float64Matrix;
    } val;
} Variable;

typedef struct Node_col_t {
    Variable val;
    struct Node_col_t *next;
    int n_elem_col;
    Type col_type;
} NodeCol;

typedef struct Node_row_t {
    NodeCol *val;
    struct Node_row_t *next;
    int n_elem_row;
    Type row_type;
} NodeRow;

extern bool is_int_or_float(Variable v1);
extern bool is_int(Variable v);
extern bool is_float(Variable v);
extern bool is_int_vector(Variable v);
extern bool is_float_vector(Variable v);
extern bool is_int_matrix(Variable v);
extern bool is_float_matrix(Variable v);
extern bool is_vector(Variable v);
extern bool is_string(Variable v);
extern bool is_bool(Variable v);
extern void error(char *str);

extern void store_val(Variable var, FILE *out);
extern void crop_first_last_elem(char **str);
extern void print_node_row(NodeRow *row);
extern void store_matrix(NodeRow *row, Variable *var);
extern void print_var(char* str, Variable var, FILE *out);


extern void get_val(char *key, Variable *v);

extern void get_vector_elem(char *vector_name, int idx, Variable *dst);
extern void get_matrix_elem(char *matrix_name, int row, int col, Variable *dst);
extern void get_id_vector_elem(char *vector_name, char *vector_idx_name, Variable *res);
extern void get_id_matrix_elem(char *matrix_name, char *row_idx_name, char *col_idx_name, Variable *dst);

#endif