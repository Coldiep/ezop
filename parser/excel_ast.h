

#ifndef EXCEL_AST_H__
#define EXCEL_AST_H__

#include "ast.h"

namespace parser{

class excel_ast_node : public ast_node_t{

  enum en_excel_ast_node_type{
  
    en_excel_ast_unknown,

    // formula
    // --------------------
    en_excel_ast_formula,
    en_excel_ast_scalar_formula,
    en_excel_ast_array_formula,

    // expression
    // --------------------
    en_excel_ast_logical_expression,
    en_excel_ast_concat_expression,
    en_excel_ast_additive_expression,
    en_excel_ast_multiplicative_expression,
    en_excel_ast_exponent_expression,
    en_excel_ast_percent_expression,
    en_excel_ast_unary_expression,
    en_excel_ast_union_expression,
    en_excel_ast_intersection_expression,
    en_excel_ast_term_expression,
    en_excel_ast_parentesis_expression,

    // function call
    // --------------------
    en_excel_ast_function_call,
    en_excel_ast_function_name_prefix,
    en_excel_ast_function_name,
    en_excel_ast_function_call_argument_list,
    en_excel_ast_function_argument,
    en_excel_ast_function_comma,

    // array
    // --------------------
    en_excel_ast_const_array,
    en_excel_ast_array_element_list,
    en_excel_ast_array_element,
    en_excel_ast_array_comma,
    en_excel_ast_array_semicolon,

    // reference
    // --------------------
    en_excel_ast_reference,
    en_excel_ast_reference_prefix,

    // workbook
    // --------------------
    en_excel_ast_workbook,
    en_excel_ast_workbook_name,
    en_excel_ast_sheet,
    en_excel_ast_sheet_name,

    // range
    // --------------------
    en_excel_ast_range_column,
    en_excel_ast_range_r1_c1_column,
    en_excel_ast_range_row,
    en_excel_ast_range_r1_c1_row,
    en_excel_ast_range_cell,
    en_excel_ast_range_expression,
    en_excel_ast_range_sheet,

    // cell reference
    // --------------------
    en_excel_ast_cell,
    en_excel_ast_a1_column,
    en_excel_ast_r1_c1_column,
    en_excel_ast_a1_row,
    en_excel_ast_r1_c1_row,
    en_excel_ast_abs_reference,
    en_excel_ast_a1_column_designator,
    en_excel_ast_row_integer,
    en_excel_ast_column_c,
    en_excel_ast_row_r,
    en_excel_ast_r1_c1_offset,
    en_excel_ast_r1_c1_offset_left_bracket,
    en_excel_ast_r1_c1_offset_right_bracket,
    en_excel_ast_r1_c1_offset_minus,
    en_excel_ast_r1_c1_offset_plus,
    en_excel_ast_r1_c1_offset_integer,

    // primitive
    // --------------------
    en_excel_ast_integer,
    en_excel_ast_float,
    en_excel_ast_bool,
    en_excel_ast_string,
    en_excel_ast_id,
    en_excel_ast_error,

    // if-then-else
    // --------------------
    en_excel_if_then_else_statement,
    en_excel_if_statement,
    en_excel_else_if_statement,
    en_excel_else_statement,

    //
    en_excel_ast_size
  };

  enum en_excel_ast_operation_type{
  
    en_excel_op_type_unknown = 0,

    en_excel_op_type_minus,
    en_excel_op_type_plus,
    en_excel_op_type_multiply,
    en_excel_op_type_divide,
    en_excel_op_type_logic_less,
    en_excel_op_type_logic_less_or_equal,
    en_excel_op_type_logic_greater,
    en_excel_op_type_logic_greater_or_equal,
    en_excel_op_type_logic_equal,
    en_excel_op_type_logic_not_equal,
    en_excel_op_type_logic_not_equal_2,

    en_excel_op_type_size
  };


};


} // namespace parser


#endif // EXCEL_AST_H__
