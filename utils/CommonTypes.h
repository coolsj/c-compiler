#ifndef COMMONTYPES_H_
#define COMMONTYPES_H_

#include <string>
using namespace std;

enum class node_type_t
{
	program_root_nd,
	file_nd,
	func_decl_stmt_nd,
	var_decl_stmt_nd,
	compound_stmt_nd,
	expr_stmt_nd,
	select_stmt_nd,
	iter_stmt_nd,
	scalar_id_expr_nd,
	array_id_expr_nd,
	assign_op_expr_nd,
	binary_op_expr_nd,
	unary_op_expr_nd,
	func_formal_arg_nd,
	jump_stmt_nd,
	constant_expr_nd,
	function_call_expr_nd
};

enum class binary_op_t
{
	add_op,
	subtract_op,
	multiply_op,
	divide_op,
	modulus_op,
	right_shift_op,
	left_shift_op,
	logical_and_op,
	logical_or_op,
	compare_gt_op,
	compare_ge_op,
	compare_lt_op,
	compare_le_op,
	compare_eq_op,
	compare_ne_op
};
string binary_op_to_string( binary_op_t op );

enum class assign_op_t
{
	assign_op,
	add_assign_op,
	subtract_assign_op,
	multiply_assign_op,
	divide_assign_op
};
string assign_op_to_string( assign_op_t op );

enum class unary_op_t
{
	pre_incr_op,
	pre_decr_op,
	post_incr_op,
	post_decr_op
};
string unary_op_to_string( unary_op_t op );

enum class value_type_t
{
	void_type,
	bool_type,
	char_type,
	short_type,
	int_type,
	long_type,
	signed_type,
	unsigned_type,
	float_type,
	double_type,
	string_type
};
string type_to_string( value_type_t type );

enum class jump_type_t
{
	break_t,
	return_t,
	continue_t
};
string jump_type_to_string( jump_type_t jump_type );

#endif // COMMONTYPES_H_
