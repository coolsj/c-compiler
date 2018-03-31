#include "CommonTypes.h"

string binary_op_to_string( binary_op_t op )
{
	switch( op )
	{
		case binary_op_t::add_op:					return "+";
		case binary_op_t::subtract_op:				return "-";
		case binary_op_t::multiply_op:				return "*";
		case binary_op_t::divide_op:				return "/";
		case binary_op_t::logical_and_op:			return "&&";
		case binary_op_t::logical_or_op:			return "||";
		case binary_op_t::compare_gt_op:			return ">";
		case binary_op_t::compare_ge_op:			return ">=";
		case binary_op_t::compare_lt_op:			return "<";
		case binary_op_t::compare_le_op:			return "<=";
		case binary_op_t::compare_eq_op:			return "==";
		case binary_op_t::compare_ne_op:			return "!=";
		default:									return "-invalid-bin-op-";
	}
}

string assign_op_to_string( assign_op_t op )
{
	switch( op )
	{
		case assign_op_t::assign_op:				return "=";
		case assign_op_t::add_assign_op:			return "+=";
		case assign_op_t::subtract_assign_op:		return "-=";
		case assign_op_t::multiply_assign_op:		return "*=";
		case assign_op_t::divide_assign_op:			return "/=";
		default:									return "-invalid-assign-op-";
	}
}

string unary_op_to_string( unary_op_t op )
{
	switch( op )
	{
		case unary_op_t::pre_incr_op:
		case unary_op_t::post_incr_op:	return "++";
		case unary_op_t::pre_decr_op:
		case unary_op_t::post_decr_op:	return "--";
		default:						return "-invalid-unary-op-";
	}
}

string type_to_string( value_type_t type )
{
	switch( type )
	{
		case value_type_t::void_type: return "void";
		case value_type_t::bool_type: return "bool";
		case value_type_t::char_type: return "char";
		case value_type_t::short_type: return "short";
		case value_type_t::int_type: return "int";
		case value_type_t::long_type: return "long";
		case value_type_t::signed_type: return "signed";
		case value_type_t::unsigned_type: return "unsigned";
		case value_type_t::float_type: return "float";
		case value_type_t::double_type: return "double";
	}
}

string jump_type_to_string( jump_type_t jump_type )
{
	switch( jump_type )
	{
		case jump_type_t::break_t:		return "break";
		case jump_type_t::return_t:		return "return";
		case jump_type_t::continue_t:	return "continue";
	}
}
