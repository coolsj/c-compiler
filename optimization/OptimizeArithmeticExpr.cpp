#include "Optimize.h"
#include <cmath>

extern bool optimize;

namespace optimize_ns
{
	bool isPowerOf2(int n)
	{
		return (n && !(n&(n-1)));
	}

	int nextPowerOf2(int n)
	{
	  	int count = 0;
		if (n && !(n&(n-1)))
			return n;
		while( n != 0)
		{
			n  >>= 1;
			count += 1;
		}
		return 1 << count;
	}

	int previousPowerOf2(int n)
	{
		int res = 0;
		for (int i=n; i>=1; i--)
		{
			// If i is a power of 2
			if ((i & (i-1)) == 0)
			{
				res = i;
	            break;
		    }
		}
		return res;
	}
}

using namespace optimize_ns;

ASTExpression* OptimizeArithmeticExpr(ASTExpression *p_left, ASTExpression *p_right, binary_op_t op)
{
	if( optimize )
	{
		ASTConstantExpression *p_left_constant = nullptr, *p_right_constant = nullptr;
		bool is_left_constant = false, is_right_constant = false;
		
		p_left_constant = dynamic_cast<ASTConstantExpression*>(p_left);
		if(p_left_constant) is_left_constant = true;
		p_right_constant = dynamic_cast<ASTConstantExpression*>(p_right);
		if(p_right_constant) is_right_constant = true;

		if(is_left_constant || is_right_constant)
		{
			if(is_left_constant && !is_right_constant)
			{
				if( p_left_constant->GetType() == value_type_t::int_type )
				{
					int left_val = stoi( p_left_constant->GetValue() );
					switch(op)
					{
						case binary_op_t::add_op:
						case binary_op_t::subtract_op:
							{
								if(left_val == 0)
								{
									delete p_left_constant;
									return p_right;
								}
							}
						case binary_op_t::multiply_op:
							{
								if(left_val == 0)
								{
									delete p_right;
									return p_left;
								}
								else if(left_val == 1)
								{
									delete p_left_constant;
									return p_right;
								}
								else if( isPowerOf2(left_val) )
								{
									int new_val = log2(left_val);
									p_left_constant->SetValue( to_string(new_val) );
									return new ASTBinaryOperationExpression(binary_op_t::left_shift_op, p_right, p_left);
								}
								else
								{
									int x = nextPowerOf2(left_val);
									int y = previousPowerOf2(left_val);
									int n = left_val;
									if( (x-n) <= (n-y) )
									{
										ASTConstantExpression *p_powerof2_const = new ASTConstantExpression(value_type_t::int_type, to_string(x));
										ASTExpression *p_op1 = OptimizeArithmeticExpr(p_powerof2_const, p_right->Clone(), binary_op_t::multiply_op);
										p_left_constant->SetValue( to_string(x-n) );
										ASTExpression *p_op2 = OptimizeArithmeticExpr(p_left, p_right, binary_op_t::multiply_op);
										return new ASTBinaryOperationExpression(binary_op_t::subtract_op, p_op1, p_op2);
									}
									else
									{
										ASTConstantExpression *p_powerof2_const = new ASTConstantExpression(value_type_t::int_type, to_string(y));
										ASTExpression *p_op1 = OptimizeArithmeticExpr(p_powerof2_const, p_right->Clone(), binary_op_t::multiply_op);
										p_left_constant->SetValue( to_string(n-y) );
										ASTExpression *p_op2 = OptimizeArithmeticExpr(p_left, p_right, binary_op_t::multiply_op);
										return new ASTBinaryOperationExpression(binary_op_t::add_op, p_op1, p_op2);
									}
								}
							}
					}
				}
			}
			else if(!is_left_constant && is_right_constant)
			{
				if( p_right_constant->GetType() == value_type_t::int_type )
				{
					int right_val = stoi( p_right_constant->GetValue() );
					switch(op)
					{
						case binary_op_t::add_op:
						case binary_op_t::subtract_op:
							{
								if(right_val == 0)
								{
									delete p_right_constant;
									return p_left;
								}
							}
						case binary_op_t::multiply_op:
							{
								if(right_val == 0)
								{
									delete p_left;
									return p_right;
								}
								else if(right_val == 1)
								{
									delete p_right_constant;
									return p_left;
								}
								else if( isPowerOf2(right_val) )
								{
									int new_val = log2(right_val);
									p_right_constant->SetValue( to_string(new_val) );
									return new ASTBinaryOperationExpression(binary_op_t::left_shift_op, p_left, p_right);
								}
								else
								{
									int x = nextPowerOf2(right_val);
									int y = previousPowerOf2(right_val);
									int n = right_val;
									if( (x-n) <= (n-y) )
									{
										ASTConstantExpression *p_powerof2_const = new ASTConstantExpression(value_type_t::int_type, to_string(x));
										ASTExpression *p_op1 = OptimizeArithmeticExpr(p_powerof2_const, p_left->Clone(), binary_op_t::multiply_op);
										p_right_constant->SetValue( to_string(x-n) );
										ASTExpression *p_op2 = OptimizeArithmeticExpr(p_left, p_right, binary_op_t::multiply_op);
										return new ASTBinaryOperationExpression(binary_op_t::subtract_op, p_op1, p_op2);
									}
									else
									{
										ASTConstantExpression *p_powerof2_const = new ASTConstantExpression(value_type_t::int_type, to_string(y));
										ASTExpression *p_op1 = OptimizeArithmeticExpr(p_powerof2_const, p_left->Clone(), binary_op_t::multiply_op);
										p_right_constant->SetValue( to_string(n-y) );
										ASTExpression *p_op2 = OptimizeArithmeticExpr(p_left, p_right, binary_op_t::multiply_op);
										return new ASTBinaryOperationExpression(binary_op_t::add_op, p_op1, p_op2);
									}
								}
							}
						case binary_op_t::divide_op:
							{
								if(right_val == 1)
								{
									delete p_right_constant;
									return p_left;
								}
								else if( isPowerOf2(right_val) )
								{
									int new_val = log2(right_val);
									p_right_constant->SetValue( to_string(new_val) );
									return new ASTBinaryOperationExpression(binary_op_t::right_shift_op, p_left, p_right);
								}
							}
					}
				}
			}
			else
			{
				if( p_left_constant->GetType() == value_type_t::int_type
					&& p_right_constant->GetType() == value_type_t::int_type )
				{
					int left_val = stoi( p_left_constant->GetValue() );
					int right_val = stoi( p_right_constant->GetValue() );
					int new_val;
					switch(op)
					{
						case binary_op_t::add_op:			new_val = left_val + right_val; break;
						case binary_op_t::subtract_op:		new_val = left_val - right_val; break;
						case binary_op_t::multiply_op:		new_val = left_val * right_val; break;
						case binary_op_t::divide_op:		new_val = left_val / right_val; break;
						case binary_op_t::modulus_op:		new_val = left_val % right_val; break;
						case binary_op_t::right_shift_op:	new_val = left_val >> right_val; break;
						case binary_op_t::left_shift_op:	new_val = left_val << right_val; break;
					}
					p_left_constant->SetValue( to_string(new_val) );
					delete p_right_constant;
					return p_left_constant;
				}
			}
		}
	}
	return new ASTBinaryOperationExpression(op, p_left, p_right);
}
