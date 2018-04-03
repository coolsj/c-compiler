//#include <stdio.h>

int fact_recurse(int num)
{
	int result;
	if(num == 1)
		result = 1;
	else
		result = num * fact_recurse(num-1);
	return result;
}

int fact_iterate(int num)
{
	int result = 1;
	while(num > 0)
	{
		result = result * num;
		num = num - 1;
	}
	return result;
}

int main()
{
	int a=5;
	printf("Factorial of %d is %d ", a, fact_recurse(a));
	puts("");
	int b = a+1;
	printf("Factorial of %d is %d ", b, fact_iterate(b));
	puts("");
	return 0;
}
