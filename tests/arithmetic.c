//#include <stdio.h>

int add(int a, int b)
{
	return a+b;
}

int subtract(int a, int b)
{
	return a-b;
}

int multiply(int a, int b)
{
	return a*b;
}

int divide(int a, int b)
{
	return a/b;
}

int main()
{
	printf("Add: %d ", add(10,20));
	puts("");
	printf("Subtract: %d ", subtract(10,20));
	puts("");
	printf("Multiply: %d ", multiply(10,20));
	puts("");
	printf("Divide: %d ", divide(10,20));
	puts("");
	return 0;
}
