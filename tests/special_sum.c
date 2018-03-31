//#include "stdio.h"

int special_sum(int num)
{
	int ret = 0;
	int i=1;
	while(i <= num)
	{
		if(i < 3)
		{
			i = i+1;
			continue;
		}
		else if(i > 10)
			break;
		ret += i;
		i = i+1;
	}
	return ret;
}

int main()
{
	int a = 100;
	printf("Special sum of %d numbers is: %d", a, special_sum(a) );
	return 0;
}
