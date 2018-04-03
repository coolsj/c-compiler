void print_pascal_triangle(int limit)
{
	int i=0;
	while(i < 10)
	{
		int j=0, num = 1;
		int max = (2*i)+1;
		while(j < max )
		{
			printf("%d ", num);
			if(j < max/2 )
				num += 1;
			else
				num -= 1;
			j += 1;
		}
		puts("");
		if(i > limit)
		{
			break;
		}
		i += 1;
	}
}

int main()
{
	print_pascal_triangle(5);
	return 0;
}

