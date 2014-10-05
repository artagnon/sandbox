#include <stdio.h>
#include <limits.h>

#define MAX 30

static int stack[MAX];
static int stack_top;
static int minimum = INT_MAX;

int find_minimum()
{
	int min = INT_MAX;
	for (int i = 0; i < stack_top; i++)
		if (stack[i] < min)
			min = stack[i];
	return min;
}

int pop()
{
	int ret = stack[--stack_top];
	if (ret == minimum)
		minimum = find_minimum();
	return ret;
}

void push(int v)
{
	if (v < minimum)
		minimum = v;
	stack[stack_top++] = v;
}

int getMinimum()
{
	return minimum;
}

int main()
{
	push(3);
	push(4);
	push(5);
	push(2);
	printf("%d\n", getMinimum());
	pop();
	printf("%d\n", getMinimum());
	return 0;
}
