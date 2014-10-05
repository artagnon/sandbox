#include <stdio.h>

#define MAX 10
#define max(a, b) ((a > b) ? a : b)

static int pots[MAX] = { 2, 3, 4, 9, 11, 33, 42, 92, 28, 18 };

int selectmax(int start, int end)
{
	int a, b, opt;

	if (start > end)
		return 0;
	opt = selectmax(start + 1, end - 1);
	a = pots[start] + max(selectmax(start + 2, end), opt);
	b = pots[end] + max(selectmax(start, end - 2), opt);
	return max(a, b);
}

int main()
{
	int sum = 0;

	printf("%d\n", selectmax(0, MAX - 1));
	for(int i = 0; i < MAX; i++)
		sum += pots[i];
	printf("%d\n", sum);
}
