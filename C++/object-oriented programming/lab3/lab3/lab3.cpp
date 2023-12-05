#include <iostream>
#include "Array.h"


using namespace std;

int main()
{
	Array<int> arr(10);
	for (int i = 0; i < 10; i++)
	{
		arr[i] = i + 1;
	}
	arr.Print();
	arr.Scan();
	cout << arr << endl;

	return 0;
}