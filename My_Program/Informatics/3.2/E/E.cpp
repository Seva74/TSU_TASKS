#include <iostream>

using namespace std;
int a, b, x;
int main()
{
	cin >> x;
	while (x >= 1) {
		b = x % 10;
		a = a + b;
		x /= 10;
	}
	cout << a;
}
