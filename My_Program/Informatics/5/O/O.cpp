#include <iostream>

using namespace std;
int n, m[10000], a, b;
int main()
{
	int c = 1;
	cin >> n;
	for (int i = 0; i < n; i++) {
		cin >> m[i];
	}
	for (int i = 0; i <= n-2; i++) {
		if ((m[i]) == (m[i + 1])) {
			a += 2;
			for (int g = 1; b == 0; g++) {
				if ((m[i + 1]) != m[i + 2]) {
					a -= 2;
					b = 1;
				}
				else if ((m[i + g]) == m[i + g + 1]) {
					a++;
					c++;
				}
				else b = 1;
			}
			b = 0;
			i += c;
			c = 1;
		}
	}
	cout << a;
}
	