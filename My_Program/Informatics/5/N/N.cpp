#include <iostream>

using namespace std;
int n, k, m[10000], a;
int main()
{
	cin >> n;
	for (int i = 0; i < n; i++) {
		cin >> m[i];
	}
	cin >> k;
	if (k > 0) {
		for (int g = 0; g < k; g++) {
			a = m[n - 1];
			for (int i = n - 1; i > 0; i--) {
				m[i] = m[i - 1];
			}
			m[0] = a;
		}
	}

	else if (k <= 0) {
		k = -k;
		for (int g = 0; g < k; g++) {
			a = m[n - 1];
			for (int i = n - 2; i >= 0; i--){
				m[i + 1] = m[i];
			}
			m[0] = a;
		}
	}

	for (int i = 0; i < n; i++) {
		cout << m[i] << " ";
	}
}