#include <iostream>
using namespace std;

void solve(int K, int row, int col[], int &count) {
    if (row == K) {
        count++;
        return;
    }


    for (int i = 0; i < K; i++) {
        bool can = true;
        for (int j = 0; j < row; j++) {
            if (col[j] == i || col[j] == i - row + j || col[j] == i + row - j) {
                can = false;
                break;
            }
        }
        if (can) {
            col[row] = i;
            solve(K, row+1, col, count);
            col[row] = -1;
        }
    }
}

int main() {
    int K;
    cout << "Enter the number of queens (K): ";
    cin >> K;
    int col[K];
    for (int i = 0; i < K; i++) {
        col[i] = -1;
    }
    int count = 0;
    solve(K, 0, col, count);
    cout << "Total number of solutions: " << count << endl;
    return 0;
}
