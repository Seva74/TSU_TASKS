#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

// The function of the Gaussian-Jordan step-by-step matrix reduction
void gauss_jordan(int M, int N, vector<vector<double>>& A) {
    for (int k = 0; k < M; k++) {
        int i_max = k;
        double max = A[k][k];

        // Find a maximal element in k-column
        for (int i = k + 1; i < M; i++) {
            if (abs(A[i][k]) > abs(max)) {
                i_max = i;
                max = A[i][k];
            }
        }

        // Swap k-string with i_max-line
        for (int j = k; j <= N; j++) {
            double temp = A[k][j];
            A[k][j] = A[i_max][j];
            A[i_max][j] = temp;
        }

        // Check if the current row is a zero row
        bool zeroRow = true;
        for (int j = k; j <= N; j++) {
            if (A[k][j] != 0) {
                zeroRow = false;
                break;
            }
        }

        // Skip the current row if it is a zero row
        if (zeroRow)
            continue;

        // Top triangular view
        for (int i = k + 1; i < M; i++) {
            double c = -A[i][k] / A[k][k];
            for (int j = k; j <= N; j++) {
                if (k == j) A[i][j] = 0;
                else A[i][j] += c * A[k][j];
            }
        }
    }

    // Diagonal view
    for (int k = M - 1; k >= 0; k--) {
        // Check if the current row is a zero row
        bool zeroRow = true;
        for (int j = 0; j <= N; j++) {
            if (A[k][j] != 0) {
                zeroRow = false;
                break;
            }
        }

        // Skip the current row if it is a zero row
        if (zeroRow)
            continue;

        for (int i = k - 1; i >= 0; i--) {
            double c = -A[i][k] / A[k][k];
            for (int j = k; j <= N; j++) {
                if (k == j) A[i][j] = 0;
                else A[i][j] += c * A[k][j];
            }
        }
    }

    // Simplification
    for (int i = 0; i < M; i++) {
        if (A[i][i] != 0) {
            A[i][N] /= A[i][i];
            A[i][i] = 1;
        }
    }
}

int main() {
    ifstream fin("input.txt");
    int M, N;
    fin >> M >> N;

    vector<vector<double>> A(M, vector<double>(N + 1));
    for (int i = 0; i < M; i++) {
        for (int j = 0; j <= N; j++) {
            fin >> A[i][j];
        }
    }

    // Induce matrix reduction function to step-like
    gauss_jordan(M, N, A);
    ofstream fout("output.txt");

    // Check if the system has a solution
    bool SolutionExists = true;
    for (int i = 0; i < M; i++) {
        bool ZeroRow = true;
        for (int j = 0; j < N; j++) {
            if (A[i][j] != 0) {
                ZeroRow = false;
                break;
            }
        }
        if (ZeroRow && A[i][N] != 0) {
            SolutionExists = false;
            break;
        }
    }

    if (!SolutionExists) {
        fout << "No solution exists" << endl;
        return 0;
    } else {
        // Check if the system has a unique solution
        bool Unique = true;
        for (int i = 0; i < M; i++) {
            bool ZeroRow = true;
            for (int j = 0; j < N; j++) {
                if (A[i][j] != 0) {
                    ZeroRow = false;
                    break;
                }
            }
            if (ZeroRow && A[i][N] != 0) {
                Unique = false;
                break;
            }
        }

        if (Unique) {
            for (int i = 0; i < M; i++) {
                fout << "x" << i + 1 << " = " << A[i][N] << endl;
            }
        }
    }
    cout << "Successfully compiled" << endl;
    return 0;
}
