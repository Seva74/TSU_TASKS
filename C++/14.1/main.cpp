#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

void createAdjacencyMatrix()
{
    ifstream in("input.txt");
    ofstream out("adjacency_matrix.txt");
    int n, m;
    in >> n >> m;

    vector<vector<int>> adjacencyMatrix(n, vector<int>(n, 0));

    for (int i = 0; i < m; ++i) {
        int u, v;
        in >> u >> v;
        adjacencyMatrix[u][v] = 1;
        adjacencyMatrix[v][u] = 1;
    }


    out << n << " " << n << endl;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            out << adjacencyMatrix[i][j] << " ";
        }
        out << endl;
    }

    in.close();
    out.close();
}

void createAdjacentVerticesArray()
{
    ifstream in("adjacency_matrix.txt");
    ofstream out("adjacent_vertices.txt");
    int n, m;
    in >> n >> m;

    vector<vector<int>> adjacencyMatrix(n, vector<int>(n, 0));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            in >> adjacencyMatrix[i][j];
        }
    }

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (adjacencyMatrix[i][j] == 1) {
                out << j << " ";
            }
        }
        out << endl;
    }
    in.close();
    out.close();
}

void createEdgeSequence()
{
    ifstream in("adjacency_matrix.txt");
    ofstream out("edge_sequence.txt");
    int n, m;
    in >> n >> m;

    vector<vector<int>> adjacencyMatrix(n, vector<int>(n, 0));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            in >> adjacencyMatrix[i][j];
        }
    }

    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (adjacencyMatrix[i][j] >= 1) {
                out << i << " " << j << endl;
            }
        }
    }

    in.close();
    out.close();
}


int main()
{
    createAdjacencyMatrix();
    createAdjacentVerticesArray();
    createEdgeSequence();

    cout << "Compiled successfully" << endl;

    return 0;
}
