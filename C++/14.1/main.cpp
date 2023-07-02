#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

const int MAX_SIZE = 100;

void createAdjacencyMatrix()
{
    ifstream in("input.txt");
    ofstream out("adjacency_matrix.txt");
    int n, m;
    in >> n >> m;

    int adjacencyMatrix[MAX_SIZE][MAX_SIZE];
    memset(adjacencyMatrix, 0, sizeof(adjacencyMatrix));

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
        out << '\n';
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

    int adjacencyMatrix[MAX_SIZE][MAX_SIZE];

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            in >> adjacencyMatrix[i][j];
        }
    }

    in.close();

    out << n << " " << n << '\n';

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (adjacencyMatrix[i][j] == 1) {
                out << j << " ";
            }
        }
        out << '\n';
    }

    out.close();
}

void createEdgeSequence()
{
    ifstream in("adjacent_vertices.txt");
    ofstream out("edge_sequence.txt");
    int n, m;
    in >> n >> m;

    int adjacentVertices[MAX_SIZE][MAX_SIZE];

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            int vertex;
            in >> vertex;
            if (vertex == 1) {
                adjacentVertices[i][j] = 1;
            }
        }
    }

    in.close();

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (adjacentVertices[i][j] == 1) {
                out << i << "-" << j << '\n';
            }
        }
    }

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
