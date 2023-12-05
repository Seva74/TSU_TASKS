#include <iostream>
#include <fstream>

using namespace std;

void createAdjacencyMatrix()
{
    ifstream in("input.txt");
    ofstream out("adjacency_matrix.txt");
    int n, m,a,b;
    in >> n >> m;
    out<<n<<" "<<m<<endl;
    int adjacencyMatrix[n][n];
    for(int i=0;i<n;i++)
    {
        for(int j=0;j<n;j++)
        {
            adjacencyMatrix[i][j]=0;
        }
    }
    while(in>>a>>b)
    {
        adjacencyMatrix[a][b]=1;
        adjacencyMatrix[b][a]=1;
    }
    for(int i=0;i<n;i++)
    {
        for(int j=0;j<n;j++)
        {

            out<<adjacencyMatrix[i][j]<<" ";
        }
        out<<endl;
    }
    in.close();
    out.close();
}

void createAdjacentVerticesArray()
{
    ifstream in("adjacency_matrix.txt");
    ofstream out("adjacent_vertices.txt");
    int n, m, k[n];
    in >> n >> m;

    int adjacencyMatrix[n][n];

    for (int i = 0; i < n; ++i) {
        k[i]=0;
        for (int j = 0; j < n; ++j) {
            in >> adjacencyMatrix[i][j];
            if (adjacencyMatrix[i][j] == 1)k[i]++;
        }
    }

    out << n << " " << endl;

    for (int i = 0; i < n; ++i) {
            out<<k[i]<<" ";
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
    ifstream in("adjacent_vertices.txt");
    ofstream out("edge_sequence.txt");
    int n, m, k, a;
    in >> n ;

    int adjacentVertices[n][n];

    for (int i = 0; i < n; ++i) {
        in>>k;
        for (int j = 0; j < k; j++) {
            in>>a;
            out<<i<<" "<<a<<endl;
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
