#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

struct Point {
    double x;
    double y;
};

struct Edge {
    int u, v;
    double weight;

    bool operator<(const Edge& other) const {
        return weight < other.weight;
    }
};

vector<Point> generateRandomPoints(int N) {
    vector<Point> points;
    for (int i = 0; i < N; ++i) {
        points.push_back({static_cast<double>(rand() % 100), static_cast<double>(rand() % 100)});
    }
    return points;
}

double calculateDistance(const Point& a, const Point& b) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

vector<Edge> generateEdges(const vector<Point>& points, int N) {
    vector<Edge> edges;
    for (int i = 0; i < N; ++i) {
        for (int j = i + 1; j < N; ++j) {
            edges.push_back({i, j, calculateDistance(points[i], points[j])});
        }
    }
    return edges;
}

class DisjointSet {
public:
    DisjointSet(int n) : parent(n), rank(n, 0) {
        for (int i = 0; i < n; ++i) {
            parent[i] = i;
        }
    }

    int find(int u) {
        if (parent[u] != u) {
            parent[u] = find(parent[u]);
        }
        return parent[u];
    }

    void unite(int u, int v) {
        int root_u = find(u);
        int root_v = find(v);

        if (root_u != root_v) {
            if (rank[root_u] > rank[root_v]) {
                parent[root_v] = root_u;
            } else if (rank[root_u] < rank[root_v]) {
                parent[root_u] = root_v;
            } else {
                parent[root_v] = root_u;
                rank[root_u]++;
            }
        }
    }

private:
    vector<int> parent;
    vector<int> rank;
};

vector<Edge> kruskalMST(int N, vector<Edge>& edges) {
    sort(edges.begin(), edges.end());
    DisjointSet ds(N);
    vector<Edge> mst;

    for (const auto& edge : edges) {
        if (ds.find(edge.u) != ds.find(edge.v)) {
            ds.unite(edge.u, edge.v);
            mst.push_back(edge);
        }
    }
    return mst;
}

void dfs(int u, const vector<vector<int>>& adj, vector<bool>& visited, vector<int>& component) {
    visited[u] = true;
    component.push_back(u);
    for (int v : adj[u]) {
        if (!visited[v]) {
            dfs(v, adj, visited, component);
        }
    }
}

vector<vector<int>> findConnectedComponents(int N, const vector<Edge>& edges, int numEdges) {
    vector<vector<int>> adj(N);
    for (int i = 0; i < numEdges; ++i) {
        adj[edges[i].u].push_back(edges[i].v);
        adj[edges[i].v].push_back(edges[i].u);
    }

    vector<bool> visited(N, false);
    vector<vector<int>> components;

    for (int i = 0; i < N; ++i) {
        if (!visited[i]) {
            vector<int> component;
            dfs(i, adj, visited, component);
            components.push_back(component);
        }
    }

    return components;
}

void calculateAndPrintClusterStats(const vector<Point>& points, const vector<vector<int>>& components) {
    int k=0;
    for (const auto& component : components) {
        double minX = 1.7E+308, maxX = 1.7E-308, sumX = 0;
        double minY = 1.7E+308, maxY = 1.7E-308, sumY = 0;

        for (int idx : component) {
            const Point& p = points[idx];
            if (p.x < minX) minX = p.x;
            if (p.x > maxX) maxX = p.x;
            if (p.y < minY) minY = p.y;
            if (p.y > maxY) maxY = p.y;
            sumX += p.x;
            sumY += p.y;
        }

        double centroidX = sumX / component.size();
        double centroidY = sumY / component.size();
        k++;

        cout << k << " Cluster:\n";
        cout << "Number of points: " << component.size() << "\n";
        cout << "Min X: " << minX << ", Max X: " << maxX << "\n";
        cout << "Min Y: " << minY << ", Max Y: " << maxY << "\n";
        cout << "Centroid: (" << centroidX << ", " << centroidY << ")\n\n";
    }
}

int main() {
    srand(static_cast<unsigned int>(time(0)));

    int N = 1000; // Number of points
    int K = 4;   // Number of clusters

    // Step 1: Generate random points
    vector<Point> points = generateRandomPoints(N);

    // Step 2: Generate all edges of the complete graph
    vector<Edge> edges = generateEdges(points, N);

    // Step 3: Find the MST using Kruskal's algorithm
    vector<Edge> mst = kruskalMST(N, edges);

    // Step 4: Sort the edges of the MST by weight
    sort(mst.begin(), mst.end());

    // Step 5: Use the first N-K edges to find K connected components
    vector<vector<int>> components = findConnectedComponents(N, mst, N - K);

    // Step 6: Calculate and print cluster statistics
    calculateAndPrintClusterStats(points, components);

    return 0;
}