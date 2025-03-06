#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

struct Item {
    int weight;
    int cost;
};

bool compareItems(const Item& a, const Item& b) {
    return (a.cost > b.cost);
}

int greedyKnapsack(vector<Item> items, int capacity) {
    sort(items.begin(), items.end(), compareItems);
    int totalCost = 0;
    for (const auto& item : items) {
        if (capacity >= item.weight) {
            totalCost += item.cost;
            capacity -= item.weight;
        }
    }
    return totalCost;
}

int recursiveKnapsack(vector<Item> items, int capacity, int index) {
    if (index < 0 || capacity <= 0) {
        return 0;
    }
    if (items[index].weight > capacity) {
        return recursiveKnapsack(items, capacity, index - 1);
    }
    return max(recursiveKnapsack(items, capacity, index - 1),
        recursiveKnapsack(items, capacity - items[index].weight, index - 1) + items[index].cost);
}

int dynamicKnapsack(vector<Item> items, int capacity) {
    int n = items.size();
    vector<vector<int>> A(n + 1, vector<int>(capacity + 1, 0));

    for (int i = 1; i <= n; i++) {
        for (int w = 1; w <= capacity; w++) {
            if (items[i - 1].weight <= w) {
                A[i][w] = max(A[i - 1][w], A[i - 1][w - items[i - 1].weight] + items[i - 1].cost);
            }
            else {
                A[i][w] = A[i - 1][w];
            }
        }
    }

    return A[n][capacity];
}

int main() {
    int n = 7; // количество предметов
    vector<Item> items = { {2, 3}, {3, 4}, {4, 8}, {5, 8}, {9, 10}, {5, 5}, {6, 7} }; // веса и стоимости предметов. {4, 8}, {5, 8}, {6, 7}
    int capacity = 15; // грузоподъемность рюкзака


    // Жадный алгоритм
    int greedyResult = greedyKnapsack(items, capacity);
    cout << "Greedy algorithm: " << greedyResult << endl;

    // Рекурсивный алгоритм с отсечением
    int recursiveResult = recursiveKnapsack(items, capacity, n - 1);
    cout << "Recursive algorithm: " << recursiveResult << endl;

    // Алгоритм динамического программирования
    int dynamicResult = dynamicKnapsack(items, capacity);
    cout << "Dynamic programming algorithm: " << dynamicResult << endl;

    return 0;
}
