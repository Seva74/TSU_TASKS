#include <iostream>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <ctime>

using namespace std;

struct Item {
    int weight;
    int cost;
};

bool cmpGreedy(const Item& a, const Item& b) {
    return (double)a.cost / a.weight > (double)b.cost / b.weight;
}

pair<int, int> greedyKnapsack(const vector<Item>& items, int W) {
    vector<Item> sorted = items;
    sort(sorted.begin(), sorted.end(), cmpGreedy);

    int totalCost = 0;
    int totalWeight = 0;
    for (const auto& it : sorted) {
        if (totalWeight + it.weight <= W) {
            totalWeight += it.weight;
            totalCost += it.cost;
        }
    }
    return make_pair(totalCost, totalWeight);
}

pair<int, int> dpKnapsack(const vector<Item>& items, int W) {
    int n = items.size();
    vector<vector<int>> dp(n + 1, vector<int>(W + 1, 0));

    for (int i = 1; i <= n; ++i) {
        for (int w = 0; w <= W; ++w) {
            dp[i][w] = dp[i - 1][w];
            if (items[i - 1].weight <= w) {
                int take = dp[i - 1][w - items[i - 1].weight] + items[i - 1].cost;
                if (take > dp[i][w])
                    dp[i][w] = take;
            }
        }
    }

    int totalWeight = 0;
    int curW = W;
    for (int i = n; i > 0; --i) {
        if (dp[i][curW] != dp[i - 1][curW]) {
            totalWeight += items[i - 1].weight;
            curW -= items[i - 1].weight;
        }
    }

    return make_pair(dp[n][W], totalWeight);
}

int bestCost = 0;
int bestWeight = 0;

void backtrack(int idx, int curWeight, int curCost,
    const vector<Item>& items, int W) {
    if (idx == (int)items.size()) {
        if (curCost > bestCost) {
            bestCost = curCost;
            bestWeight = curWeight;
        }
        return;
    }

    // Берём, если влезает
    if (curWeight + items[idx].weight <= W) {
        backtrack(idx + 1,
            curWeight + items[idx].weight,
            curCost + items[idx].cost,
            items, W);
    }

    // Не берём текущий груз
    backtrack(idx + 1, curWeight, curCost, items, W);
}

pair<int, int> backtrackKnapsack(const vector<Item>& items, int W) {
    bestCost = 0;
    bestWeight = 0;
    backtrack(0, 0, 0, items, W);
    return make_pair(bestCost, bestWeight);
}

int main() {
    setlocale(LC_ALL, "RUS");
    srand((unsigned int)time(NULL));

    const int n = 30;
    vector<Item> items(n);
    int sumWeight = 0;

    cout << "Грузы (вес, стоимость):\n";
    for (int i = 0; i < n; ++i) {
        items[i].weight = rand() % 100 + 1;   // вес от 1 до 100
        items[i].cost = rand() % 200 + 1;   // стоимость от 1 до 200
        sumWeight += items[i].weight;

        cout << "(" << items[i].weight << "," << items[i].cost << ") ";
        if ((i + 1) % 10 == 0) cout << "\n";
    }
    cout << "\n\n";

    int W = sumWeight / 2;
    cout << "Вместимость рюкзака W = " << W << "\n\n";

    pair<int, int> greedyRes = greedyKnapsack(items, W);
    cout << "Жадный алгоритм:               стоимость = " << greedyRes.first
        << ", вес = " << greedyRes.second << "\n";

    pair<int, int> dpRes = dpKnapsack(items, W);
    cout << "Динамическое программирование: стоимость = " << dpRes.first
        << ", вес = " << dpRes.second << "\n";

    pair<int, int> btRes = backtrackKnapsack(items, W);
    cout << "Бэктрекинг:                    стоимость = " << btRes.first
        << ", вес = " << btRes.second << "\n\n";

    return 0;
}