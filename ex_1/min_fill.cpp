#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>

using namespace std;

struct disjoint_set {
	int *parent, *rank;
 
	disjoint_set(int n) {
    	parent = new int[n];
        rank = new int[n];
 
        for (int i = 0; i < n; i++) {
        	parent[i] = i;
            rank[i] = 1;
        }
    }

    int find(int v) {
    	if (parent[v] != v)
        	parent[v] = find(parent[v]);
        return parent[v];
    }

    void merge(int v, int u) {
    	v = find(v);
        u = find(u);
 
        if (v != u) {
        	if (rank[v] > rank[u]) {
				parent[u] = v;
                rank[v] += rank[u];
            	
            }
            else {
            	parent[v] = u;
                rank[u] += rank[v];
            }
        }
    }
};
 
struct graph {
	int N, M;
	vector<vector<int> > edges;
 
	graph(int N, int M) { 
		this->N = N;
		this->M = M; 
	}
 
    void add_edge(int u, int v, int w) {
    	edges.push_back({ w, u, v });
    }
 
    int max_L() {
    	sort(edges.begin(), edges.end());
 
		int L = -1;
		disjoint_set *ds = new disjoint_set(N);
        for (auto edge : edges) {
        	int w = edge[0];
            int u = edge[1];
            int v = edge[2];
 
            if (ds->find(u) != ds->find(v)) {
            	ds->merge(u, v);
                if (w > L)
		    		L = w;
            }
        }
		delete(ds);
		return L;
    }
};

int main(int argc, char** argv) {
	int N, M, u, v, w;
	ifstream input(argv[1]);
	if (!input.is_open())
		return 0;
	
	input >> N >> M;
	graph *map = new graph(N, M);

	while (input >> u >> v >> w) {
  		map->add_edge(u, v, w);
	}
	input.close();
 
	cout << map->max_L() << endl;
	delete(map);

	return 0;
}