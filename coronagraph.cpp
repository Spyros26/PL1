//Helpful code: https://www.geeksforgeeks.org/detect-cycle-undirected-graph/
//              https://www.geeksforgeeks.org/connected-components-in-an-undirected-graph/


#include<iostream> 
#include <list> 
#include <limits.h> 
#include <bits/stdc++.h>
#include <fstream>
using namespace std; 
  
// Class for an undirected graph 
class Graph 
{ 
    int V;    // No. of vertices 
    list<int> *adj;    // Pointer to an array containing adjacency lists 
     // A function used by DFS 
    void DFSUtil(int v, bool visited[]);
    int TREEUtil(int v, bool visited[], bool cyclicpart[]);
    bool isCyclicUtil(int v, bool visited[],bool cyclicpart[], int parent, int start); 
public: 
    Graph(int V);   // Constructor 
    void addEdge(int v, int w);   // to add an edge to graph 
    bool isCorona();   // returns true if there is a cycle 
    bool cyclicpart[];
}; 
  
Graph::Graph(int V) 
{ 
    this->V = V; 
    adj = new list<int>[V]; 
} 
  
void Graph::addEdge(int v, int w) 
{ 
    adj[v-1].push_back(w-1); // Add w to v’s list. 
    adj[w-1].push_back(v-1); // Add v to w’s list. 
} 
   

int Graph::TREEUtil(int v, bool visited[], bool cyclicpart[])
{ 
    int r = 1;
    // Mark the current node as visited and print it 
    visited[v] = true; 
    
    // Recur for all the vertices 
    // adjacent to this vertex except the cyclic ones
    list<int>::iterator i; 
    for(i = adj[v].begin(); i != adj[v].end(); ++i) 
        if(!cyclicpart[*i])
		{
		if(!visited[*i]) 
           r = r + TREEUtil(*i, visited, cyclicpart); 
}
return r;
}

void Graph::DFSUtil(int v, bool visited[])
{ 
    // Mark the current node as visited and print it 
    visited[v] = true; 
   // cout << v << " "; 
  
    // Recur for all the vertices 
    // adjacent to this vertex 
    list<int>::iterator i; 
    for(i = adj[v].begin(); i != adj[v].end(); ++i) 
        if(!visited[*i]) 
            DFSUtil(*i, visited); 
}


// A recursive function that uses visited[] and parent to detect 
// cycle in subgraph reachable from vertex v. 
bool Graph::isCyclicUtil(int v, bool visited[],bool cyclicpart[], int parent,int start) 
{ 
    // Mark the current node as visited 
    visited[v] = true; 
    
    
  
    // Recur for all the vertices adjacent to this vertex 
    list<int>::iterator i; 
    for (i = adj[v].begin(); i != adj[v].end(); ++i) 
    { 
        // If an adjacent is not visited, then recur for that adjacent 
        if (!visited[*i]) 
        { 
           if (isCyclicUtil(*i, visited,cyclicpart, v ,start)) 
              {
			  if (cyclicpart[start]==false)
			  {
			  	cyclicpart[v]=true;
			  	return true;
			  }
			  else {
			  return true; 
}
	}
		} 
  
        // If an adjacent is visited and not parent of current vertex, 
        // then there is a cycle. 
        else if (*i != parent) 
           {
           	start = *i;
		   cyclicpart[v] = true;
		   return true; 
}
	} 
    return false; 
} 
  
// Returns true if the graph contains a cycle, else false. 
bool Graph::isCorona() 
{   
   int ncon = 0;
   // Mark all the vertices as not visited 
    bool *visited = new bool[V]; 
    for(int v = 0; v < V; v++) 
        {
		visited[v] = false;  
    }
    
        bool *cyclicpart = new bool[V]; 
    for(int v = 0; v < V; v++) 
        {
		cyclicpart[v] = false;
}

	DFSUtil(0,visited);
	for(int v = 0; v < V; v++) 
       {
		 if (!visited[v]) 
          ncon = ncon + 1;
	}
	if (ncon==0) 
	{
	
	// Mark all the vertices as not visited and not part of recursion 
    // stack 
    //bool *visited = new bool[V]; 
    for (int i = 0; i < V; i++) 
        visited[i] = false; 
  
    // Call the recursive helper function to detect cycle in different 
    // DFS trees 
          if (isCyclicUtil(0, visited,cyclicpart, -1, -2)) 
             {
             	cout<< "CORONA ";
             	for (int p = 0; p<V; p++)
	{
		if (cyclicpart[p]==true)
		{
		ncon = ncon+1;
}
	}
	cout << ncon <<"\n";

for (int i = 0; i < V; i++) 
        {
		visited[i] = false;
}

	int trees[ncon];
	int k = 0;
    for (int ll=0; ll<V; ll++)
	{
		if(cyclicpart[ll]==true)
		{
			trees[k] = TREEUtil(ll, visited, cyclicpart);
			k = k+1;	
		}
		
				 }
		
		int nn = sizeof(trees) / sizeof(trees[0]);
		sort(trees, trees+nn);
		cout << trees[0];
		for (int i = 1; i < ncon; ++i) 
        cout << " " << trees[i];
        cout << "\n";
			 return true; 
}
else{
    cout << "NO CORONA\n";
    return false; 
}
}
else {
cout << "NO CORONA\n";
return false;
}
} 
  
  // MAIN
int main(int argc, char *argv[]) 
{ 
ifstream myfile (argv[1]);
if (myfile.is_open())
{

int T;
myfile >> T;
if ((T>0 && T<11))
{for (int j=0; j<T; j++)
{

int N; int M; int i; int a; int b;
myfile >> N >> M;
    Graph g1(N);
	for (i=0; i<M; i++) 
	{
		myfile >> a >> b;
	g1.addEdge(a,b); 
}
if (M!=N)
{
	cout << "NO CORONA\n";
}
else{
	g1.isCorona();
}
    
  
     
}
}
myfile.close();
}
return 0;
}
