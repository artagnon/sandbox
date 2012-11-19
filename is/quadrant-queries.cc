#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

struct Node {
	int q1, q2, q3, q4;
	int lbound, rbound;
	Node *left, *right;
};

struct Point {
	bool x, y;
};

struct Query {
	char type;
	int lbound, rbound;
};

Node *build_segment_tree(int lbound, int rbound, vector<Point *> &points) {
	Node *this_node = new Node;
	this_node->lbound = lbound;
	this_node->rbound = rbound;
	for (int i = lbound - 1; i < rbound; i++) {
		if (points[i]->x and points[i]->y)
			this_node->q1++;
		else if (not points[i]->x and points[i]->y)
			this_node->q2++;
		else if (not points[i]->x and not points[i]->y)
			this_node->q3++;
		else
			this_node->q4++;
	}
	if (lbound != rbound) {
		int mid = (rbound + lbound) / 2;
		this_node->left = build_segment_tree(lbound, mid, points);
		this_node->right = build_segment_tree(mid + 1, rbound, points);
	}
	return this_node;
}

void reflect_x(Node *root) {
	int temp;
	temp = root->q1;
	root->q1 = root->q4;
	root->q4 = temp;
	temp = root->q2;
	root->q2 = root->q3;
	root->q3 = temp;
	
	if (root->left) {
		reflect_x(root->left);
		reflect_x(root->right);
	}
}

void reflect_y(Node *root) {
	int temp;
	temp = root->q1;
	root->q1 = root->q2;
	root->q2 = temp;
	temp = root->q4;
	root->q4 = root->q3;
	root->q3 = temp;
	
	if (root->left) {
		reflect_y(root->left);
		reflect_y(root->right);
	}
}

vector<int> return_q(Node *root) {
	vector<int> to_return;
	to_return.push_back(root->q1);
	to_return.push_back(root->q2);
	to_return.push_back(root->q3);
	to_return.push_back(root->q4);
	return to_return;
}

template <typename Function>
vector<int> segment_exec(int lbound, int rbound, Node *root, Function f) {
	if (lbound == root->lbound and rbound == root->rbound)
		return f(root);
	else if (lbound >= root->right->lbound)
		return segment_exec(lbound, rbound, root->right, f);
	else if (rbound <= root->left->rbound)
		return segment_exec(lbound, rbound, root->left, f);
	else {
		vector<int> vec1, vec2;
		vec1 = segment_exec(lbound, root->left->rbound, root->left, f);
		vec2 = segment_exec(root->right->lbound, rbound, root->right, f);
		vec1[0] += vec2[0];
		vec1[1] += vec2[1];
		vec1[2] += vec2[2];
		vec1[3] += vec2[3];
		return vec1;
	}
}

template <typename Function>
void segment_exec_void(int lbound, int rbound, Node *root, Function f) {
	if (lbound == root->lbound and rbound == root->rbound)
		f(root);
	else if (lbound >= root->right->lbound)
		segment_exec_void(lbound, rbound, root->right, f);
	else if (rbound <= root->left->rbound)
		segment_exec_void(lbound, rbound, root->left, f);
	else {
		segment_exec_void(lbound, root->left->rbound, root->left, f);
		segment_exec_void(root->right->lbound, rbound, root->right, f);
	}
}

void execute_query(Query *this_query, Node *root) {
	vector<int> result;
	switch (this_query->type) {
	case 'X':
		segment_exec_void(this_query->lbound,
				  this_query->rbound, root, reflect_x);
		break;
	case 'Y':
		segment_exec_void(this_query->lbound,
				  this_query->rbound, root, reflect_y);
		break;
	case 'C':
		result = segment_exec(this_query->lbound,
				      this_query->rbound, root, return_q);
		cout << result[0] << " " << result[1] << " " <<
			result[2] << " " << result[3] << endl;
		break;
	}
}

int main() {
	int n_points, n_queries;
	vector<Query *> queries;
	vector<Point *> points;
	cin >> n_points;
	for (int i = 0; i < n_points; i++) {
		int this_x, this_y;
		Point *this_point = new Point;
		cin >> this_x >> this_y;
		this_point->x = this_x > 0 ? true : false;
		this_point->y = this_y > 0 ? true : false;
		points.push_back(this_point);
	}
	cin >> n_queries;
	for (int i = 0; i < n_queries; i++) {
		Query *this_query = new Query;
		cin >> this_query->type >>
			this_query->lbound >> this_query->rbound;
		queries.push_back(this_query);
	}
	Node *root = build_segment_tree(1, n_points, points);
	for (vector<Query *>::iterator i = queries.begin();
	     i != queries.end(); i++)
		execute_query(*i, root);
	return 0;
}
