#include <algorithm>
#include <iostream>
#include <cstring>
#include <sstream>
#include <vector>

using namespace std;

struct Node {
	int q1, q2, q3, q4;
	int lbound, rbound;
	Node *parent, *left, *right;
};

struct Point {
	bool x, y;
};

struct Query {
	char type;
	int lbound, rbound;
};

struct qresult {
	int q1, q2, q3, q4;
} result;

Node *build_segment_tree(int lbound, int rbound,
			 Node *parent, vector<Point *> &points) {
	Node *this_node = new Node;
	this_node->lbound = lbound;
	this_node->rbound = rbound;
	this_node->parent = parent;
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
		this_node->left = build_segment_tree(lbound, mid,
						     this_node, points);
		this_node->right = build_segment_tree(mid + 1, rbound,
						      this_node, points);
	}
	return this_node;
}

void update_parent_chain(Node *root) {
	for (Node *tnode = root->parent; tnode; tnode = tnode->parent) {
		tnode->q1 = tnode->left->q1 + tnode->right->q1;
		tnode->q2 = tnode->left->q2 + tnode->right->q2;
		tnode->q3 = tnode->left->q3 + tnode->right->q3;
		tnode->q4 = tnode->left->q4 + tnode->right->q4;
	}
}

void reflect_x(Node *root, bool update_parent) {
	int temp;

	temp = root->q1;
	root->q1 = root->q4;
	root->q4 = temp;
	temp = root->q2;
	root->q2 = root->q3;
	root->q3 = temp;
	
	if (update_parent)
		update_parent_chain(root);

	if (root->left) {
		reflect_x(root->left, false);
		reflect_x(root->right, false);
	}
}

void reflect_y(Node *root, bool update_parent) {
	int temp;

	temp = root->q1;
	root->q1 = root->q2;
	root->q2 = temp;
	temp = root->q4;
	root->q4 = root->q3;
	root->q3 = temp;

	if (update_parent)
		update_parent_chain(root);

	if (root->left) {
		reflect_y(root->left, false);
		reflect_y(root->right, false);
	}
}

void collect_q(Node *root, bool dummy) {
	result.q1 += root->q1;
	result.q2 += root->q2;
	result.q3 += root->q3;
	result.q4 += root->q4;
}

template <typename Function>
void segment_exec(int lbound, int rbound, Node *root, Function f) {
	if (lbound == root->lbound and rbound == root->rbound)
		f(root, true);
	else if (lbound >= root->right->lbound)
		segment_exec(lbound, rbound, root->right, f);
	else if (rbound <= root->left->rbound)
		segment_exec(lbound, rbound, root->left, f);
	else {
		segment_exec(lbound, root->left->rbound, root->left, f);
		segment_exec(root->right->lbound, rbound, root->right, f);
	}
}

void execute_query(Query *this_query, Node *root) {
	switch (this_query->type) {
	case 'X':
		segment_exec(this_query->lbound,
			     this_query->rbound, root, reflect_x);
		break;
	case 'Y':
		segment_exec(this_query->lbound,
			     this_query->rbound, root, reflect_y);
		break;
	case 'C':
		memset(&result, 0, sizeof(struct qresult));
		segment_exec(this_query->lbound,
			     this_query->rbound, root, collect_q);
		cout << result.q1 << " " << result.q2 << " " <<
			result.q3 << " " << result.q4 << endl;
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
	Node *root = build_segment_tree(1, n_points, NULL, points);
	for (vector<Query *>::iterator i = queries.begin();
	     i != queries.end(); i++)
		execute_query(*i, root);
	return 0;
}
