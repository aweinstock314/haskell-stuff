// g++ BinaryTree.cpp -std=c++11 
#include <memory>
using std::shared_ptr;
using std::make_shared;

template <class A> struct BinaryTree {
    A elem;
    shared_ptr<BinaryTree<A>> left, right;
};

template <class A> shared_ptr<BinaryTree<A>> makeTree(A x,
        shared_ptr<BinaryTree<A>> l, shared_ptr<BinaryTree<A>> r) {
    return make_shared({elem: x, left: l, right: r});
}

template <class A> shared_ptr<BinaryTree<A>> emptyTree() {
    return shared_ptr<BinaryTree<A>>(nullptr);
}

template <class A> shared_ptr<BinaryTree<A>>
treeInsert(A x, shared_ptr<BinaryTree<A>> tree) {
    if(tree.get() == nullptr) {
        return makeTree(x, emptyTree<A>(), emptyTree<A>());
    } else if(x < tree->elem) {
        return makeTree(tree->elem, treeInsert(x, tree->left), tree->right);
    } else if(tree->elem < x) {
        return makeTree(tree->elem, tree->left, treeInsert(x, tree->right));
    } else { // equal
        return tree;
    }
}

template <class A> shared_ptr<BinaryTree<A>>
treeFind(A x, shared_ptr<BinaryTree<A>> tree) {
    if(tree.get() == nullptr) {
        return tree;
    } else if(x < tree->elem) {
        return treeFind(x, tree->left);
    } else if(tree->elem < x) {
        return treeFind(x, tree->right);
    } else { // equal
        return tree;
    }
}

template <class A> shared_ptr<BinaryTree<A>>
treeRemove(A x, shared_ptr<BinaryTree<A>> tree) {
    if(tree.get() == nullptr) {
        return tree;
    } else if(x < tree->elem) {
        return treeRemove(x, tree->left);
    } else if(tree->elem < x) {
        return treeRemove(x, tree->right);
    } else { // equal
        if((tree->left.get() == nullptr) &&
            (tree->right.get() == nullptr)) {
            return emptyTree<A>();
        } else if(tree->right.get() == nullptr) {
            return tree->left;
        } else if(tree->left.get() == nullptr) {
            return tree->right;
        } else {
            // TODO: tree iterator implementation
            A lMax = std::max(begin(tree->left), end(tree->left));
            return makeTree(lMax, treeRemove(lMax, tree->left), tree->right);
        }
    }
}

