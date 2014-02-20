/**
 * Binary search tree implementation
 *
 * Copyright (c) 2013 the authors listed at the following URL, and/or
 * the authors of referenced articles or incorporated external code:
 * http://en.literateprograms.org/Binary_search_tree_(C)?action=history&offset=20121127201818
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Retrieved from: http://en.literateprograms.org/Binary_search_tree_(C)?oldid=18734
 * Modified: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 */


/***********************************************************/
/* NOTE: You can modify/add any piece of code that will    */
/* make your algorithm work                                */
/***********************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "bst.h"

pthread_mutex_t cg_mutex;

static void lock_lock(struct bst_node** node) {
    if (!((*node) == NULL)) {
//        puts("Tar ett lock!\n");
        pthread_mutex_lock(&((*node)->node_mutex));
    }
    
}

static void unlock_lock(struct bst_node** node) {
    if (!((*node) == NULL)) {
//        puts("Ger ett lock!\n");
        pthread_mutex_unlock(&((*node)->node_mutex));
    }
    
}

/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           the node containg the data
 */
struct bst_node** search(struct bst_node** root, comparator compare, void* data)
{

    /* TODO: For the Step 2 you will have to make this function thread-safe */
    //lock_lock(root);
    struct bst_node** node = root;

    while (*node != NULL) {
        struct bst_node* prev = *node;
        int compare_result = compare(data, (*node)->data);
        if (compare_result < 0) {
            // lock_lock(&((*node)->left));
            //unlock_lock(&prev);
            node = &(*node)->left;
        }
        else if (compare_result > 0) {
            //lock_lock(&((*node)->right));
            //unlock_lock(&prev);
            node = &(*node)->right;
        }
        else
            break;
    }
////    if the node is makred for deletion it does not exist
    /* if (!((*node) == NULL)) { */
    /*     if ((*node)->deleted == 1) { */
    /*         node = NULL; */
    /*     } */
    /* } */
////
    return node;
}


/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           the node containg the data
 */
struct bst_node** search_two(struct bst_node** root, comparator compare, void* data)
{

    /* TODO: For the Step 2 you will have to make this function thread-safe */
    lock_lock(root);
    struct bst_node** node = root;

    while (*node != NULL) {
        struct bst_node* prev = *node;
        int compare_result = compare(data, (*node)->data);
        if (compare_result < 0) {
            lock_lock(&((*node)->left));
            unlock_lock(&prev);
            node = &(*node)->left;
        }
        else if (compare_result > 0) {
            lock_lock(&((*node)->right));
            unlock_lock(&prev);
            node = &(*node)->right;
        }
        else
            break;
    }
////    if the node is makred for deletion it does not exist
    /* if (!((*node) == NULL)) { */
    /*     if ((*node)->deleted == 1) { */
    /*         node = NULL; */
    /*     } */
    /* } */
////
    return node;
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void node_delete_aux(struct bst_node** node)
{
    /* TODO: For Step 2 you will have to make this function thread-safe */

    struct bst_node* old_node = *node;

    if ((*node)->left == NULL) {
        *node = (*node)->right;
        free_node(old_node);
    } else if ((*node)->right == NULL) {
        *node = (*node)->left;
        free_node(old_node);
    } else {
        struct bst_node** pred = &(*node)->left;
	while ((*pred)->right != NULL) {
	    pred = &(*pred)->right;
	}

	/* Swap values */
	void* temp = (*pred)->data;
	(*pred)->data = (*node)->data;
	(*node)->data = temp;

	node_delete_aux(pred);
    }
}



/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void node_delete_aux_fg(struct bst_node** node)
{
    /* TODO: For Step 2 you will have to make this function thread-safe */
    ////
    //lock_lock(node);
    struct bst_node** root = node;
    struct bst_node* old_node = *node;
    
    if ((*node)->left == NULL) {
         lock_lock(&((*node)->right)); 
         //(*node)->deleted = 1; 
         *node = (*node)->right;
         unlock_lock(root); 
         free_node(old_node); 
         unlock_lock(node); 
    } else if ((*node)->right == NULL) {
         lock_lock(&((*node)->left)); 
         //(*node)->deleted = 1; 
         *node = (*node)->left;
         unlock_lock(root);   
         free_node(old_node); 
         unlock_lock(node);
    } else {
        lock_lock((&(*node)->left));
        struct bst_node** pred = &((*node)->left);
        struct bst_node** pred_tmp = &((*node)->left);
	while ((*pred)->right != NULL) {
            lock_lock((&(*pred)->right));
	    pred = &(*pred)->right;
            unlock_lock(pred_tmp);
            pred_tmp = pred;
	}

	/* Swap values */
	void* temp = (*pred)->data;
	(*pred)->data = (*node)->data;
	(*node)->data = temp;

        ////
        //(*pred)->deleted = 1;
        //   unlock_lock(pred);
        unlock_lock(root);
	node_delete_aux_fg(pred);
    }
}

/**
 * Deletes the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           -1 if data is not found, 0 otherwise
 */
int
node_delete(struct bst_node** root, comparator compare, void* data)
{
    lock_lock(root);
    struct bst_node** node = search(root, compare, data);
    unlock_lock(root);
    if (*node == NULL)
        return -1;

    node_delete_aux(node);

    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_cg(struct bst_node** root, comparator compare, void* data)
{
    pthread_mutex_lock(&(cg_mutex));    
/* TODO: Fill-in the body of this function */
    struct bst_node** node = search(root, compare, data);

    if (node == NULL)
        return -1;
    
    node_delete_aux(node);
    pthread_mutex_unlock(&(cg_mutex));
    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_fg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */
    
    struct bst_node** node = search_two(root, compare, data);
    
    if (node == NULL)
        return -1;
    
    node_delete_aux_fg(node);
    
    return 0;
}


/**
 * Allocate resources and initialize a BST.
 *
 * @return           root of the BST
 */
struct bst_node **
tree_init(void)
{
    struct bst_node** root = malloc(sizeof(*root));
    if (root == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    *root = NULL;

    /* TODO: Initialize any global variables you use for the BST */
    pthread_mutex_init(&(cg_mutex), NULL);
    return root;
}

/**
 * Remove resources for the tree.
 *
 * @param root       root of the tree
 */
void
tree_fini(struct bst_node ** root)
{
    /* TODO: Free any global variables you used for the BST */

    if (root != NULL)
        free(root);
}


/**
 * Inserts a new node with the requested data if not already in the tree.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be inserted
 * @return           1 if data is in the BST already, 0 otherwise
 */
int
node_insert(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);
    if (*node == NULL) {
        *node = new_node(data);
        return 0;
    } else
        return 1;
}


/**
 * Creates a new node with the requested data.
 *
 * @param data       pointer to the data pointed be the new node
 */
struct bst_node* 
new_node(void* data)
{
    struct bst_node* node = malloc(sizeof(struct bst_node));
    if (node == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    } else {
        /* TODO: Initialize any per node variables you use for the BST */

        node->left = NULL;
        node->right = NULL;
        node->data = data;
        pthread_mutex_init(&(node->node_mutex), NULL);
        //node->deleted = 0;
    }

    return node;
}


/**
 * Deletes a node.
 *
 * @param node       node to be freed
 */
void
free_node(struct bst_node* node) 
{
    if (node == NULL)
        fprintf(stderr, "Invalid node\n");
    else {
        /* TODO: Finalize any per node variables you use for the BST */
        free(node);
    }
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
