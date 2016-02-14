/*****************************************************************************
 * libds :: list.h
 *
 * Linked list/queue implementation.
 *
 * Author:  Chris Rink <chrisrink10@gmail.com>
 *
 * License: MIT (see LICENSE document at source tree root)
 *****************************************************************************/

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include "libds/list.h"
#include "listpriv.h"
#include "iterpriv.h"

struct DSList {
    struct node *head;
    struct node *foot;
    size_t len;
    dslist_compare_fn cmp;
    dslist_free_fn free;
};

static struct node *make_node(void *elem, struct node *next, struct node *prev);
static void *remove_node(DSList *list, struct node *cur);

/*
 * LIST PUBLIC FUNCTIONS
 */

DSList *dslist_new(dslist_compare_fn cmpfn, dslist_free_fn freefn) {
    DSList *list = malloc(sizeof(struct DSList));
    if (!list) {
        return NULL;
    }

    list->head = NULL;
    list->foot = NULL;
    list->len = 0;
    list->cmp = cmpfn;
    list->free = freefn;
    return list;
}

void dslist_destroy(DSList *list) {
    if (!list) { return; }
    dslist_clear(list);
    free(list);
}

size_t dslist_len(DSList *list) {
    assert(list);
    return list->len;
}

void* dslist_get(DSList *list, size_t index) {
    if ((!list) || (index >= list->len)) {
        return NULL;
    }

    struct node *cur = list->head;
    size_t count = 0;
    while (cur) {
        if (index == count) {
            return cur->data;
        }
        cur = cur->next;
        count++;
    }

    return NULL;
}

void dslist_foreach(DSList *list, void (*func)(void*)) {
    if ((!list) || (!func)) { return; }

    struct node *cur = list->head;
    while (cur) {
        func(cur->data);
        cur = cur->next;
    }
}

bool dslist_append(DSList *list, void *elem) {
    return (list) ? dslist_insert(list, elem, list->len) : false;
}

bool dslist_extend(DSList *list, DSList *other){
    if ((!list) || (!other)) { return false; }
    if ((list->cmp != other->cmp) || (list->free != other->free)) {
        return false;
    }

    if (list->len == 0) {
        list->head = other->head;
        list->foot = other->foot;
        list->len = other->len;
        other->head = NULL;
        other->foot = NULL;
        other->len = 0;
        return true;
    }

    list->foot->next = other->head;
    other->head->prev = list->foot;
    list->foot = other->foot;
    list->len += other->len;
    other->head = NULL;
    other->foot = NULL;
    other->len = 0;
    return true;
}

bool dslist_insert(DSList *list, void *elem, size_t index){
    if ((!list) || (!elem)) { return false; }
    if (index > list->len) { return false; }

    // Insert at the front of the list (or starting a new list)
    if (index == 0) {
        struct node *newnode = make_node(elem, list->head, NULL);
        if (!newnode) { return false; }

        if (list->head) { list->head->prev = newnode; }
        list->head = newnode;
        if (list->len == 0) { list->foot = newnode; }
        list->len++;
        return true;
    }

    // Append an element at the end of the list
    if  (index == list->len) {
        struct node *newnode = make_node(elem, NULL, list->foot);
        if (!newnode) { return false; }

        list->foot->next = newnode;
        list->foot = newnode;
        list->len++;
        return true;
    }

    // Insert an element in the middle of the list
    struct node *cur = list->head;
    size_t count = 0;
    while (cur) {
        if (count == index) {
            struct node *newnode = make_node(elem, cur, cur->prev);
            if (!newnode) { return false; }
            if (cur->prev) { cur->prev->next = newnode; }
            list->len++;
            return true;
        }
        cur = cur->next;
        count++;
    }

    return false;
}

void* dslist_remove(DSList *list, void *elem){
    if ((!list) || (!elem)) { return NULL; }
    if (!list->cmp) { return NULL; }

    struct node *cur = list->head;
    while (cur) {
        if (list->cmp(&cur->data, &elem) == 0) {
            return remove_node(list, cur);
        }
        cur = cur->next;
    }

    return NULL;
}

void* dslist_remove_index(DSList *list, size_t index){
    if (!list) { return NULL; }
    if (index >= list->len) { return NULL; }

    struct node *cur = list->head;
    size_t count = 0;
    while (cur) {
        if (index == count) {
            return remove_node(list, cur);
        }
        cur = cur->next;
        count++;
    }

    return NULL;
}

bool dslist_enqueue(DSList *list, void *elem){
    return (list) ? dslist_insert(list, elem, list->len) : false;
}

void* dslist_dequeue(DSList *list){
    if ((!list) || (list->len < 1)) { return NULL; }

    // Point the list header to the next node
    struct node *head = list->head;
    void *data = head->data;
    list->head = head->next;
    if (list->head) { list->head->prev = NULL; }
    if ((!list->head) && (list->len == 1)) { list->foot = NULL; }
    list->len--;

    // Destroy references and free the node
    head->next = NULL;
    head->prev = NULL;
    head->data = NULL;
    free(head);

    return data;
}

void* dslist_pop(DSList *list){
    if ((!list) || (list->len < 1)) { return NULL; }

    // Point the list footer to the previous node
    struct node *foot = list->foot;
    void *data = foot->data;
    list->foot = foot->prev;
    if (list->foot) { list->foot->next = NULL; }
    if ((!list->foot) && (list->len == 1)) { list->head = NULL; }
    list->len--;

    // Destroy references and free the node
    foot->next = NULL;
    foot->prev = NULL;
    foot->data = NULL;
    free(foot);

    return data;
}

void dslist_clear(DSList *list){
    if (!list) { return; }
    bool can_free = (list->free) ? true : false;

    struct node *cur = list->head;
    struct node *prev = cur;
    while (cur) {
        if (can_free) {
            list->free(cur->data);
        }
        cur = cur->next;
        free(prev);
        prev = cur;
        list->len--;
    }

    list->head = NULL;
    list->foot = NULL;
}

int dslist_index(DSList *list, void *elem){
    if ((!list) || (!elem)) { return DSLIST_NULL_POINTER; }
    if (!list->cmp) { return DSLIST_NO_CMP_FUNC; }

    struct node *cur = list->head;
    int index = 0;
    while (cur) {
        if (list->cmp(&cur->data, &elem) == 0) {
            return index;
        }
        cur = cur->next;
        index++;
    }

    return DSLIST_NOT_FOUND;
}

void dslist_reverse(DSList *list) {
    if (!list) { return; }

    struct node *cur = list->head;
    while (cur) {
        struct node *swap = cur->next;
        cur->next = cur->prev;
        cur->prev = swap;

        cur = cur->prev;
    }

    struct node *temp = list->head;
    list->head = list->foot;
    list->foot = temp;
}

DSIter *dslist_iter(DSList *list) {
    if (!list) { return NULL; }

    DSIter *iter = dsiter_priv_new(ITER_LIST, list);
    if (!iter) {
        return NULL;
    }

    return iter;
}

/*
 * LIST PRIVATE FUNCTIONS
 */

// Wrap the code required to create a new node
static struct node *make_node(void *elem, struct node *next, struct node *prev) {
    struct node *newnode = malloc(sizeof(struct node));
    if (!newnode) { return false; }
    newnode->data = elem;
    newnode->next = next;
    newnode->prev = prev;
    return newnode;
}

// Remove the current node and fix any links
static void *remove_node(DSList *list, struct node *cur) {
    assert(list);
    assert(cur);

    if (!cur->prev) {
        list->head = cur->next;
    } else {
        cur->prev->next = cur->next;
    }

    if (!cur->next) {
        list->foot = cur->prev;
    } else {
        cur->next->prev = cur->prev;
    }

    list->len--;
    void *data = cur->data;
    free(cur);
    return data;
}

// Iterate on the next list entry
bool dsiter_dslist_next(DSIter *iter, bool advance) {
    assert(iter);
    assert(iter->type == ITER_LIST);

    if (DSITER_IS_FINISHED(iter)) {
        return false;
    } else if (DSITER_IS_NEW_ITER(iter)) {
        if (!advance) {
            return (iter->target.list->len > 0);
        }

        iter->node.list = iter->target.list->head;
        iter->cur++;
        iter->stat = DSITER_NORMAL;
    } else {
        if (advance) {
            iter->cur++;
            iter->node.list = (iter->node.list) ? (iter->node.list->next) : NULL;
        }
    }

    void *data = (iter->node.list) ? (iter->node.list->data) : NULL;
    if (data) {
        return true;
    }

    if ((advance) && (!iter->node.list)) {
        iter->stat = DSITER_NO_MORE_ELEMENTS;
    }
    return false;
}
