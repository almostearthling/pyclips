/* loptr.c
 *  Simple list-of-pointers handling library; this library is intended to
 *  be directly included in the source file that would use it, without prior
 *  or separated compilation. We use a hash table not just because we expect
 *  to be likely to have many records in the list, but also hoping that in
 *  many cases this will make it faster to find out whether or not a pointer
 *  is already present.
 *
 * Even though this library is useless, it's however released under the LGPL.
 *
 * $Id: loptr.c 340 2008-02-21 00:39:34Z Franz $
 * (c) 2008 - Francesco Garosi/JKS
 */



/* the kind of integers we will be dealing with */
#define LOPTR_INT unsigned long
#define LOPTR_BOOL int
#define LOPTR_TRUE 1
#define LOPTR_FALSE 0

/* the following can be helpful when allocating objects */
#ifdef USE_PYTHON_MEMMGR
#define LOPTR_NEW(x) PyMem_New(x, 1)
#define LOPTR_DELETE(x) PyMem_Del(x)
#else
#define LOPTR_NEW(x) ((x *)malloc(sizeof(x)))
#define LOPTR_DELETE(x) free(x)
#endif

/* these functions are intended to be very fast and inlined */
#ifdef F_INLINE
#define LOPTR_INLINE F_INLINE
#else
#define LOPTR_INLINE
#endif /* F_INLINE */

/* size of hash table for sublists of pointers, should be a prime */
#ifndef STRAY_FACTS_TABLE_SIZE
#define STRAY_FACTS_TABLE_SIZE 211
#endif
#define LOPTR_HASH_TABLE_SIZE ((LOPTR_INT)STRAY_FACTS_TABLE_SIZE)
#define LOPTR_HASH(_x) (((LOPTR_INT)_x) % LOPTR_HASH_TABLE_SIZE)

/* decide whether or not to use register variables and pointers */
#ifdef USE_REGISTER_VARIABLES
#define LOPTR_REGISTER register
#else
#define LOPTR_REGISTER
#endif


/* will constitute the sublists of pointers */
typedef struct __struct_LOPTR_ITEM {
    void *elem;
    struct __struct_LOPTR_ITEM *next;
} LOPTR_ITEM;


/* type of function accepted by LOPTR_apply */
typedef LOPTR_BOOL(*LOPTR_FUNC)(void *);


/* give a standardized way to create a hash table */
#define LOPTR_HASH_TABLE(name) LOPTR_ITEM *name[LOPTR_HASH_TABLE_SIZE]
#define INIT_LOPTR_HASH_TABLE(name) \
	memset(name, 0, sizeof(LOPTR_ITEM *) * (LOPTR_HASH_TABLE_SIZE))
#define COPY_LOPTR_HASH_TABLE(dst, src) \
	memcpy((dst), (src), sizeof(LOPTR_ITEM *) * (LOPTR_HASH_TABLE_SIZE))


/* functions to transparently manage the list */

/* find an element */
static LOPTR_INLINE LOPTR_ITEM *LOPTR_find(LOPTR_ITEM **t, void *elem) {
    LOPTR_REGISTER LOPTR_ITEM *p = t[LOPTR_HASH(elem)];
    
    while(p) {
        if(p->elem == elem)
            return p;
        else
            p = p->next;
    }
    return NULL;
}

/* find the last item in the hash table for element */
static LOPTR_INLINE LOPTR_ITEM *LOPTR_lastitem(LOPTR_ITEM **t, void *elem) {
    LOPTR_REGISTER LOPTR_ITEM *p = t[LOPTR_HASH(elem)];
    
    if(p) {
        while(p->next != NULL)
            p = p->next;
        return p;
    } else
        return NULL;
}

/* append an element in the hash table */
static LOPTR_INLINE LOPTR_BOOL LOPTR_append(LOPTR_ITEM **t, void *elem) {
    LOPTR_ITEM *p = NULL, *q = NULL;
    
    if(LOPTR_find(t, elem))
        return LOPTR_FALSE;
    else {
        p = LOPTR_NEW(LOPTR_ITEM);
        p->elem = elem;
        p->next = NULL;
        q = LOPTR_lastitem(t, elem);
        if(q)
            q->next = p;
        else
            t[LOPTR_HASH(elem)] = p;
        return LOPTR_TRUE;
    }
}

/* remove an element from the hash table */
static LOPTR_INLINE LOPTR_BOOL LOPTR_remove(LOPTR_ITEM **t, void *elem) {
    int h = LOPTR_HASH(elem);
    LOPTR_REGISTER LOPTR_ITEM *p = t[h];
    LOPTR_ITEM *q = NULL;

    if(p) {
        if(p->elem == elem) {
            t[h] = p->next;  /* could be NULL */
            LOPTR_DELETE(p);
            return LOPTR_TRUE;
        } else {
            while(p) {
                q = p;
                p = p->next;
                if(p->elem == elem) {
                    q->next = p->next;  /* could be NULL either */
                    LOPTR_DELETE(p);
                    return LOPTR_TRUE;
                }
            }
        }
    }
    return LOPTR_FALSE;
}

/* apply a bool-returning function to list elements ignoring result */
static LOPTR_INLINE void LOPTR_apply(LOPTR_ITEM **t, LOPTR_FUNC f) {
    int i = 0;
    LOPTR_REGISTER LOPTR_ITEM *p = NULL;
    
    for(i = 0; i < LOPTR_HASH_TABLE_SIZE; i++) {
        p = t[i];
        while(p) {
            f(p->elem);
            p = p->next;
        }
    }
}

/* apply a bool-returning function to list elements returning failing result */
static LOPTR_INLINE void *LOPTR_test_apply(LOPTR_ITEM **t, LOPTR_FUNC f) {
    LOPTR_REGISTER int i = 0;
    LOPTR_REGISTER LOPTR_ITEM *p = NULL;
    
    for(i = 0; i < LOPTR_HASH_TABLE_SIZE; i++) {
        p = t[i];
        while(p) {
            if(!f(p->elem))
                return p->elem;
            p = p->next;
        }
    }
    return NULL;
}

/* reset a hash table (only leave the array) removing all possible lists */
static LOPTR_INLINE void LOPTR_reset_hash_table(LOPTR_ITEM **t) {
    LOPTR_REGISTER int i = 0;
    LOPTR_REGISTER LOPTR_ITEM *p = NULL;
    LOPTR_ITEM *q = NULL;
    
    for(i = 0; i < LOPTR_HASH_TABLE_SIZE; i++) {
        p = t[i];
        while(p) {
            q = p;
            p = p->next;
            LOPTR_DELETE(q);
        }
        t[i] = NULL;
    }
}



/* end. */
