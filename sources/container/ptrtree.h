#pragma once

typedef struct _tagTNode TNODE, *PTNODE;
struct _tagTNode {
    PTNODE parent;
    PTNODE prev;
    PTNODE next;
    PTNODE firstchild;
    PTNODE lastchild;
    BYTE data[1];
};
