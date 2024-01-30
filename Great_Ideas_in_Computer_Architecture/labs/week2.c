#include <stdio.h>

// EX 2
/*
// Only change any of these 4 values
#define V0 3
#define V1 3
#define V2 3
#define V3 3

int main(void) {
	int a;
	char *s;

	printf("Berkeley eccentrics:\n====================\n");

	// for loop 
	for(a=0; a<V0; a++) {
		printf("Happy ");
	}
	printf("\n");

	// switch statement
	switch(V1) {
		case 0:		printf("Yoshua\n");
		case 1: 	printf("Triangle Man\n");	break;
		case 2: 	printf("Chinese Erhu Guy\n");
		case 3: 	printf("Yoshua\n");		break;
		case 4:		printf("Dr. Jokemon\n");	break;
		case 5:		printf("Hat Lady\n");
		default:	printf("I don't know these people!\n");
	}

	// ternary operator
	s = (V3==3) ? "Go" : "Boo";

	// if statement 
	if(V2) {
		printf("\n%s BEARS!\n",s);
	} else  {
		printf("\n%s CARDINAL!\n",s);
	}
    
	return 0;
}
*/



//EX3
/*
typedef struct node {
	int val;
	struct node* next;
} node;
// 
// FIXME: this function is buggy. 
int ll_equal(const node* a, const node* b) { //Hmmm... why are a and b typed as const when they are reassigned?
	while ((a != NULL) && (b != NULL)) { //CORRECTION HERE... it works now? All I saw was that b needs to be check if it is null. They do seem to be pointing to the correct address.
		if (a->val != b->val)
			return 0;
		a = a->next;
		b = b->next;
	}
	// lists are equal if a and b are both null
	return a == b;
}


int main(int argc, char** argv) {
	int i;
	node nodes[10];

	for (i=0; i<10; i++) {
		nodes[i].val = 0;
		nodes[i].next = NULL;
	}

	nodes[0].next = &nodes[1];
	nodes[1].next = &nodes[2];
	nodes[2].next = &nodes[3];

	printf("equal test 1 result = %d\n", ll_equal(&nodes[0], &nodes[0]));
	printf("equal test 2 result = %d\n", ll_equal(&nodes[0], &nodes[2]));

	return 0;
}
*/

//EX

typedef struct node {
	int value;
	struct node *next;
} node;

/* int ll_has_cycle(node *head) {
    node *hare = head;
    node *tortoise = head;
    
    while (hare != NULL && hare->next != NULL) {
        hare = hare->next->next; // Move hare two steps
        tortoise = tortoise->next; // Move tortoise one step
        
        if (hare == tortoise) {
            return 1; // Cycle detected
        }
    }
    
    return 0; // No cycle detected
}
 */
int ll_has_cycle(node *head) {
	// your code here
	node *hare = head;
	node *tortise = head;
	while (hare != NULL){
		hare = hare->next;
		if (hare == NULL){
			return 0;
		}
		hare = hare->next;
		tortise = tortise->next;
		if (tortise == hare){
			return 1;
	    }
	}
}

void test_ll_has_cycle(void) {
	int i;
	node nodes[25]; //enough to run our tests
	for(i=0; i < sizeof(nodes)/sizeof(node); i++) {
		nodes[i].next = 0;
		nodes[i].value = 0;
	}
	nodes[0].next = &nodes[1];
	nodes[1].next = &nodes[2];
	nodes[2].next = &nodes[3];
	printf("Checking first list for cycles. There should be none, ll_has_cycle says it has %s cycle\n", ll_has_cycle(&nodes[0])?"a":"no");
  
	nodes[4].next = &nodes[5];
	nodes[5].next = &nodes[6];
	nodes[6].next = &nodes[7];
	nodes[7].next = &nodes[8];
	nodes[8].next = &nodes[9];
	nodes[9].next = &nodes[10];
	nodes[10].next = &nodes[4];
	printf("Checking second list for cycles. There should be a cycle, ll_has_cycle says it has %s cycle\n", ll_has_cycle(&nodes[4])?"a":"no");
  
	nodes[11].next = &nodes[12];
	nodes[12].next = &nodes[13];
	nodes[13].next = &nodes[14];
	nodes[14].next = &nodes[15];
	nodes[15].next = &nodes[16];
	nodes[16].next = &nodes[17];
	nodes[17].next = &nodes[14];
	printf("Checking third list for cycles. There should be a cycle, ll_has_cycle says it has %s cycle\n", ll_has_cycle(&nodes[11])?"a":"no");
  
	nodes[18].next = &nodes[18];
	printf("Checking fourth list for cycles. There should be a cycle, ll_has_cycle says it has %s cycle\n", ll_has_cycle(&nodes[18])?"a":"no");
  
	nodes[19].next = &nodes[20];
	nodes[20].next = &nodes[21];
	nodes[21].next = &nodes[22];
	nodes[22].next = &nodes[23];
	printf("Checking fifth list for cycles. There should be none, ll_has_cycle says it has %s cycle\n", ll_has_cycle(&nodes[19])?"a":"no");
  
	printf("Checking length-zero list for cycles. There should be none, ll_has_cycle says it has %s cycle\n", ll_has_cycle(NULL)?"a":"no");
}

int main(void) {
  test_ll_has_cycle();
  return 0;
}