/* Include the system headers we need */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Include our header */
#include "vector.h"

/* Define what our struct is */
struct vector_t {
	size_t size;
	int *data;
};

/* Utility function to handle allocation failures. In this
   case we print a message and exit. */
static void allocation_failed() {
    fprintf(stderr, "Out of memory.\n");
    exit(1);
}

/* Create a new vector */
vector_t *vector_new() {
	vector_t *retval;  

	/* First, we need to allocate the memory for the struct */
	retval = malloc(1 * sizeof(vector_t));

	/* Check our return value to make sure we got memory */
	if(retval == NULL)
                allocation_failed();
	 
	/* Now we need to initialize our data */
	retval->size = 1;
	retval->data = malloc(retval->size * sizeof(int));

	/* Check our return value to make sure we got memory */
	if(retval->data == NULL) {
		free(retval);
                allocation_failed();
	}

	retval->data[0] = 0;
	
	/* and return... */
	return retval;
}

/* Free up the memory allocated for the passed vector */
void vector_delete(vector_t *v) {
	/* Remember, you need to free up ALL the memory that is allocated */
	free((void *) v->data);
	free((void *) v);
}
/* Return the value in the vector */
int vector_get(vector_t *v, size_t loc) {

	/* If we are passed a NULL pointer for our vector, complain about it and
         * exit.
	 */
	if(v == NULL) {
		fprintf(stderr, "vector_get: passed a NULL vector.\n");
                abort();
	}

	/* If the requested location is higher than we have allocated, return 0.
	 * Otherwise, return what is in the passed location.
	 */
	// pr/intf("get \n");
	if(loc < v->size) {
		// printf("loc %lu \n", loc);
		// printf("size %lu \n", v->size);
		// printf("v->data[loc] %d \n", v->data[loc]);
		return v->data[loc];
	} else {
		// printf("fail loc %lu \n", loc);
		// printf("fail size %lu \n", v->size);
		// printf("fail \n\n");
		return 0;
	}
}

/* Set a value in the vector. If the extra memory allocation fails, call
   allocation_failed(). */
void vector_set(vector_t *v, size_t loc, int value) {
	/* What do you need to do if the location is greater than the size we have
	 * allocated?  Remember that unset locations should contain a value of 0.
	 */
	// check size
	if (loc > v->size){
		// printf("> loc %lu \n", loc);
		// printf("size %lu \n", v->size);
		int *data_new = (int *) calloc((loc+1),  sizeof(int));
		// printf(" (loc+1) * sizeof(int) %lu \n", ((loc+1) * sizeof(int)));
		if(data_new == NULL) {
			free(data_new);
			allocation_failed();
			}
		//printf("sizeof %ld \n", sizeof(v->data));
		
		
		size_t i;


		for (i = 0; i < v->size; i++){
			data_new[i] = 0;
		}

		for (i = 0; i < v->size; i++){
			data_new[i] = v->data[i];
		}
		free(v->data);
		v->data = data_new;
		
		v->data[loc] = value;
		v->size = loc + 1;

	} else{
		// printf("< loc %lu \n", loc);
		// printf("size %lu \n", v->size);
		v->data[loc] = value;
		// printf("val %d \n", v->data[loc]);
	}
	//if (v->size > 15){printf("val 15 %d \n", v->data[15]);}
	// printf("val %d \n", v->data[loc]);


}
