#include <CUnit/CUError.h>
#include <CUnit/TestDB.h>
#include <CUnit/TestRun.h>
#include <CUnit/Basic.h>

#include <stdio.h>

#include "vector.h"

#define PROC_SUIT_ERROR do {res = CU_get_error (); \
        printf ("Failed to add a suite\n");        \
        goto regcleanup;}                          \
    while (0);

#define PROC_TEST_ERROR do {res = CU_get_error (); \
        printf ("Failed to add a test\n");         \
        goto regcleanup;}                          \
    while (0);

void push_pop1 ()
{
    vector_t test_vec;
    int num[] = {1, 2, 3, 4, 5, 6};
    int n = 6;
    int i, val;
    init_vector (&test_vec, sizeof(int));

    for (i=0; i<n; i++) push (&test_vec, &(num[i]));
    CU_ASSERT (elements(&test_vec) == n);
    for (i=1; i<=n; i++)
    {
        pop (&test_vec, &val);
        CU_ASSERT (val == num[n-i]);
    }
    
    free_vector (&test_vec);
}

void push_pop2 ()
{
    vector_t test_vec;
    int num[] = {1, 2, 3, 4, 5, 6};
    int n = 6;
    int i, val;
    init_vector (&test_vec, sizeof(int));

    for (i=0; i<n; i++) push (&test_vec, &(num[i]));
    for (i=1; i<=3; i++) pop (&test_vec, &val);
    for (i=0; i<n; i++) push (&test_vec, &(num[i]));
    for (i=1; i<=n; i++)
    {
        pop (&test_vec, &val);
        CU_ASSERT (val == num[n-i]);
    }
    for (i=4; i<=n; i++)
    {
        pop (&test_vec, &val);
        CU_ASSERT (val == num[n-i]);
    }

    CU_ASSERT (elements(&test_vec) == 0);    
}
        
int main ()
{
    CU_pTest test;
    
    CU_ErrorCode res = CU_initialize_registry();
    if (res != CUE_SUCCESS)
    {
        printf ("Failed to initialize registry\n");
        goto exit_;
    }

    // Sample stack
    CU_pSuite samp_stack = CU_add_suite ("sample stack", NULL, NULL);
    if (samp_stack == NULL) PROC_SUIT_ERROR;

    test = CU_add_test (samp_stack, "push/pop 1", push_pop1);
    if (test == NULL) PROC_TEST_ERROR;

    test = CU_add_test (samp_stack, "push_pop 2", push_pop2);
    if (test == NULL) PROC_TEST_ERROR;

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    
regcleanup: CU_cleanup_registry();
exit_:    return res;
}
