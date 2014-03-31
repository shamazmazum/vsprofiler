#include <CUnit/CUError.h>
#include <CUnit/TestDB.h>
#include <CUnit/TestRun.h>
#include <CUnit/Basic.h>

#include <stdio.h>

#include "squeue.h"

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
    squeue_t test_queue;
    smpl_t num[] = {1, 2, 3, 4, 5, 6};
    int i, n = 6;
    smpl_t val;
    squeue_init (&test_queue);

    for (i=0; i<n; i++) squeue_push_entry (&test_queue, num[i]);
    squeue_finalize_sample (&test_queue);
    CU_ASSERT (squeue_entries(&test_queue) == n+1);
    CU_ASSERT (squeue_samples(&test_queue) == 1);

    for (i=0; i<n; i++)
    {
        squeue_pop_entry (&test_queue, &val);
        CU_ASSERT (val == num[i]);
    }
    squeue_pop_entry (&test_queue, &val);
    CU_ASSERT (val == SAMPLE_TERM);
    
    squeue_free (&test_queue);
}

void push_pop2 ()
{
    squeue_t test_queue;
    smpl_t num[] = {1, 2, 3, 4, 5, 6};
    int i, n = 6;
    smpl_t val;
    squeue_init (&test_queue);

    for (i=0; i<n; i++)
    {
        squeue_push_entry (&test_queue, num[i]);
        squeue_finalize_sample (&test_queue);
    }
    CU_ASSERT (squeue_entries(&test_queue) == 2*n);
    CU_ASSERT (squeue_samples(&test_queue) == n);

    for (i=0; i<n; i++)
    {
        squeue_pop_entry (&test_queue, &val);
        CU_ASSERT (val == num[i]);
        squeue_pop_entry (&test_queue, &val);
        CU_ASSERT (val == SAMPLE_TERM);
    }
    
    squeue_free (&test_queue);
}

void push_pop3 ()
{
    squeue_t test_queue;
    smpl_t num[] = {1, 2, 3, 4, 5, 6};
    int i, n = 6;
    smpl_t val;
    squeue_init (&test_queue);

    /* for (i=0; i<n; i++) squeue_push_entry (&test_queue, num[i]); */
    /* for (i=1; i<=3; i++) squeue_pop_entry (&test_queue, &val); */
    /* for (i=0; i<n; i++) squeue_push_entry (&test_queue, num[i]); */
    /* for (i=1; i<=n; i++) */
    /* { */
    /*     squeue_pop_entry (&test_queue, &val); */
    /*     CU_ASSERT (val == num[n-i]); */
    /* } */
    /* for (i=4; i<=n; i++) */
    /* { */
    /*     squeue_pop_entry (&test_queue, &val); */
    /*     CU_ASSERT (val == num[n-i]); */
    /* } */

    /* CU_ASSERT (squeue_entries(&test_queue) == 0); */


    for (i=0; i<n; i++) squeue_push_entry (&test_queue, num[i]);
    for (i=0; i<3; i++) squeue_pop_entry (&test_queue, &val);
    for (i=0; i<n; i++) squeue_push_entry (&test_queue, num[i]);
    squeue_finalize_sample (&test_queue);
    CU_ASSERT (squeue_entries(&test_queue) == 3+6+1);
    CU_ASSERT (squeue_samples(&test_queue) == 1);
    
    for (i=3; i<n; i++)
    {
        squeue_pop_entry (&test_queue, &val);
        CU_ASSERT (val == num[i]);
    }
    for (i=0; i<n; i++)
    {
        squeue_pop_entry (&test_queue, &val);
        CU_ASSERT (val == num[i]);
    }
    squeue_pop_entry (&test_queue, &val);
    CU_ASSERT (val == 0);

    CU_ASSERT (squeue_entries(&test_queue) == 0);
    CU_ASSERT (squeue_samples(&test_queue) == 0);
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

    // Sample queue
    CU_pSuite samp_queue = CU_add_suite ("sample queue", NULL, NULL);
    if (samp_queue == NULL) PROC_SUIT_ERROR;

    test = CU_add_test (samp_queue, "push/pop 1", push_pop1);
    if (test == NULL) PROC_TEST_ERROR;

    test = CU_add_test (samp_queue, "push_pop 2", push_pop2);
    if (test == NULL) PROC_TEST_ERROR;

        test = CU_add_test (samp_queue, "push_pop 3", push_pop3);
    if (test == NULL) PROC_TEST_ERROR;

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    
regcleanup: CU_cleanup_registry();
exit_:    return res;
}
