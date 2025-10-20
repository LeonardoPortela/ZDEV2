  TRY.
      lo_oc_email_manager->add_emails_to_bcs( EXPORTING it_oc_email_recipients = gt_oc_email_recipients
                                               CHANGING co_bcs                 = lo_cl_bcs ).

    CATCH cx_sy_ref_is_initial INTO lo_sy_ref_is_initial.
      lv_exception_text = lo_sy_ref_is_initial->get_longtext( ).
  ENDTRY.

                 "add_emails_to_bcs_class
