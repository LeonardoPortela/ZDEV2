*&---------------------------------------------------------------------*
*&  Include           RVADOR01_REMOVE_DEFAULT
*&---------------------------------------------------------------------*


DATA:
  gt_oc_email_recipients TYPE if_oc_email=>tt_oc_email_recipients,
  lo_oc_email_manager    TYPE REF TO cl_oc_email_manager,
  lo_sy_ref_is_initial   TYPE REF TO cx_sy_ref_is_initial,
  lo_nast_key            TYPE if_oc_email=>ty_nast_key,
  lv_exception_text      TYPE string.

lo_oc_email_manager = cl_oc_email_manager=>get_instance( ).
MOVE-CORRESPONDING nast TO lo_nast_key.

TRY.
    gt_oc_email_recipients = lo_oc_email_manager->get_emails_by_type( iv_output_type = nast-kschl
                                                                      iv_object_key  = nast-objky
                                                                      iv_partner     = nast-parnr
                                                                      iv_partner_ro     = nast-parvw ).
    IF gt_oc_email_recipients IS INITIAL.
      gt_oc_email_recipients = lo_oc_email_manager->get_sent_emails_by_nast_key( iv_object_key = nast-objky
                                                                                 iv_nast_key   = lo_nast_key ).
    ENDIF.
    IF gt_oc_email_recipients IS NOT INITIAL.
      CLEAR ls_email_rcp.
    ENDIF.


  CATCH cx_sy_ref_is_initial INTO lo_sy_ref_is_initial.
    lv_exception_text = lo_sy_ref_is_initial->get_longtext( ).
ENDTRY.
