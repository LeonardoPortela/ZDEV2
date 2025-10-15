*----------------------------------------------------------------------*
*   INCLUDE LMB_BUS2017FM0                                             *
*----------------------------------------------------------------------*

* MAA2: error M7 599 during return delivery with BAPI          "n1654305
* do not carry out function module 'MB_CREATE_GOODS_MOVEMENT'  "n1654305
* after method 'MAA_ITEM_DUPLICATOR_BAPI' detected an error    "n1654305

*---------------------------------------------------------------------*
*      Form  MB_CANCEL_GOODS_MOVEMENT                                 *
*---------------------------------------------------------------------*
*      Call Function for to cancel the material document.             *
*---------------------------------------------------------------------*
FORM mb_cancel_goods_movement USING l_budat LIKE mkpf-budat
                                    l_mblnr LIKE mkpf-mblnr
                                    l_mjahr LIKE mkpf-mjahr.

ENHANCEMENT-SECTION     MB_CANCEL_GOODS_MOVEMENT_01 SPOTS ES_SAPLMB_BUS2017.
  CALL FUNCTION 'MB_CANCEL_GOODS_MOVEMENT'
       EXPORTING
            budat     = l_budat
            mblnr     = l_mblnr
            mjahr     = l_mjahr
            tcode     = 'MBST'
       IMPORTING
            emkpf     = s_emkpf
       TABLES
            emseg     = t_emseg
            imseg     = t_imseg
            mblpo_tab = t_imseg_cancel.
END-ENHANCEMENT-SECTION.
ENDFORM.
*---------------------------------------------------------------------*
*      Form  MB_CREATE_GOODS_MOVEMENT                                 *
*---------------------------------------------------------------------*
*      Call Function for to create the material document.             *
*---------------------------------------------------------------------*
FORM mb_create_goods_movement.
  DATA: old_subrc   type sy-subrc.

* -------------------------------------------------------------- MAA2
  DATA : lo_mmim_maa_2       TYPE  REF TO cl_mmim_maa_2,
         lv_flag_maa_active  type  abap_bool,

         lv_flag_error       type  abap_bool,
         lt_message_log      type  bapirettab,
         ls_message_log      like  line of lt_message_log,

         lt_imseg_out        TYPE  ty_t_imseg.
   DATA: lt_emseg_maa        TYPE TABLE OF EMSEG,
         s_emseg             TYPE EMSEG,
         s_emkpf_maa         TYPE EMKPF,
         lt_iseri_out                  type  iseri_t,
         lv_flag_serial_changed        type  abap_bool.

* check whether the business function multi account assignment is active
  CALL METHOD cl_ops_switch_check=>mm_sfws_maa
    RECEIVING
      rv_active = lv_flag_maa_active.

  IF  lv_flag_maa_active = abap_true.
    CALL METHOD cl_mmim_maa_2=>get_singleton_instance
      RECEIVING
        ro_singleton = lo_mmim_maa_2.

*   call without setting accounting objects
    CALL METHOD lo_mmim_maa_2->maa_item_duplicator_bapi
      EXPORTING
        it_imseg              = t_imseg[]
        is_imkpf              = s_imkpf
        it_serial_in          = t_iseri[]
      CHANGING
        ct_imseg_out          = lt_imseg_out
        ct_serial_out         = lt_iseri_out
        cv_flag_serial_changed = lv_flag_serial_changed
        ct_message_log        = lt_message_log
        cv_flag_error         = lv_flag_error.

    IF lv_flag_error = abap_true.
*     evaluate the messages
      LOOP AT lt_message_log         INTO  ls_message_log.
        CLEAR                        s_emseg.
        IF ls_message_log-row IS INITIAL.
          s_emkpf_maa-msgty      = ls_message_log-type.
          s_emkpf_maa-msgid      = ls_message_log-id.
          s_emkpf_maa-msgno      = ls_message_log-number.
          s_emkpf_maa-msgv1      = ls_message_log-message_v1.
          s_emkpf_maa-msgv2      = ls_message_log-message_v2.
          s_emkpf_maa-msgv3      = ls_message_log-message_v3.
          s_emkpf_maa-msgv4      = ls_message_log-message_v4.
        ELSE.
*         save the data of the error message into the next table
          s_emseg-msgty          = ls_message_log-type.
          s_emseg-msgid          = ls_message_log-id.
          s_emseg-msgno          = ls_message_log-number.
          s_emseg-msgv1          = ls_message_log-message_v1.
          s_emseg-msgv2          = ls_message_log-message_v2.
          s_emseg-msgv3          = ls_message_log-message_v3.
          s_emseg-msgv4          = ls_message_log-message_v4.
          s_emseg-global_counter = ls_message_log-row.
          APPEND  s_emseg           TO lt_emseg_maa.
        ENDIF.
      ENDLOOP.

      MOVE  '4'                     TO  s_emkpf_maa-subrc.
      CLEAR s_emseg.
    ELSE.
*     replace the content of input table IMSEG
      MOVE  lt_imseg_out[]           TO  t_imseg[].
*     refresh t_iseri.                                         "n1768662
*     move  lt_iseri_out[]           to  t_iseri[].            "n1768662
      CLEAR:  s_emkpf_maa.
      REFRESH lt_emseg_maa.
    ENDIF.
  ENDIF.

* begin -------------------------------------------------------"n1654305
* do not carry out function module 'MB_CREATE_GOODS_MOVEMENT'  "n1654305
* after method 'MAA_ITEM_DUPLICATOR_BAPI' detected an error    "n1654305
 IF NOT s_emkpf_maa-subrc IS INITIAL.
    MOVE s_emkpf_maa TO s_emkpf.
    APPEND LINES OF lt_emseg_maa to t_emseg.
    RETURN.        " error detected -> leave this routine
  ENDIF.
* end   -------------------------------------------------------"n1654305

* special handling when the serial numbers were changed
  IF  lv_flag_serial_changed = abap_true.
    CLEAR : t_iseri,                   t_iseri[].
    MOVE  : lt_iseri_out[]             TO  t_iseri[].

* Transfer information to local memory of serial number management
    CALL FUNCTION 'DOCUMENT_SERIALNOS_IMPORT'
      TABLES
        ser_tab = t_iseri.

*   Initialization of serialnumber tables *******************************
    CALL FUNCTION 'SERIALPROFILE_CHECK'
         EXPORTING
              operation            = 'SNCL'
         EXCEPTIONS error_message.

*    IF sy-subrc <> 0.
*      global_error = true.
*      PERFORM sy_msg_to_bapiret2 TABLES return
*                                 USING  0
*                                        'GOODSMVT_HEADER'.
*    ENDIF.
  ENDIF.

* -------------------------------------------------------------- MAA2

* --Outsourced Manufacturing------------------------------------n1768662

   DATA: lv_flag_om_active TYPE  abap_bool,
         lv_imseg_changed  TYPE  abap_bool,
         lv_iseri_changed  TYPE  abap_bool,
         lv_flag_om_error  TYPE  abap_bool,
         lt_imseg_om_out   TYPE  STANDARD TABLE OF imseg,
         lt_iseri_om_out   TYPE  STANDARD TABLE OF iseri,
         lt_emseg_om_out   TYPE  TABLE OF emseg.


   " Check whether the business function is active
   CALL METHOD cl_ops_switch_check=>mm_om1_sfws_sc
     RECEIVING
       rv_active = lv_flag_om_active.

   IF NOT lv_flag_om_active IS INITIAL
      AND t158g-tcode EQ 'MB01'.

     PERFORM om_item_duplicate TABLES    t_imseg
                                         t_iseri
                                         lt_imseg_om_out
                                         lt_iseri_om_out
                                         lt_emseg_om_out
                             CHANGING    lv_imseg_changed
                                         lv_iseri_changed
                                         lv_flag_om_error.

     IF lv_flag_om_error IS NOT INITIAL.
       MOVE:  '4' TO  s_emkpf-subrc.
       APPEND LINES OF lt_emseg_om_out TO t_emseg.
       RETURN.                                       " leave the routine
     ENDIF.

     " Items in IMSEG were changed
     IF lv_imseg_changed EQ abap_true.
       MOVE:  lt_imseg_om_out[] TO t_imseg[].
     ENDIF.
     " Serial numbers were changed
     IF lv_iseri_changed EQ abap_true.
       MOVE: lt_iseri_om_out[] TO t_iseri[].
       " Transfer to local memory of serial number management
       CALL FUNCTION 'DOCUMENT_SERIALNOS_IMPORT'
         TABLES
           ser_tab = t_iseri.
       " Initialize serial number tables
       CALL FUNCTION 'SERIALPROFILE_CHECK'
            EXPORTING
                 operation            = 'SNCL'
            EXCEPTIONS error_message.

     ENDIF.

     REFRESH: lt_imseg_om_out, lt_iseri_om_out, lt_emseg_om_out.
     CLEAR: lv_imseg_changed, lv_iseri_changed, lv_flag_om_error.

   ENDIF.

* --Outsourced Manufacturing------------------------------------n1768662

  IF f_testrun = space.
ENHANCEMENT-SECTION     CREATE_GOODS_MOVEMENT_01 SPOTS ES_SAPLMB_BUS2017.
      CALL FUNCTION 'MB_CREATE_GOODS_MOVEMENT'
           EXPORTING
                imkpf       = s_imkpf
                xallp       = x
                xallb       = x
                xallr       = x
                ctcod       = t158g-tcode
                xqmcl       = blank
                old_subrc   = old_subrc
                ipkcom      = s_ipkcom
*               x_authority = x     "new logic 4.7: perform check 402522
                xlisu       = x
           IMPORTING
                emkpf       = s_emkpf
           TABLES
                emseg       = t_emseg
                imseg       = t_imseg.
END-ENHANCEMENT-SECTION.
  ELSE.

      CALL FUNCTION 'MB_SIMULATE_GOODS_MOVEMENT'
           EXPORTING
               sim_imkpf    = s_imkpf
               ctcod        = t158g-tcode
               xqmcl        = blank
               old_subrc    = old_subrc
               ipkcom       = s_ipkcom
*              x_authority  = x     "new logic 4.7: perform check 402522
               xlisu        = x
           IMPORTING
               sim_emkpf    = s_emkpf
           TABLES
               sim_emseg     = t_emseg
               sim_imseg     = t_imseg.
  ENDIF.

  IF NOT s_emkpf_maa-subrc IS INITIAL.                   "MAA2
    MOVE s_emkpf_maa TO s_emkpf.                         "MAA2
    APPEND LINES OF lt_emseg_maa to t_emseg.             "MAA2
  ENDIF.                                                 "MAA2

ENDFORM.

*---------------------------------------------------------------------*
*      Form  MB_POST_GOODS_MOVEMENT                                   *
*---------------------------------------------------------------------*
*      Call Function for to post the material document.               *
*---------------------------------------------------------------------*
FORM mb_post_goods_movement TABLES    loc_return
                            STRUCTURE bapiret2
                            USING     loc_goodsmvt_headret
                            STRUCTURE bapi2017_gm_head_ret.
  CALL FUNCTION 'MB_POST_GOODS_MOVEMENT'
       IMPORTING
            emkpf       = s_emkpf.
  IF s_emkpf-subrc <> 0.
    global_error = true.
    PERFORM set_msg_to_bapiret2 TABLES loc_return
                                USING  s_emkpf-msgid
                                       s_emkpf-msgty
                                       s_emkpf-msgno
                                       s_emkpf-msgv1
                                       s_emkpf-msgv2
                                       s_emkpf-msgv3
                                       s_emkpf-msgv4
                                       'GOODSMVT_HEADER'
                                       0
                                       space.
  ELSE.
* map EMKPF to external structure *************************************
    CALL FUNCTION 'MAP2E_EMKPF_TO_BAPI2017_GM_HD'
         EXPORTING
              emkpf                = s_emkpf
         CHANGING
              bapi2017_gm_head_ret = loc_goodsmvt_headret.
  ENDIF.
ENDFORM.
