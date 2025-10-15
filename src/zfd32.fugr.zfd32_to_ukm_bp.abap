FUNCTION ZFD32_TO_UKM_BP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_BDCDATA) TYPE  BDCDATA_TAB
*"  EXPORTING
*"     REFERENCE(IT_MESSTAB) TYPE  SRCT_CTU_MESSTAB
*"----------------------------------------------------------------------

DATA : io_facade        TYPE REF TO cl_ukm_facade,
         io_calculator    TYPE REF TO if_ukm_calculator,
         io_partner       TYPE REF TO cl_ukm_business_partner,
         io_bupa_factory  TYPE REF TO cl_ukm_bupa_factory,
         io_account       TYPE REF TO cl_ukm_account,
         lw_bp_credit_sgm TYPE ukm_s_bp_cms_sgm,
         lw_bp_cms_sgm    TYPE ukm_s_bp_cms,
         lt_return        TYPE ukm_t_monitor_return,
         lt_tline         TYPE STANDARD TABLE OF tline,
         lt_text_upd      TYPE STANDARD TABLE OF tline.

  DATA: v_struct_name TYPE bdcdata-fnam,
        v_field_name  TYPE bdcdata-fnam,
        v_name        TYPE thead-tdname,
        v_id          TYPE thead-tdid,
        v_language    TYPE thead-tdspras,
        v_object      TYPE thead-tdobject.

  DATA: wa_line_upd TYPE tline,
        wa_header   TYPE thead,
        wa_messtab  TYPE bdcmsgcoll.

  FIELD-SYMBOLS <fs_struct> TYPE any.
  FIELD-SYMBOLS <fs_field>  TYPE any.

  DATA: i_partner       TYPE bu_partner,
        i_creditsegment TYPE ukm_credit_sgmnt.

  CONSTANTS: c_id(4)     TYPE c VALUE '0001',
             c_language  TYPE c VALUE 'P',
             c_object(6) TYPE c VALUE 'UKM_01',
             c_tcode(6)  type c value 'UKM_BP',
             c_s         type c value 'S'.

  READ TABLE it_bdcdata INTO DATA(wa_bdc) WITH KEY fnam = 'RF02L-KUNNR'. " Cliente
  IF sy-subrc EQ 0.
    i_partner       = wa_bdc-fval.
  ENDIF.

  READ TABLE it_bdcdata INTO wa_bdc WITH KEY fnam = 'RF02L-KKBER'.       " Area ctr cre
  IF sy-subrc EQ 0.
    i_creditsegment = wa_bdc-fval.
  ENDIF.

  io_facade       = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
  io_calculator   = io_facade->get_calculator( ).
  io_bupa_factory = io_facade->get_bupa_factory( ).

  io_partner = io_bupa_factory->get_business_partner( i_partner ).

  CALL METHOD io_bupa_factory->get_credit_account
    EXPORTING
      i_partner         = i_partner
      i_credit_sgmnt    = i_creditsegment
    RECEIVING
      ro_credit_account = io_account.

  io_account->get_bp_cms_sgm( IMPORTING es_bp_cms_sgm = lw_bp_credit_sgm ).

  io_account->ao_business_partner->get_bp_cms( IMPORTING es_bp_cms = lw_bp_cms_sgm ).

  LOOP AT it_bdcdata INTO wa_bdc.
    CLEAR v_struct_name.
    CLEAR v_field_name.

    SPLIT wa_bdc-fnam AT '-' INTO v_struct_name v_field_name.

    CASE v_field_name.

      WHEN 'KLIMK'.
        IF wa_bdc-fval CS ','.
          TRANSLATE wa_bdc-fval USING '. '.
          TRANSLATE wa_bdc-fval USING ',.'.
          CONDENSE  wa_bdc-fval NO-GAPS.
        ENDIF.
        lw_bp_credit_sgm-credit_limit   = wa_bdc-fval. " Limite Cred
        lw_bp_credit_sgm-limit_chg_date = sy-datum.    " Data modificação limite crédito
      WHEN 'CTLPC'.
        lw_bp_cms_sgm-risk_class        = wa_bdc-fval. " Classe de Risco
      WHEN 'CRBLB'.
        lw_bp_credit_sgm-xblocked       = wa_bdc-fval. " Bloqueio em SAP Credit Management
      WHEN 'GRUPP'.
        lw_bp_credit_sgm-cust_group     = wa_bdc-fval. " Grupo de crédito do cliente
    ENDCASE.

    IF v_field_name CS 'TXLINE'.
      DATA(lv_fill_text) = abap_true.
    ENDIF.

  ENDLOOP.

* lw_bp_credit_sgm-xcritical = 'X'.
* lw_bp_cms_sgm-limit_rule   =
* lw_bp_cms_sgm-own_rating   =
* lw_bp_cms_sgm-check_rule   =
* lw_bp_cms_sgm-credit_group =


  io_account->set_bp_cms_sgm( EXPORTING is_bp_cms_sgm = lw_bp_credit_sgm ).

  io_account->ao_business_partner->set_bp_cms( EXPORTING is_bp_cms = lw_bp_cms_sgm ).

  io_bupa_factory->save_all( EXPORTING i_upd_task = abap_true RECEIVING et_return = lt_return ).

  IF lt_return[] IS  INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

      wa_messtab-tcode     =  c_tcode.
      wa_messtab-msgspra   =  sy-langu.
      wa_messtab-msgtyp    =  c_s.
      wa_messtab-msgid     =  'Z01'.
      wa_messtab-msgnr     =  '001'.
      wa_messtab-msgv1     =  'Segmento de crédito alterado com sucesso.'.
      wa_messtab-msgv2     =  i_partner .
      wa_messtab-msgv3     =  i_creditsegment.
      APPEND wa_messtab TO it_messtab.
  ELSE.
    LOOP AT lt_return INTO DATA(wa_return).
      wa_messtab-tcode     =  c_tcode.
      wa_messtab-msgspra   =  sy-langu.
      wa_messtab-msgtyp    =  wa_return-msgty.
      wa_messtab-msgid     =  wa_return-msgid.
      wa_messtab-msgnr     =  wa_return-msgno.
      wa_messtab-msgv1     =  wa_return-msgv1.
      wa_messtab-msgv2     =  wa_return-msgv2.
      wa_messtab-msgv3     =  wa_return-msgv3.
      wa_messtab-msgv4     =  wa_return-msgv4.
      APPEND wa_messtab TO it_messtab.
    ENDLOOP.
  ENDIF.

** Altera textos

  DATA(it_bdcdata_aux) = it_bdcdata[].

  DELETE it_bdcdata_aux WHERE fnam NS 'RSTXT-TXLINE'.

  IF lv_fill_text EQ abap_true.

    v_name      = i_partner.
    v_id        = c_id.
    v_language  = c_language.
    v_object    = c_object .

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = v_id
        language                = v_language
        name                    = v_name
        object                  = v_object
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    wa_header-tdid      = v_id.
    wa_header-tdname    = v_name.
    wa_header-tdobject  = v_object.
    wa_header-tdspras   = v_language.

    LOOP AT it_bdcdata_aux  INTO wa_bdc.
      wa_line_upd-tdline = wa_bdc-fval.
      APPEND  wa_line_upd TO lt_text_upd.
    ENDLOOP.

    LOOP AT lt_tline INTO wa_line_upd.
      APPEND  wa_line_upd TO lt_text_upd.
    ENDLOOP.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = wa_header
        savemode_direct = 'X'
      TABLES
        lines           = lt_text_upd
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

  ENDIF.

ENDFUNCTION.
