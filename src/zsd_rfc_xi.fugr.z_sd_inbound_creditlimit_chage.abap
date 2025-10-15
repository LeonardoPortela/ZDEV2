FUNCTION z_sd_inbound_creditlimit_chage.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_LIMITE_CREDITO STRUCTURE  ZSD_CREDITLIMIT
*"----------------------------------------------------------------------

  DATA: it_outreturn      TYPE TABLE OF zfie_ret_document.

  DATA: wa_limite_credito LIKE zsd_creditlimit,
        wa_outreturn      TYPE zfie_ret_document,
        i_knka            TYPE knka,          "#EC CI_USAGE_OK[2227014]
        i_knkk            TYPE knkk,          "#EC CI_USAGE_OK[2227014]
        upd_knka          TYPE cdpos-chngind,
        upd_knkk          TYPE cdpos-chngind,
        yknka             TYPE knka,          "#EC CI_USAGE_OK[2227014]
        yknkk             TYPE knkk.          "#EC CI_USAGE_OK[2227014]

  DATA: vl_area_limite TYPE knkk-kkber,       "#EC CI_USAGE_OK[2227014]
        vl_mensagem    TYPE zfie_ret_document-message.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_knkk       TYPE STANDARD TABLE OF knkk, "#EC CI_USAGE_OK[2227014]
        lt_data_where TYPE STANDARD TABLE OF zknkk_key,
        wa_data_where TYPE zknkk_key.
* <--- S4 Migration - 18/07/2023 - CA

  "21-08-2023 - PROJ HANA AJuste Limite de credito - BUG #120167 - BG INICIO
  DATA:
    lo_facade       TYPE REF TO cl_ukm_facade,
    l_dummy,
    ls_segment      TYPE ukm_credit_sgmnt,
    lo_bupa_factory TYPE REF TO cl_ukm_bupa_factory,
    lo_customizing  TYPE REF TO cl_ukm_customizing,
    lo_account      TYPE REF TO cl_ukm_account,
    lt_vector_it    TYPE ukm_t_vector_it.

  DATA:
    l_credit_sgmnt TYPE ukm_credit_sgmnt,
    l_currency     TYPE ukm_sgm_currency,
    lo_partner     TYPE REF TO cl_ukm_business_partner,
    ls_bp_cms_sgm  TYPE ukm_s_bp_cms_sgm.

  DATA:     io_bupa_factory    TYPE REF TO cl_ukm_bupa_factory.

  "21-08-2023 - PROJ HANA AJuste Limite de credito - BUG #120167 - BG FIM

  SELECT SINGLE valor
    INTO vl_area_limite
    FROM zparametros
   WHERE nome_parametro = 'AREA_CONTROLE_CREDITO'.

  LOOP AT it_limite_credito INTO wa_limite_credito.
*   control area data
* ---> S4 Migration - 18/07/2023 - CA
*    SELECT SINGLE *
*      FROM KNKK
*      INTO I_KNKK
*     WHERE KUNNR = WA_LIMITE_CREDITO-KUNNR
*       AND KKBER = VL_AREA_LIMITE.

    CLEAR: wa_data_where.
    FREE: lt_data_where, lt_knkk.

    wa_data_where-kunnr = wa_limite_credito-kunnr.
    wa_data_where-kkber = vl_area_limite.
    APPEND wa_data_where TO lt_data_where.


    CALL FUNCTION 'Z_FROM_TO_KNKK'
      TABLES
        t_data_where = lt_data_where
        t_knkk       = lt_knkk.

    READ TABLE lt_knkk INTO DATA(wa_knkk) INDEX 1.

* <--- S4 Migration - 18/07/2023 - CA
    IF sy-subrc EQ 0.
      upd_knkk = 'U'.
* ---> S4 Migration - 18/07/2023 - CA
      MOVE-CORRESPONDING wa_knkk TO i_knkk.
* <--- S4 Migration - 18/07/2023 - CA
    ELSE.
      upd_knkk = 'I'.
    ENDIF.

    i_knkk-mandt = sy-mandt.
    i_knkk-kunnr = wa_limite_credito-kunnr.
    i_knkk-kkber = vl_area_limite.
    i_knkk-klimk = wa_limite_credito-klimk.

*   central data
    SELECT SINGLE *
      FROM knka                               "#EC CI_USAGE_OK[2227014]
      INTO i_knka
     WHERE kunnr = wa_limite_credito-kunnr.

    IF sy-subrc EQ 0.
      upd_knka = 'U'.
    ELSE.
      upd_knka = 'I'.
    ENDIF.

    i_knka-mandt = sy-mandt.
    i_knka-kunnr = wa_limite_credito-kunnr.
    i_knka-klimg = wa_limite_credito-klimk.
    i_knka-klime = wa_limite_credito-klimk.
    i_knka-waers = wa_limite_credito-waers.
    i_knka-dlaus = wa_limite_credito-dlaus.

    "21-08-2023 - PROJ HANA AJuste Limite de credito - BUG #120167 - BG INICIO

*    CALL FUNCTION 'CREDITLIMIT_CHANGE'
*      EXPORTING
*        i_knka   = i_knka
*        i_knkk   = i_knkk
*        upd_knka = upd_knka
*        upd_knkk = upd_knkk  "I = new rec, U otherwise
*        xneua    = ' '
*        xrefl    = ' '
*        yknka    = yknka
*        yknkk    = yknkk.

    SELECT SINGLE businesspartner FROM ibupacustomer INTO @DATA(v_codbp)
      WHERE customer EQ @i_knka-kunnr.
    l_credit_sgmnt = 'MAGI'.

* instantiate facade and factory
    lo_facade = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>update_credit_vector ).
    lo_bupa_factory = lo_facade->get_bupa_factory( ).

    lo_customizing = cl_ukm_customizing=>create( i_activity = cl_ukm_cnst_eventing=>update_credit_vector ).

    ls_segment = lo_customizing->if_ukm_customizing~get_credit_sgmnt( l_credit_sgmnt ).
    lo_account = lo_bupa_factory->get_credit_account( i_partner      = v_codbp
                                                      i_credit_sgmnt = l_credit_sgmnt ).

    lo_account->get_bp_cms_sgm(
      EXPORTING
        i_check_validity = abap_false
      IMPORTING
        es_bp_cms_sgm    = ls_bp_cms_sgm ).

    ls_bp_cms_sgm-credit_limit = wa_limite_credito-klimk.
    ls_bp_cms_sgm-limit_chg_date = sy-datum.
    ls_bp_cms_sgm-limit_valid_date = i_knka-dlaus.

    CALL METHOD lo_account->set_bp_cms_sgm
      EXPORTING
        i_is_authorized = 'X'
        is_bp_cms_sgm   = ls_bp_cms_sgm.

    lo_facade->save_all( ).

    "21-08-2023 - PROJ HANA AJuste Limite de credito - BUG #120167 - BG FIM

    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.

      CONCATENATE 'Limite do cliente' wa_limite_credito-kunnr 'atualizado com sucesso.' INTO vl_mensagem SEPARATED BY space.
      wa_outreturn-obj_key        = wa_limite_credito-ch_referencia.
      wa_outreturn-interface      = '38'.
      wa_outreturn-dt_atualizacao = sy-datum.
      wa_outreturn-hr_atualizacao = sy-uzeit.
      wa_outreturn-type           = 'S'.
      wa_outreturn-id             = 'SD'.
      wa_outreturn-num            = '38'.
      wa_outreturn-message        = vl_mensagem.

      APPEND wa_outreturn TO it_outreturn.

    ENDIF.

    CLEAR : i_knka, i_knkk, upd_knka , upd_knkk, yknka,yknkk, vl_mensagem, wa_outreturn.

  ENDLOOP.


  IF NOT it_outreturn[] IS INITIAL.
    SORT it_outreturn BY obj_key interface.

*--> 26.09.2023 18:00:50 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_outreturn.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
*<-- 26.09.2023 18:00:50 - Migração S4 – ML – Fim

    COMMIT WORK.

  ENDIF.



ENDFUNCTION.
