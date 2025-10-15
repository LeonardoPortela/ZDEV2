*&---------------------------------------------------------------------*
*& Report  ZMBST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmbst.

TABLES: mkpf.

DATA: t_bdc     TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab TYPE TABLE OF bdcmsgcoll.

DATA:
  wa_zppt0006     TYPE zppt0006,
  wa_zppt0002     TYPE zppt0002,
  vid             TYPE zppt0006-id,
  v_bukrs         TYPE t001k-bukrs,
  t_t001w         TYPE TABLE OF t001w,
  w_t001w         TYPE t001w,
  return          TYPE TABLE OF bapiret2 WITH HEADER LINE,
  l_erro          TYPE c,
  it_saida        TYPE TABLE OF zreturndoc WITH HEADER LINE,
  it_outreturn    TYPE TABLE OF zfie_ret_document,
  wa_outreturn    TYPE zfie_ret_document,
  vg_obj_key      TYPE zmmt_ee_zgr-obj_key,
  vg_interface(2),
  e_status(1),
  e_messa(64).

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
    PARAMETERS: dt_estor LIKE mkpf-budat OBLIGATORY,
                dt_anodo LIKE mkpf-mjahr OBLIGATORY.
    SELECT-OPTIONS: nr_docum FOR mkpf-mblnr NO INTERVALS.
  SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.


START-OF-SELECTION.

  PERFORM estorna_documentos.

*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field  USING    VALUE(p_flag)
                           VALUE(p_fnam)
                           VALUE(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  ESTORNA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_carrega_permissao.

  FREE: t_t001w.

  SELECT *
    FROM zppt0027
    INTO TABLE @DATA(t_zppt0027)
   WHERE usuario = @sy-uname.

  IF t_zppt0027[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_t001w
      FROM t001w
       FOR ALL ENTRIES IN t_zppt0027
     WHERE werks >= t_zppt0027-werks_from
       AND werks <= t_zppt0027-werks_to.

    SORT t_t001w BY werks.
    DELETE ADJACENT DUPLICATES FROM t_t001w
                          COMPARING werks.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_verifica_permissao USING p_docnum TYPE mkpf-mblnr
                       CHANGING p_erro.

  FREE: p_erro,
        return,
        wa_zppt0002.

  SELECT SINGLE * " Verifica se é estorno do fardinho
    FROM zppt0002
    INTO wa_zppt0002
   WHERE mblnr EQ p_docnum
     AND status_registro NE '05'.

  IF sy-subrc = 0.
    READ TABLE t_t001w INTO w_t001w WITH KEY werks = wa_zppt0002-werks
                                    BINARY SEARCH.
    IF sy-subrc <> 0.
      p_erro         = abap_true.
      return-type    = 'E'.
      return-message = 'Usuário sem permissão para efetuar Estorno deste Processo!'.
      APPEND return.
      EXIT.
    ENDIF.
  ELSE.
    SELECT SINGLE * " Verifica se é estorno do fardão
      FROM zppt0002
      INTO wa_zppt0002
     WHERE mblnr02 EQ nr_docum-low
       AND status_registro NE '06'.

    IF sy-subrc = 0.
      READ TABLE t_t001w INTO w_t001w WITH KEY werks = wa_zppt0002-werks
                                      BINARY SEARCH.
      IF sy-subrc <> 0.
        p_erro         = abap_true.
        p_erro         = abap_true.
        return-type    = 'E'.
        return-message = 'Usuário sem permissão para efetuar Estorno deste Processo!'.
        APPEND return.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM estorna_documentos .

  DATA: vg_data                  TYPE c LENGTH 10,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret.
*       return                   TYPE TABLE OF bapiret2 WITH HEADER LINE.

  FREE it_saida.

  WRITE dt_estor TO vg_data.

  SORT nr_docum BY low DESCENDING.

  CLEAR e_status.
  LOOP AT nr_docum.
    SELECT SINGLE t001k~bukrs
      INTO v_bukrs
     FROM t001k
     INNER JOIN mseg
      ON  mseg~mblnr = nr_docum-low
      AND mseg~mjahr = dt_anodo
     WHERE t001k~bwkey = mseg~werks.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = v_bukrs
        i_data   = dt_estor
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
    ENDIF.

    IF  e_status = 'E'.
      MESSAGE e_messa TYPE 'I'.
    ENDIF.
    EXIT.
  ENDLOOP.

  IF e_status = 'E'.
    EXIT.
  ENDIF.

*-CS2021000713 - 15.10.2021 - JT - inicio
  PERFORM f_carrega_permissao.
*-CS2021000713 - 15.10.2021 - JT - fim

  LOOP AT nr_docum.

    FREE: return, l_erro.

*-CS2021000713 - 15.10.2021 - JT - inicio
    PERFORM f_verifica_permissao USING nr_docum-low
                              CHANGING l_erro.

    IF l_erro = abap_false.
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = nr_docum-low
          matdocumentyear     = dt_anodo
          goodsmvt_pstng_date = dt_estor
        IMPORTING
          goodsmvt_headret    = vg_invoicedocnumber_migo
        TABLES
          return              = return.
    ENDIF.
*-CS2021000713 - 15.10.2021 - JT - fim

    READ TABLE return WITH KEY type = 'E'.
    IF NOT sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      it_saida-type = 'S'.
      it_saida-mblnr = nr_docum-low.
      it_saida-message = |Doc Estornado: { vg_invoicedocnumber_migo-mat_doc }/{ vg_invoicedocnumber_migo-doc_year }|.
      APPEND it_saida.

      PERFORM f_remove_vinculo_zmm0023 USING vg_invoicedocnumber_migo.

      "TRACECOTTON
      vg_interface = '39'.
      REFRESH it_outreturn.
      SELECT  SINGLE * " Verifica se é estorno do fardinho
        FROM zppt0002
        INTO  wa_zppt0002
        WHERE mblnr = nr_docum-low
        AND   status_registro NE '05'.

      IF sy-subrc = 0.
        "
        UPDATE zppt0002 SET status_registro = '05' "Estorno Fardinho
        WHERE acharg = wa_zppt0002-acharg
        AND   werks  = wa_zppt0002-werks
        AND   status_registro NE '05'.


        CONCATENATE wa_zppt0002-acharg '|' wa_zppt0002-werks INTO vg_obj_key.
        wa_outreturn-obj_key        = vg_obj_key.
        wa_outreturn-interface      = vg_interface.
        wa_outreturn-id             = vg_interface.
        wa_outreturn-num            = vg_interface.
        CONCATENATE 'Estorno material(MBST) fardinho:' vg_invoicedocnumber_migo-mat_doc INTO wa_outreturn-message.
        wa_outreturn-message_v1     = '05'.
        wa_outreturn-message_v2     = ''.
        wa_outreturn-type           = 'S'.
        wa_outreturn-dt_atualizacao = sy-datum.
        wa_outreturn-hr_atualizacao = sy-uzeit.
        APPEND wa_outreturn TO it_outreturn.

        SELECT MAX( id ) INTO vid
          FROM zppt0006
        WHERE charg  = wa_zppt0002-charg
        AND   acharg = wa_zppt0002-acharg
        AND   werks  = wa_zppt0002-werks.
        "
        ADD 1 TO vid.
        wa_zppt0006-werks             = wa_zppt0002-werks.
        wa_zppt0006-charg             = wa_zppt0002-charg.
        wa_zppt0006-acharg            = wa_zppt0002-acharg.
        wa_zppt0006-id_fardinho       = wa_zppt0002-id_fardinho.
        wa_zppt0006-id_fardao         = wa_zppt0002-id_fardao.
        wa_zppt0006-id                = vid.
        wa_zppt0006-budat             = wa_zppt0002-budat.
        wa_zppt0006-matnr             = wa_zppt0002-matnr.
        wa_zppt0006-verid             = wa_zppt0002-verid.
        wa_zppt0006-lgort             = wa_zppt0002-lgort.
        wa_zppt0006-cd_classificacao  = wa_zppt0002-cd_classificacao.
        wa_zppt0006-msgnr             = '000'.
        wa_zppt0006-status_msg        = 'S'.
        wa_zppt0006-status_registro   = '05'.
        wa_zppt0006-cd_mensagem       = wa_outreturn-message.
        wa_zppt0006-data              = sy-datum.
        wa_zppt0006-hora              = sy-uzeit.
        wa_zppt0006-flag_envio        = 'W'.
        wa_zppt0006-id_cotton         = wa_zppt0002-id_cotton.
        MODIFY zppt0006 FROM wa_zppt0006.
        "
        SORT it_outreturn BY obj_key interface.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*        CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*          DESTINATION 'XI_SIGAM_RETURN'
*          TABLES
*            outreturn = it_outreturn.

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
* <--- S4 Migration - 28/08/2023 - JGP - Fim
        COMMIT WORK.

      ELSE.
        SELECT  SINGLE * " Verifica se é estorno do fardão
        FROM zppt0002
        INTO  wa_zppt0002
        WHERE mblnr02 = nr_docum-low
        AND   status_registro NE '06'.

        IF sy-subrc = 0.
          "
          UPDATE zppt0002 SET status_registro = '06' "Estorno fardão
          WHERE charg = wa_zppt0002-charg
          AND   werks  = wa_zppt0002-werks
          AND   status_registro NE '06'.
          "
          IF wa_zppt0002-id_cotton IS INITIAL.
            CONCATENATE wa_zppt0002-charg '|' wa_zppt0002-werks INTO vg_obj_key.
          ELSE.
            CONCATENATE wa_zppt0002-id_cotton '|' wa_zppt0002-werks INTO vg_obj_key.
          ENDIF.
          wa_outreturn-obj_key        = vg_obj_key.
          wa_outreturn-interface      = vg_interface.
          wa_outreturn-id             = vg_interface.
          wa_outreturn-num            = vg_interface.
          CONCATENATE 'Estorno material(MBST) fardão:' vg_invoicedocnumber_migo-mat_doc INTO wa_outreturn-message.
          wa_outreturn-message_v1     = '06'.
          wa_outreturn-message_v2     = ''.
          wa_outreturn-type           = 'S'.
          wa_outreturn-dt_atualizacao = sy-datum.
          wa_outreturn-hr_atualizacao = sy-uzeit.
          APPEND wa_outreturn TO it_outreturn.

          SELECT MAX( id ) INTO vid
          FROM zppt0006
          WHERE charg  = wa_zppt0002-charg
          AND   acharg = wa_zppt0002-charg
          AND   werks  = wa_zppt0002-werks.

          ADD 1 TO vid.
          wa_zppt0006-charg        = wa_zppt0002-charg.
          wa_zppt0006-acharg       = wa_zppt0002-charg.
          wa_zppt0006-id_fardao    = wa_zppt0002-id_fardao.
          wa_zppt0006-id_fardinho  = 0.
          wa_zppt0006-id           = vid.
          wa_zppt0006-budat        = wa_zppt0002-budat.
          wa_zppt0006-werks        = wa_zppt0002-werks.
          wa_zppt0006-matnr        = wa_zppt0002-matnr.
          wa_zppt0006-verid        = wa_zppt0002-verid.
          wa_zppt0006-msgnr        = '000'.
          wa_zppt0006-status_msg   = 'S'.
          wa_zppt0006-status_registro   = '06'.
          wa_zppt0006-cd_mensagem  = wa_outreturn-message.
          wa_zppt0006-data         = sy-datum.
          wa_zppt0006-hora         = sy-uzeit.

          wa_zppt0006-flag_envio  = 'W'.
          wa_zppt0006-id_cotton    = wa_zppt0002-id_cotton.
          MODIFY zppt0006 FROM wa_zppt0006.

          SORT it_outreturn BY obj_key interface.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*          CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*            DESTINATION 'XI_SIGAM_RETURN'
*            TABLES
*              outreturn = it_outreturn.

          CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

          CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
            EXPORTING
              i_fm          = cc_fm
            IMPORTING
              e_rfc         = lv_rfc
            EXCEPTIONS
              no_rfc        = 1
              no_rfc_config = 2
              OTHERS        = 3.

          IF sy-subrc EQ 0.
            CALL FUNCTION cc_fm IN BACKGROUND TASK
              DESTINATION lv_rfc
              AS SEPARATE UNIT
              TABLES
                outreturn = it_outreturn.
          ELSE.
            CALL FUNCTION cc_fm IN BACKGROUND TASK
              TABLES
                outreturn = it_outreturn.
          ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
          COMMIT WORK.

        ENDIF.

      ENDIF.
    ELSE.
      LOOP AT return.
        it_saida-type = return-type.
        it_saida-mblnr = nr_docum-low.
        it_saida-message = return-message.
        APPEND it_saida.
      ENDLOOP.

    ENDIF.
*    CLEAR: t_bdc[], t_messtab.
*
*    PERFORM f_bdc_field USING: 'X' 'SAPMM07M'      '0460',
*                               ' ' 'BDC_OKCODE'    '/00',
*                               ' ' 'MKPF-BUDAT'    vg_data, "23.01.2011
*                               ' ' 'RM07M-MBLNR'  nr_docum-low,
*                               ' ' 'RM07M-MJAHR'  dt_anodo,
*                               ' ' 'RM07M-GRUND'  '0001',
*                               ' ' 'RM07M-WVERS2'  'X'.
*
*    PERFORM f_bdc_field USING: 'X' 'SAPMM07M'      '0421',
*                               ' ' 'BDC_OKCODE'    '=BU'.
*
*    CALL TRANSACTION 'MBST' USING t_bdc MODE 'N' UPDATE 'S' MESSAGES INTO t_messtab.
*    BREAK-POINT.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'zreturndoc'
    TABLES
      t_outtab         = it_saida
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

ENDFORM.                    " ESTORNA_DOCUMENTOS

FORM f_remove_vinculo_zmm0023 USING p_doc_estorno TYPE bapi2017_gm_head_ret.

  delete from zmmt0008 WHERE mblnr = nr_docum-low
                         AND mjahr = dt_anodo.

ENDFORM.
