*&---------------------------------------------------------------------*
*& Report  ZFIR076
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir076.

DATA: lt_bkdf  TYPE TABLE OF bkdf,
      lt_bkpf  TYPE TABLE OF bkpf,
      wa_bkpf  TYPE bkpf,
      lt_bsec  TYPE TABLE OF bsec,
      wa_bseg  TYPE bseg,
      lt_bsed  TYPE TABLE OF bsed,
      lt_bseg  TYPE TABLE OF bseg,
      lt_bset  TYPE TABLE OF bset,
      msg_text TYPE string.



DATA: wa_ret_document TYPE zfie_ret_document,
      it_ret_document LIKE STANDARD TABLE OF wa_ret_document.

DATA: vg_job      TYPE i.

SELECT SINGLE COUNT( * ) INTO vg_job
  FROM tbtco
 WHERE jobname EQ 'ATRIBUICAO_LOTE_SIGAM'
   AND status EQ 'R'.

IF ( vg_job EQ 1 ).
  PERFORM f_seleciona_dados.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: it_zfit0155_aux TYPE TABLE OF zfit0155,
        tabix           TYPE sy-tabix,
        tabix2          TYPE sy-tabix,
        fg_alterou(1).

  SELECT *
    FROM zfit0155
    INTO TABLE @DATA(it_zfit0155)
    WHERE rg_atualizado = '0'
    ORDER BY lote.

  it_zfit0155_aux[] = it_zfit0155[].
  DELETE ADJACENT DUPLICATES FROM it_zfit0155_aux COMPARING lote.
  LOOP AT it_zfit0155_aux INTO DATA(w_aux) .
    tabix = sy-tabix.
    LOOP AT it_zfit0155 INTO DATA(wa_155) WHERE lote = w_aux-lote.
      tabix2 = sy-tabix.
* ---> S4 Migration - 15/06/2023 - MA
*      Não tem todos campos chave
      SELECT *
        FROM bseg
        INTO TABLE  @DATA(t_bseg_aux)
         WHERE bukrs = @wa_155-bukrs
         AND   belnr = @wa_155-belnr
         AND   bschl IN ( '31', '21','29','39' ). "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA


      IF sy-subrc NE 0.
        w_aux-rg_atualizado = 'N'.
        EXIT.
      ENDIF.
      SORT t_bseg_aux BY bschl.
      LOOP AT t_bseg_aux INTO DATA(w_bseg_aux).
        IF strlen( w_bseg_aux-zuonr ) LE 10.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF strlen( w_bseg_aux-zuonr ) GT 10.
        w_aux-rg_atualizado = 'N'.
        EXIT.
      ENDIF.

    ENDLOOP.
*    MODIFY IT_ZFIT0155_AUX FROM W_AUX INDEX TABIX TRANSPORTING RG_ATUALIZADO. "Não marca o lote como Não processar
  ENDLOOP.


  LOOP AT it_zfit0155 INTO DATA(wa_zfit0155).
    REFRESH: it_ret_document.
    CLEAR fg_alterou.
* ---> S4 Migration - 15/06/2023 - MA
*    Não tem todos campo chave
    SELECT *
       FROM bseg
       INTO TABLE  @DATA(t_bseg)
        WHERE bukrs = @wa_zfit0155-bukrs
        AND   belnr = @wa_zfit0155-belnr
        AND   bschl IN ( '31', '21','29','39' ). "#EC CI_DB_OPERATION_OK[2431747]

*<--- S4 Migration - 15/06/2023 - MA

    IF sy-subrc NE 0.
      CLEAR wa_ret_document.
      wa_ret_document-obj_key        = wa_zfit0155-lote.
      msg_text = 'Documento não existe ou não é fornec'.
      CONCATENATE msg_text '-' wa_zfit0155-belnr INTO msg_text .
      wa_ret_document-interface      = '59'.
      wa_ret_document-dt_atualizacao = sy-datum.
      wa_ret_document-hr_atualizacao = sy-uzeit.
      wa_ret_document-type           = 'E'.
      wa_ret_document-id             = 'Z01'.
      wa_ret_document-num            = '004'.
      wa_ret_document-message        = msg_text.
      wa_ret_document-message_v1     = wa_zfit0155-belnr.
      wa_ret_document-message_v2     = ''.
      wa_ret_document-message_v3     = ''.
      wa_ret_document-message_v4     = ''.
      fg_alterou = 'X'.
      APPEND wa_ret_document TO it_ret_document.
    ELSE.
      SORT t_bseg BY bschl.
      LOOP AT t_bseg INTO DATA(w_bseg).
        IF strlen( w_bseg-zuonr ) LE 10.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF strlen( w_bseg-zuonr ) GT 10.
        CLEAR wa_ret_document.
        wa_ret_document-obj_key        = wa_zfit0155-lote.
        msg_text = 'Documento já atribuido com lote'.
        CONCATENATE msg_text '-' wa_zfit0155-belnr INTO msg_text .
        wa_ret_document-interface      = '59'.
        wa_ret_document-dt_atualizacao = sy-datum.
        wa_ret_document-hr_atualizacao = sy-uzeit.
        wa_ret_document-type           = 'S'.
        wa_ret_document-id             = 'Z01'.
        wa_ret_document-num            = '004'.
        wa_ret_document-message        = msg_text.
        wa_ret_document-message_v1     = wa_zfit0155-belnr.
        wa_ret_document-message_v2     = ''.
        wa_ret_document-message_v3     = ''.
        wa_ret_document-message_v4     = ''.
        fg_alterou = 'X'.
        APPEND wa_ret_document TO it_ret_document.
      ELSE.
        READ TABLE it_zfit0155_aux INTO w_aux WITH KEY lote = wa_zfit0155-lote.
        IF w_aux-rg_atualizado NE 'N'.
          CLEAR wa_bkpf.
          REFRESH lt_bkpf.
          SELECT SINGLE *
            FROM bkpf
            INTO wa_bkpf
          WHERE bukrs = w_bseg-bukrs
          AND   belnr = w_bseg-belnr
          AND   gjahr = w_bseg-gjahr.
          APPEND wa_bkpf TO lt_bkpf.
          CLEAR wa_bseg.
          REFRESH  lt_bseg.
          MOVE-CORRESPONDING w_bseg TO wa_bseg.
          wa_bseg-zuonr = wa_zfit0155-lote.
          APPEND wa_bseg TO lt_bseg.

          CALL FUNCTION 'CHANGE_DOCUMENT'
            TABLES
              t_bkdf = lt_bkdf
              t_bkpf = lt_bkpf
              t_bsec = lt_bsec
              t_bsed = lt_bsed
              t_bseg = lt_bseg
              t_bset = lt_bset.

          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            CLEAR wa_ret_document.
            wa_ret_document-obj_key        = wa_zfit0155-lote.
            msg_text = 'Documento atribuido com sucesso'.
            CONCATENATE msg_text '-' wa_zfit0155-belnr INTO msg_text .
            wa_ret_document-interface      = '59'.
            wa_ret_document-dt_atualizacao = sy-datum.
            wa_ret_document-hr_atualizacao = sy-uzeit.
            wa_ret_document-type           = 'S'.
            wa_ret_document-id             = 'Z01'.
            wa_ret_document-num            = '004'.
            wa_ret_document-message        = msg_text.
            wa_ret_document-message_v1     = wa_zfit0155-belnr.
            wa_ret_document-message_v2     = ''.
            wa_ret_document-message_v3     = ''.
            wa_ret_document-message_v4     = ''.
            fg_alterou = 'X'.
            APPEND wa_ret_document TO it_ret_document.
          ELSE.
            CLEAR wa_ret_document.
            wa_ret_document-obj_key        = wa_zfit0155-lote.
            msg_text = 'Documento não atribuido'.
            CONCATENATE msg_text '-' wa_zfit0155-belnr INTO msg_text .
            wa_ret_document-interface      = '59'.
            wa_ret_document-dt_atualizacao = sy-datum.
            wa_ret_document-hr_atualizacao = sy-uzeit.
            wa_ret_document-type           = 'E'.
            wa_ret_document-id             = 'Z01'.
            wa_ret_document-num            = '004'.
            wa_ret_document-message        = msg_text.
            wa_ret_document-message_v1     = wa_zfit0155-belnr.
            wa_ret_document-message_v2     = ''.
            wa_ret_document-message_v3     = ''.
            wa_ret_document-message_v4     = ''.
            fg_alterou = 'X'.
            APPEND wa_ret_document TO it_ret_document.
          ENDIF.
        ELSE.
          CLEAR wa_ret_document.
          wa_ret_document-obj_key        = wa_zfit0155-lote.
          msg_text = 'Lote contem documento sem chave fornec'.
          CONCATENATE msg_text '-' wa_zfit0155-belnr INTO msg_text .
          wa_ret_document-interface      = '59'.
          wa_ret_document-dt_atualizacao = sy-datum.
          wa_ret_document-hr_atualizacao = sy-uzeit.
          wa_ret_document-type           = 'E'.
          wa_ret_document-id             = 'Z01'.
          wa_ret_document-num            = '004'.
          wa_ret_document-message        = msg_text.
          wa_ret_document-message_v1     = wa_zfit0155-belnr.
          wa_ret_document-message_v2     = ''.
          wa_ret_document-message_v3     = ''.
          wa_ret_document-message_v4     = ''.
          fg_alterou = 'X'.
          APPEND wa_ret_document TO it_ret_document.
        ENDIF.
      ENDIF.
    ENDIF.
    "
    IF fg_alterou = 'X'.
      UPDATE zfit0155 SET rg_atualizado = '1'
       WHERE bukrs = wa_zfit0155-bukrs
       AND   belnr = wa_zfit0155-belnr
       AND   gjahr = wa_zfit0155-gjahr
       AND   buzei = wa_zfit0155-buzei
       AND   lote  = wa_zfit0155-lote.
      COMMIT WORK.
    ENDIF.
    IF NOT it_ret_document IS INITIAL.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*      call function 'Z_FI_OUTBOUND_RETURN' in background task
*        destination 'XI_SIGAM_RETURN'
*        as separate unit
*        tables
*          OUTRETURN = IT_RET_DOCUMENT.

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
            outreturn = it_ret_document.
      ELSE.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          TABLES
            outreturn = it_ret_document.
      ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
      COMMIT WORK.
    ENDIF.


  ENDLOOP.




ENDFORM.
