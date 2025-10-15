FUNCTION znfw_processa_seq_lcto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SEQ_LCTO) TYPE  ZFIWRT0008-SEQ_LCTO
*"----------------------------------------------------------------------

  DATA: tg_0008     TYPE TABLE OF  zfiwrt0008,
        wg_0008     TYPE  zfiwrt0008,
        tg_zib_cont TYPE TABLE OF zib_contabil,
        wl_zib_cont TYPE zib_contabil,
        tg_zib_err  TYPE TABLE OF zib_contabil_err,
        wl_zib_err  TYPE  zib_contabil_err,
        wg_0001     TYPE zfiwrt0001,
        wg_0026     TYPE zfiwrt0026.

  DATA: wl_cont(1)  TYPE i,
        wl_cont_aux.


  SELECT SINGLE *
    FROM zfiwrt0008 INTO wg_0008
     WHERE seq_lcto EQ i_seq_lcto.

  IF sy-subrc = 0.

    SELECT SINGLE  *  FROM zfiwrt0001 INTO wg_0001
        WHERE operacao EQ wg_0008-operacao.

    SELECT SINGLE * FROM zfiwrt0026 INTO wg_0026
      WHERE ( usname EQ wg_0008-usnam OR
              usname EQ wg_0008-usuario_ult_mod ).

    IF sy-subrc NE 0 .
      IF wg_0001-lm_aprova NE 'N'.
        IF wg_0008-budat <> sy-datum AND wg_0001-complemento NE 'S'.
          IF ( wg_0008-tcode_org EQ 'ZNFW0009' AND wg_0008-form EQ 'NF90' ).
            "Então não precisa passar pela validação da função. Aceitar as datas como foram enviadas pela ZNFW0009
          ELSE.
            MESSAGE 'Data de Documento no passado, para prosseguir alterar para data do dia.' TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    SELECT  SINGLE *
      FROM  zib_contabil INTO wl_zib_cont
        WHERE obj_key EQ wg_0008-obj_key.


    CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
      EXPORTING
        seq_lcto       = i_seq_lcto
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF wg_0008-loekz IS INITIAL.
        IF wg_0008-obj_key IS INITIAL.
          CONCATENATE 'ZG0' i_seq_lcto sy-datum+0(4) INTO wg_0008-obj_key.
        ELSE.

          SELECT  SINGLE *  FROM  zib_contabil INTO wl_zib_cont
              WHERE obj_key EQ wg_0008-obj_key.

          IF wl_zib_cont-rg_atualizado EQ 'S'.

            SELECT SINGLE *
              FROM zib_contabil_err  INTO wl_zib_err
               WHERE obj_key EQ wg_0008-obj_key
               AND   type    EQ 'E'.

            IF sy-subrc = 0.
              wl_cont = wg_0008-obj_key+2(1).
              ADD 1 TO wl_cont.
              wl_cont_aux =  wl_cont.
              CONCATENATE 'ZG' wl_cont_aux i_seq_lcto sy-datum+0(4) INTO wg_0008-obj_key.
            ENDIF.
          ENDIF.
        ENDIF.
        MOVE: 'A' TO wg_0008-status.

        CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
          EXPORTING
            seq_lcto = i_seq_lcto.

        MODIFY zfiwrt0008 FROM wg_0008.
      ELSE.

      ENDIF.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = i_seq_lcto.
    ENDIF.

  ELSE.
    MESSAGE 'Lançamento informado não existe.' TYPE 'E'.
    EXIT.
  ENDIF.

ENDFUNCTION.
