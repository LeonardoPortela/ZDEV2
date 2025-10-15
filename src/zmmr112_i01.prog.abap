*----------------------------------------------------------------------*
***INCLUDE ZMMR112_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: TL_ROWS     TYPE LVC_T_ROW,
        SL_ROWS     TYPE LVC_S_ROW,
        WG_ZMMT0069 TYPE ZMMT0069,
        V_LINES     TYPE I,
        W_ANSWER(1),
        V_MATNR     TYPE MARA-MATNR.

  CASE OK-CODE.
    WHEN 'ANALI'.
      CALL METHOD CL_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TL_ROWS.
      V_LINES = LINES( TL_ROWS ).

      IF V_LINES NE 1.
        MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
        EXIT.
      ENDIF.

      READ TABLE TL_ROWS INTO SL_ROWS INDEX 1.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX SL_ROWS-INDEX.
      SELECT SINGLE * FROM ZMMT0069 INTO WG_ZMMT0069 WHERE NUREQ = WA_SAIDA-NUREQ.


      IF WG_ZMMT0069-MATNRG IS NOT INITIAL AND WG_ZMMT0069-GER_WF = 'X' .
        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I02 .
        EXIT.
      ENDIF.

*      IF WG_ZMMT0069-RECUSA_FLAG IS NOT INITIAL.
*        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I03 .
*        EXIT.
*      ENDIF.

      IF WG_ZMMT0069-ELIMINADO IS NOT INITIAL.
        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I01 .
        EXIT.
      ENDIF.

      CLEAR: WG_ACAO, OK-CODE.
      CALL SCREEN 0200.
      PERFORM F_BUSCA_DADOS.
      REFRESH: TG_MSG_RET.
      CLEAR WG_MENSAGEM.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  DATA: WA_ZMMT0069 TYPE ZMMT0069.
  DATA: WA_ZMMT0085 TYPE ZMMT0085.

  CASE OK-CODE.
    WHEN 'RECUSA'.
      CLEAR V_MATNR .
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        PERFORM F_RECUSA.
      ELSE.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN    = '200'
            I_SHOW      = C_X
            I_REPID     = SY-REPID
            I_POPUP     = 0
            I_SET_FIELD = 'X_FIELD'
          IMPORTING
            E_MESSAGEM  = WG_MENSAGEM
          TABLES
            IT_MSGS     = TG_MSG_RET.
      ENDIF.
    WHEN 'HOMOLOG'.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        PERFORM F_HOMOLOG.
      ELSE.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN    = '200'
            I_SHOW      = C_X
            I_REPID     = SY-REPID
            I_POPUP     = 0
            I_SET_FIELD = 'X_FIELD'
          IMPORTING
            E_MESSAGEM  = WG_MENSAGEM
          TABLES
            IT_MSGS     = TG_MSG_RET.
      ENDIF.

    WHEN 'WORKF'.
      CLEAR V_MATNR .
      CHECK WG_GER_WF IS INITIAL.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        IF WG_CADMAT-MATNRG IS INITIAL.
          PERFORM F_MM01 USING WA_SAIDA CHANGING V_MATNR.
          IF V_MATNR IS NOT INITIAL.
            UPDATE ZMMT0069 SET MATNR        = V_MATNR
                                MATNRG       = V_MATNR
                                CRIADO       = SY-UNAME
                                DT_CRIACAO   = SY-DATUM
                                HR_CRIACAO   = SY-UZEIT
                WHERE NUREQ = WG_CADMAT-NUREQ.
          ENDIF.

          CLEAR: WA_ZMMT0069.

          SELECT SINGLE *
            FROM ZMMT0069
            INTO WA_ZMMT0069
            WHERE NUREQ = WG_CADMAT-NUREQ.

          IF SY-SUBRC IS INITIAL.
            WG_CADMAT-CRIADO     = WA_ZMMT0069-CRIADO.
            WG_CADMAT-DT_CRIACAO = WA_ZMMT0069-DT_CRIACAO.
            WG_CADMAT-HR_CRIACAO = WA_ZMMT0069-HR_CRIACAO.
          ENDIF.

          "LOG
          MOVE-CORRESPONDING WA_ZMMT0069 TO WL_LOG.
          WL_LOG-DATA = SY-DATUM.
          WL_LOG-HORA = SY-UZEIT.
          WL_LOG-ACAO = 'Gerado codigo de material'.
          MODIFY ZMMT0069_LOG FROM       WL_LOG.
          COMMIT WORK.

        ELSE.
          V_MATNR = WG_CADMAT-MATNRG.
        ENDIF.

        WG_CADMAT-MATNRG =  V_MATNR.

        IF V_MATNR IS NOT INITIAL.
          IF WG_CADMAT-CODAGREPI IS NOT INITIAL.
            WA_ZMMT0085-CODAGREPI = WG_CADMAT-CODAGREPI.
            WA_ZMMT0085-MATNR     = V_MATNR.
            WA_ZMMT0085-DATA      = SY-DATUM.
            WA_ZMMT0085-HORA      = SY-UZEIT.
            WA_ZMMT0085-USUARIO   = SY-UNAME.
            MODIFY ZMMT0085 FROM WA_ZMMT0085.
            COMMIT WORK.
          ENDIF.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD WG_CADMAT-NUREQ.
          SUBMIT ZWFRMM001 WITH S_MATNR = V_MATNR
                           WITH S_WERKS = WG_CADMAT-WERKS
                           WITH S_VKORG = WG_CADMAT-VKORG
                           WITH S_VTWEG = WG_CADMAT-VTWEG

          AND RETURN.
          CLEAR  W_ANSWER.
          GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD  W_ANSWER.
          IF  W_ANSWER = 'N'. "Sem Erros
            UPDATE ZMMT0069 SET GER_WF = 'X'
                WHERE NUREQ = WG_CADMAT-NUREQ.
            WG_GER_WF = 'X'.
            PERFORM ENVIA_EMAIL USING 'P'.
            "LOG
            WAIT UP TO 1 SECONDS.
            MOVE-CORRESPONDING WA_ZMMT0069 TO WL_LOG.
            WL_LOG-DATA = SY-DATUM.
            WL_LOG-HORA = SY-UZEIT.
            WL_LOG-ACAO = 'Disparado Workflow'.
            MODIFY ZMMT0069_LOG FROM       WL_LOG.
            COMMIT WORK.
          ENDIF.

        ELSE.
          MESSAGE 'Erro ao criar o material' TYPE 'I'.
          EXIT.
        ENDIF.
      ELSE.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN    = '200'
            I_SHOW      = C_X
            I_REPID     = SY-REPID
            I_POPUP     = 0
            I_SET_FIELD = 'X_FIELD'
          IMPORTING
            E_MESSAGEM  = WG_MENSAGEM
          TABLES
            IT_MSGS     = TG_MSG_RET.
      ENDIF.
    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN    = '200'
          I_SHOW      = C_X
          I_REPID     = SY-REPID
          I_POPUP     = 0
          I_SET_FIELD = 'X_FIELD'
        IMPORTING
          E_MESSAGEM  = WG_MENSAGEM
        TABLES
          IT_MSGS     = TG_MSG_RET.

    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PESQUISA_TEXTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PESQUISA_TEXTO INPUT.
  DATA: V_MAKTX TYPE MAKT-MAKTX.

  DATA: TL_RETURN_TAB2 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC2      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_MAKT OCCURS 0,
          MATNR TYPE MAKT-MATNR,
          MAKTX TYPE MAKT-MAKTX,
        END OF TL_MAKT.


  CONCATENATE WG_CADMAT-MAKTX '%' INTO V_MAKTX.

  REFRESH TL_MAKT.

  SELECT  MATNR MAKTX
    FROM MAKT
    INTO TABLE TL_MAKT
    WHERE SPRAS = SY-LANGU
    AND   MAKTX LIKE V_MAKTX.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'MAKTX'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADMAT-MAKTX'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_MAKT
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOSTRA_DES_GRUPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOSTRA_DES_GRUPO INPUT.
  SELECT SINGLE DENAGREPI
           FROM ZHRST_MED_AG_EPI
           INTO WG_CADMAT-DENAGREPI
          WHERE CODAGREPI = WG_CADMAT-CODAGREPI.
ENDMODULE.
