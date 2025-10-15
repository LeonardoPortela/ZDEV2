*&---------------------------------------------------------------------*
*&  Include           ZMMR111_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: WG_ZMMT0069 TYPE ZMMT0069,
        W_ANSWER.

  CASE OK-CODE.
    WHEN 'REENVIAR'.
      IF WG_ACAO = C_MODIF.
        IF WG_CADMAT-EXPANSAO EQ 'X'.
          MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I09 .
          EXIT.
        ENDIF.
        SELECT SINGLE * FROM ZMMT0069 INTO WG_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.
        IF WG_ZMMT0069-RECUSA_FLAG IS INITIAL.
          MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I07 .
          EXIT.
        ENDIF.

        CLEAR WG_CADMAT-RECUSA_FLAG.

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 0.

      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I08 .
        EXIT.
      ENDIF.
    WHEN 'WORKF'.
      IF WG_ACAO = C_MODIF.
        IF WG_CADMAT-EXPANSAO NE 'X' OR WG_CADMAT-MATNR IS INITIAL.
          MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I05 .
          EXIT.
        ENDIF.
        PERFORM F_VERIFICA_ERROS.
        IF TG_MSG_RET[] IS INITIAL.
          CLEAR WG_ACAO.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD WG_CADMAT-NUREQ.
          SUBMIT ZWFRMM001 WITH S_MATNR = WG_CADMAT-MATNR
                           WITH S_WERKS = WG_CADMAT-WERKS
                           WITH S_VKORG = WG_CADMAT-VKORG
                           WITH S_VTWEG = WG_CADMAT-VTWEG

          AND RETURN.
          CLEAR  W_ANSWER.
          GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD  W_ANSWER.
          IF  W_ANSWER = 'N'. "Sem Erros
            UPDATE ZMMT0069 SET MATNRG = WG_CADMAT-MATNR
                                GER_WF = 'X'
               WHERE NUREQ = WG_CADMAT-NUREQ.
            WG_CADMAT-MATNRG = WG_CADMAT-MATNR.
            "
            SELECT SINGLE * FROM ZMMT0069 INTO WG_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.
            MOVE-CORRESPONDING WG_ZMMT0069 TO WL_LOG.
            WL_LOG-DATA = SY-DATUM.
            WL_LOG-HORA = SY-UZEIT.
            WL_LOG-ACAO = 'iniciado Workflow'.
            MODIFY ZMMT0069_LOG FROM       WL_LOG.
            COMMIT WORK.
          ENDIF.
        ELSE.
          MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E59.
          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              I_SCREEN      = '100'
              I_SHOW        = SPACE   "c_x
              I_REPID       = SY-REPID
              I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
              I_SET_FIELD   = 'X_FIELD'
            IMPORTING
              E_MESSAGEM    = WG_MENSAGEM
            TABLES
              IT_MSGS       = TG_MSG_RET.
        ENDIF.
      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH TEXT-I06 .
        EXIT.
      ENDIF.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_DELDOC.
      SELECT SINGLE * FROM ZMMT0069 INTO WG_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.
      IF WG_ZMMT0069-MATNRG IS NOT INITIAL.
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

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
          TEXT_QUESTION         = TEXT-P01
          TEXT_BUTTON_1         = TEXT-P02 "'Sim'(001)
          ICON_BUTTON_1         = 'ICON_OKAY'
          TEXT_BUTTON_2         = TEXT-P03 "'Não'(002)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
          START_COLUMN          = 25
          START_ROW             = 6
        IMPORTING
          ANSWER                = W_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF W_ANSWER = '1'.
        PERFORM F_ELIMINAR_LANCAMENTO.
      ENDIF.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM F_LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

    WHEN C_SAVE.
      IF WG_CADMAT-MBBEZ IS INITIAL.
        MESSAGE TEXT-I10  TYPE 'I'.
        EXIT.
      ENDIF.
      IF WG_CADMAT-ELIMINADO IS NOT INITIAL.
        MESSAGE TEXT-I01  TYPE 'I'.
        EXIT.
      ENDIF.

      IF WG_CADMAT-MATNRG IS NOT INITIAL.
        MESSAGE TEXT-I02  TYPE 'I'.
        EXIT.
      ENDIF.

      IF WG_CADMAT-RECUSA_FLAG IS NOT INITIAL.
        MESSAGE TEXT-I03  TYPE 'I'.
        EXIT.
      ENDIF.

      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        IF WG_CADMAT-NUREQ IS INITIAL.
          PERFORM  F_OBTEM_PROXIMO.
        ENDIF.
        PERFORM F_GRAVA_DADOS.

        REFRESH TG_FIELDS.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0


        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0


        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E59.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = SPACE   "c_x
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.


    WHEN C_ADD.
*      CHECK WG_ACAO <> C_ADD.
      WG_ACAO = C_ADD.  "c_modif.
      PERFORM:  F_LIMPA_CAMPOS.

      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0


      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR3'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR4'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 0.

    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_CADMAT-ELIMINADO IS NOT INITIAL.
        MESSAGE TEXT-I01  TYPE 'I'.
        EXIT.
      ENDIF.

      IF WG_CADMAT-MATNRG IS NOT INITIAL.
        MESSAGE TEXT-I02  TYPE 'I'.
        EXIT.
      ENDIF.

      IF WG_CADMAT-RECUSA_FLAG IS NOT INITIAL.
        MESSAGE TEXT-I03  TYPE 'I'.
        EXIT.
      ENDIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR3'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.

      ELSE.
        WG_ACAO = C_MODIF.
        REFRESH: TG_FIELDS.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        IF WG_CADMAT-EXPANSAO = 'X'.
          PERFORM F_TRATA_CAMPOS USING  SPACE
                                        'GR3'
                                        C_1       "INPUT 1     NO INPUT 0
                                        C_0.      "INVISIBLE 1 VISIBLE 0
        ELSE.
          PERFORM F_TRATA_CAMPOS USING  SPACE
                                         'GR3'
                                         C_0       "INPUT 1     NO INPUT 0
                                         C_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR4'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 0.

      ENDIF.

      CLEAR: OK-CODE.
    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN    = '100'
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
*&      Module  M_MUDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_MUDA INPUT.
  REFRESH TG_FIELDS.
  PERFORM F_TRATA_CAMPOS USING  SPACE
                                 'GR1'
                                 C_0       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
  IF WG_CADMAT-EXPANSAO = 'X'.
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR3'
                                  C_1       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR4'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
  ELSE.
    CLEAR WG_CADMAT-MATNR.
    PERFORM F_TRATA_CAMPOS USING  SPACE
                              'GR3'
                              C_0       "INPUT 1     NO INPUT 0
                              C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM F_TRATA_CAMPOS USING  SPACE
                                  'GR4'
                                  C_1       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DOC INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_DOCS OCCURS 0,
          NUREQ      TYPE ZMMT0069-NUREQ,
          WERKS      TYPE ZMMT0069-WERKS,
          VKORG      TYPE ZMMT0069-VKORG,
          VTWEG      TYPE ZMMT0069-VTWEG,
          EXPANSAO   TYPE ZMMT0069-EXPANSAO,
          MATNR      TYPE ZMMT0069-MATNR,
          MAKTX      TYPE ZMMT0069-MAKTX,
          MATKL      TYPE ZMMT0069-MATKL,
          MEINS      TYPE ZMMT0069-MEINS,
          DT_ENTRADA TYPE ZMMT0069-DT_CRIACAO,
          HR_ENTRADA TYPE ZMMT0069-HR_CRIACAO,
          SOLIC      TYPE ZMMT0069-SOLIC,
        END OF TL_DOCS.


  SELECT *
     FROM ZMMT0069
     INTO CORRESPONDING FIELDS OF TABLE TL_DOCS
    WHERE SOLIC = SY-UNAME
    AND   MATNRG = ''
    ORDER BY NUREQ.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'NUREQ'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WG_CADMAT-NUREQ'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DOCS
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
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
