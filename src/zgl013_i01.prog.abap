
*&SPWIZARD: INPUT MODULE FOR TS 'TS_100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE TS_100_ACTIVE_TAB_GET INPUT..

  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_TS_100-TAB1.
      G_TS_100-PRESSED_TAB = C_TS_100-TAB1.
    WHEN C_TS_100-TAB2.
      G_TS_100-PRESSED_TAB = C_TS_100-TAB2.

    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TS_100_ACTIVE_TAB_GET INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA W_ANSWER.
  DATA:VG_OP_MODE          TYPE CHAR10.
  DATA  CL_ZGL032.


  CASE OK-CODE.
    WHEN 'VISAO'.
      IF X_VISAO = 'X'.
        CLEAR X_VISAO.
        BTN_VISAO = '@KU@ Visão de Razão'.
      ELSE.
        IF WG_ZGLT031-BUKRS IS INITIAL.
          MESSAGE TEXT-I01  TYPE 'I'.
          EXIT.
        ENDIF.
        X_VISAO = 'X'.
        BTN_VISAO = '@KU@ Visão de Entrada'.
      ENDIF.
    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
*         TITLEBAR                    = ' '
         TEXT_QUESTION               = TEXT-Q01
         TEXT_BUTTON_1               = 'Sim'(001)
         ICON_BUTTON_1               = 'ICON_OKAY'
         TEXT_BUTTON_2               = 'Não'(002)
         ICON_BUTTON_2               = 'ICON_CANCEL'
         DEFAULT_BUTTON              = '1'
         DISPLAY_CANCEL_BUTTON       = ' '
         START_COLUMN                = 25
         START_ROW                   = 6
      IMPORTING
        ANSWER                      = W_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND              = 1
        OTHERS                      = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF W_ANSWER = '1'.
        PERFORM F_ELIMINAR_LANCAMENTO.
      ENDIF.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_SAVE.
*      DELETE tg_zglt032 WHERE bschl IS INITIAL AND hkont IS INITIAL.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.

        PERFORM:  F_OBTEM_PROXIMO,
                  F_GRAVA_DADOS.

        PERFORM LOGS.

        REFRESH TG_FIELDS.

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E35.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
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
                                      'GR1'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

    WHEN C_ADD.
      CHECK WG_ACAO <> C_ADD.

      WG_ACAO = C_ADD.  "c_modif.

      PERFORM:  F_LIMPA_CAMPOS.
*                f_obtem_proximo.

      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 0.

      SELECT SINGLE BNAME
      FROM USER_ADDR
      INTO VG_BNAME
      WHERE BNAME      = SY-UNAME
      AND   DEPARTMENT = 'Amaggi Holanda'.
      IF SY-SUBRC = 0.
        WG_ZGLT031-BUKRS = '0201'.
      ENDIF.

    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 1.
      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
          EXPORTING
            READONLY_MODE = 0.
      ENDIF.


    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.
      "IF tg_msg_ret[] IS NOT INITIAL.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
      "ENDIF.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
*    WHEN c_back.
*      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DEPTO INPUT.
  DATA: TL_RETURN_TAB2 TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
             TL_DSELC2      TYPE TABLE OF DSELC      WITH HEADER LINE.
  DATA: BEGIN OF TL_DEPTO OCCURS 0,
            DEP_RESP  TYPE ZIMP_CAD_DEPTO-DEP_RESP,
            DEP_RESP_DESC  TYPE ZIMP_CAD_DEPTO-DEP_RESP_DESC,
            BUKRS TYPE ZIMP_CAD_DEPTO-BUKRS,
            GSBER TYPE ZIMP_CAD_DEPTO-GSBER,
           END OF TL_DEPTO.

  SELECT DEP_RESP DEP_RESP_DESC BUKRS GSBER
    FROM ZIMP_CAD_DEPTO
    INTO TABLE TL_DEPTO
    WHERE BUKRS = WG_ZGLT031-BUKRS.

  IF TL_DEPTO[] IS INITIAL.
    SELECT DEP_RESP DEP_RESP_DESC BUKRS GSBER
      FROM ZIMP_CAD_DEPTO
      INTO TABLE TL_DEPTO.
  ENDIF.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DEP_RESP'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZIMP_CAD_DEPTO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_DEPTO
      RETURN_TAB      = TL_RETURN_TAB2
      DYNPFLD_MAPPING = TL_DSELC2.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
