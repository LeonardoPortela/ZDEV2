*&---------------------------------------------------------------------*
*&  Include           ZGL014_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: W_ANSWER,
        WA_ZGLT035  TYPE ZGLT035.
  CASE OK-CODE.
    WHEN 'LANC'.
      CLEAR VG_LOTE.
      GET PARAMETER ID 'LOT' FIELD VG_LOTE.
      IF VG_LOTE NE WG_ZGLT034-LOTE OR WG_ZGLT034-LOTE IS INITIAL.
        MESSAGE TEXT-I03 TYPE 'I'.
      ELSE.
        CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'BTN'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          TITLE             = 'Empresa'
          TEXT              = 'Informe'
        TABLES
          RANGE             = P_BUKRS
        EXCEPTIONS
          NO_RANGE_TAB      = 1
          CANCELLED         = 2
          INTERNAL_ERROR    = 3
          INVALID_FIELDNAME = 4
          OTHERS            = 5.
      IF P_BUKRS[] IS NOT INITIAL.
        READ TABLE   P_BUKRS INDEX 1.
        WG_ZGLT034-BUKRS = P_BUKRS-LOW.
      ENDIF.
    WHEN 'DELE'.
      IF WG_ZGLT034-LOTE IS INITIAL.
        EXIT.
      ENDIF.

      IF WG_ZGLT034-USNAM NE SY-UNAME.
*        MESSAGE 'Permitido excluir somente por usuario de criação do lote!' TYPE 'I'.
        MESSAGE TEXT-I01 TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM ZGLT035
       INTO WA_ZGLT035
       WHERE LOTE =  WG_ZGLT034-LOTE.

      IF SY-SUBRC EQ 0.
        MESSAGE TEXT-I02 TYPE 'I'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
*           TITLEBAR                    = ' '
*           DIAGNOSE_OBJECT             = ' '
       TEXT_QUESTION               = TEXT-P01"'“Deseja marcar para eliminação sim ou não?'
       TEXT_BUTTON_1               = TEXT-P02 "'Sim'(100)
       ICON_BUTTON_1               = 'ICON_OKAY '
       TEXT_BUTTON_2               = TEXT-P03"'Não'(101)
       ICON_BUTTON_2               = 'ICON_CANCEL'
       DEFAULT_BUTTON              = '1'
       DISPLAY_CANCEL_BUTTON       = ' '
*           USERDEFINED_F1_HELP         = ' '
       START_COLUMN                = 25
       START_ROW                   = 6
*           POPUP_TYPE                  =
*           IV_QUICKINFO_BUTTON_1       = ' '
*           IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER                      = W_ANSWER
*         TABLES
*           PARAMETER                   =
    EXCEPTIONS
      TEXT_NOT_FOUND              = 1
      OTHERS                      = 2.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        DELETE FROM ZGLT034 WHERE LOTE = WG_ZGLT034-LOTE.
        CLEAR WG_ZGLT034.
      ENDIF.

    WHEN C_SEARCH.
      WG_ACAO = C_SEARCH.
      PERFORM F_BUSCA_DADOS CHANGING WG_ZGLT034-LOTE.

    WHEN C_ENTER.
      IF WG_ZGLT034-BUKRS IS NOT INITIAL.
        SELECT SINGLE DEP_RESP
        FROM ZIMP_CAD_DEPTO
        INTO WG_ZGLT034-DEP_RESP
        WHERE BUKRS = WG_ZGLT034-BUKRS.
      ENDIF.

      CHECK WG_ACAO = C_SEARCH.
      PERFORM F_BUSCA_DADOS CHANGING WG_ZGLT034-LOTE.

    WHEN C_SAVE.
      CLEAR WL_ERRO .
      PERFORM F_VERIFICA_ERROS.
      IF WL_ERRO NE 'X'.
        CLEAR WG_ACAO.
        PERFORM F_GRAVA_DADOS.
        "PERFORM F_BUSCA_DADOS CHANGING WG_ZGLT034-LOTE.
        WG_ACAO = C_SEARCH.
        REFRESH TG_FIELDS.

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR2'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'GR1'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
      ENDIF.
    WHEN C_ADD.
      WG_ACAO = C_MODIF.
      PERFORM:  F_LIMPA_CAMPOS.
*                F_OBTEM_PROXIMO. "GERAR O NUMERO NA HORA DE CRIAR O LOTE
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      SELECT SINGLE BNAME
      FROM USER_ADDR
      INTO VG_BNAME
      WHERE BNAME      = SY-UNAME
      AND   DEPARTMENT = 'Amaggi Holanda'.
      IF SY-SUBRC = 0.
        WG_ZGLT034-BUKRS = '0201'.
      ENDIF.
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
      ENDIF.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_EXIT.
      LEAVE PROGRAM.
*    WHEN c_show_msgre.
**      PERFORM f_verifica_erros.
*      IF tg_msg_ret[] IS NOT INITIAL.
*        CALL FUNCTION 'Z_DOC_CHECK_NEW'
*          EXPORTING
*            i_screen      = '100'
*            i_show        = c_x
*            i_repid       = sy-repid
*            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
*            i_set_field   = 'X_FIELD'
*          IMPORTING
*            e_messagem    = wg_mensagem
*          TABLES
*            it_msgs       = tg_msg_ret.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DEPTO INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
           TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.
  DATA: BEGIN OF TL_DEPTO OCCURS 0,
            DEP_RESP  TYPE ZIMP_CAD_DEPTO-DEP_RESP,
            DEP_RESP_DESC  TYPE ZIMP_CAD_DEPTO-DEP_RESP_DESC,
            BUKRS TYPE ZIMP_CAD_DEPTO-BUKRS,
            GSBER TYPE ZIMP_CAD_DEPTO-GSBER,
           END OF TL_DEPTO.

  SELECT DEP_RESP DEP_RESP_DESC BUKRS GSBER
    FROM ZIMP_CAD_DEPTO
    INTO TABLE TL_DEPTO
    WHERE BUKRS = WG_ZGLT034-BUKRS.

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
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
