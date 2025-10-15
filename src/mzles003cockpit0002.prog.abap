*----------------------------------------------------------------------*
***INCLUDE MZLES003COCKPIT0002 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.

  DATA: BEGIN OF WA_ENQ.
          INCLUDE STRUCTURE SEQG7.
  DATA: END OF WA_ENQ.

  DATA: GARG LIKE SEQG3-GARG,
        ENQ  LIKE STANDARD TABLE OF WA_ENQ.

  CASE OK_CODE.
    WHEN OK_TABLOTE.
      PERFORM VERIFICA_ALTERACOES USING VG_SELECIONOU.
      IF NOT VG_SELECIONOU IS INITIAL.
        PERFORM DISMARCA_GRID_CONF.
        VG_TELA_0002               = C_0003.
        TAB_CONTROL_0002-ACTIVETAB = OK_TABLOTE.
        IF WA_LOTES_ALV IS NOT INITIAL.
          CALL FUNCTION 'Z_PFE_DEQUEUE'
            EXPORTING
              NM_LOTE = WA_LOTES_ALV-NM_LOTE.
        ENDIF.
      ENDIF.
      CLEAR OK_CODE.
    WHEN OK_TABOPER.
      PERFORM DISMARCA_GRID_CONF.
      PERFORM SELECIONA_LOTE USING VG_SELECIONOU.
      CHECK NOT VG_SELECIONOU IS INITIAL.
      PERFORM VERIFICA_BLOQUEIO USING VG_SELECIONOU.

      IF NOT VG_SELECIONOU IS INITIAL.
        VG_TELA_0002 = C_1001.
        TAB_CONTROL_0002-ACTIVETAB = OK_TABOPER.
      ENDIF.
      CLEAR OK_CODE.

    WHEN OK_TABCONFER.

      PERFORM DISMARCA_GRID_CONF.
      PERFORM SELECIONA_LOTE USING VG_SELECIONOU.
      CHECK NOT VG_SELECIONOU IS INITIAL.
      PERFORM VERIFICA_BLOQUEIO USING VG_SELECIONOU.

      IF NOT VG_SELECIONOU IS INITIAL.
        VG_TELA_0002 = C_1002.
        TAB_CONTROL_0002-ACTIVETAB = OK_TABCONFER.
      ENDIF.


    WHEN OK_TABCONFER_ADM.
      PERFORM DISMARCA_GRID_CONF.
      PERFORM SELECIONA_LOTE USING VG_SELECIONOU.
      CHECK NOT VG_SELECIONOU IS INITIAL.
*      PERFORM VERIFICA_BLOQUEIO USING VG_SELECIONOU.

      IF NOT VG_SELECIONOU IS INITIAL.
        VG_TELA_0002 = C_1003.
        TAB_CONTROL_0002-ACTIVETAB = OK_TABCONFER_ADM.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0002 OUTPUT.

  IF VG_TELA_0002 IS INITIAL.
    VG_TELA_0002 = C_0003.
  ENDIF.

ENDMODULE.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_BLOQUEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_SELECIONOU  text
*----------------------------------------------------------------------*
FORM VERIFICA_BLOQUEIO  USING  P_VG_SELECIONOU TYPE C.

  IF TAB_CONTROL_0002-ACTIVETAB EQ OK_TABLOTE.
    CALL FUNCTION 'Z_PFE_ENQUEUE'
      EXPORTING
        NM_LOTE        = WA_LOTES_ALV-NM_LOTE
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.

    CASE SY-SUBRC.
      WHEN 1.
        CONCATENATE SY-MANDT WA_LOTES_ALV-NM_LOTE INTO GARG.

        CALL FUNCTION 'ENQUE_READ2'
          EXPORTING
            GNAME  = 'ZPFE_LOTE'
            GARG   = GARG
            GUNAME = '*'
          TABLES
            ENQ    = ENQ.

        READ TABLE ENQ INTO WA_ENQ WITH KEY GNAME = 'ZPFE_LOTE'.
        MESSAGE S029 WITH WA_LOTES_ALV-NM_LOTE WA_ENQ-GUNAME.
        CLEAR: P_VG_SELECIONOU.
      WHEN 2.
        MESSAGE S030 WITH 'Erro em bloqueio!'.
        CLEAR: P_VG_SELECIONOU.
    ENDCASE.
  ENDIF.

ENDFORM.                    " VERIFICA_BLOQUEIO

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ALTERACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_SELECIONOU  text
*----------------------------------------------------------------------*
FORM VERIFICA_ALTERACOES  USING P_VG_SELECIONOU TYPE C.

  DATA: VG_RESPOSTA.

  P_VG_SELECIONOU = C_X.

  IF NOT VG_ALTEROU_CONF IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = TEXT-T01
        TEXT_QUESTION         = TEXT-T02
        TEXT_BUTTON_1         = TEXT-T03
        ICON_BUTTON_1         = 'ICON_SYSTEM_SAVE'
        TEXT_BUTTON_2         = TEXT-T04
        ICON_BUTTON_2         = 'ICON_SYSTEM_UNDO'
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = 'X'
      IMPORTING
        ANSWER                = VG_RESPOSTA
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    CASE VG_RESPOSTA.
      WHEN '1'.
        PERFORM GRAVAR_CONFERENCIAS.
      WHEN '2'.
        CLEAR: VG_ALTEROU_CONF.
      WHEN OTHERS.
        CLEAR: P_VG_SELECIONOU.
    ENDCASE.

  ENDIF.

ENDFORM.                    " VERIFICA_ALTERACOES
*&---------------------------------------------------------------------*
*&      Form  ARQUIVO_CONFERENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARQUIVO_CONFERENCIA .
  DATA: WL_CAMINHO_INPUT TYPE STRING,
        WL_CAMINHO_AUX TYPE STRING,
        TL_FILENAME TYPE TABLE OF SDOKPATH WITH HEADER LINE.
  REFRESH: TG_0062.

  CALL FUNCTION 'Z_BUSCA_PARAMETROS'
    EXPORTING
      I_NOME_PARAMETRO    = 'INPUT_CONF_ADM'
    IMPORTING
      E_CAMINHO           = WL_CAMINHO_INPUT
    EXCEPTIONS
      PARAMETRO_NOT_FOUND = 1
      OTHERS              = 2.

  IF SY-SUBRC IS INITIAL.
    CALL FUNCTION 'Z_PFE_EXIBE_ARQUIVOS'
      EXPORTING
        I_CAMINHO           = WL_CAMINHO_INPUT
      TABLES
        TE_FILENAME         = TL_FILENAME
      EXCEPTIONS
        DIRETORIO_NOT_FOUND = 1
        OTHERS              = 2.

    IF SY-SUBRC EQ 0.

      LOOP AT TL_FILENAME.
        CONCATENATE WL_CAMINHO_INPUT TL_FILENAME-PATHNAME
         INTO WL_CAMINHO_AUX SEPARATED BY '/'.
        PERFORM IMPORTA_CONFERENCIA USING WL_CAMINHO_AUX
                                          TL_FILENAME-PATHNAME.
      ENDLOOP.
      IF TL_FILENAME[] IS INITIAL.
*        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não a arquivos para serem importados'.
      ENDIF.

      CALL FUNCTION 'Z_PFE_LOG_PROC_ARQ'
        EXPORTING
          I_POPUP  = '1'
          I_SEARCH = ' '
        TABLES
          IT_LOG   = TG_0062.

    ENDIF.
  ENDIF.

ENDFORM.                    " ARQUIVO_CONFERENCIA
*&---------------------------------------------------------------------*
*&      Form  IMPORTA_CONFERENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_CAMINHO_AUX  text
*----------------------------------------------------------------------*
FORM IMPORTA_CONFERENCIA  USING    P_CAMINHO_AUX
                                   P_PATHNAME.
  TYPES: BEGIN OF TYL_LINES_ARQ,
        LINE(1000),
       END OF TYL_LINES_ARQ,

       BEGIN OF TYL_ZPFE_LOTE_ITEM.
          INCLUDE STRUCTURE ZPFE_LOTE_ITEM.
  TYPES:  INDEX TYPE SY-INDEX,
         END OF TYL_ZPFE_LOTE_ITEM.

  DATA: WL_CAMINHO_LOG TYPE STRING,
        WL_CAMINHO_PROC TYPE STRING,
        WL_CAMINHO_INPUT TYPE STRING,
        WL_TABIX          TYPE SY-TABIX,
        WL_SUCESSO,
        WL_RC TYPE SY-SUBRC,
        WL_CONT TYPE SY-TABIX,
        WL_VLR(20),
        WL_DATA(8),
        WL_CHVID_ADM(10),
        TL_ZPFE_LOTE_ITEM   TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        TL_ZPFE_LOTE_ITEM_C TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        TL_ZCTE_CIOT        TYPE TABLE OF ZCTE_CIOT WITH HEADER LINE,
        TL_ITENS            TYPE TABLE OF ZPFE_LOTE_ITEM WITH HEADER LINE,
        TL_LOTE             TYPE TABLE OF ZPFE_LOTE WITH HEADER LINE,
        TL_ZLEST0025        TYPE TABLE OF ZLEST0025 WITH HEADER LINE,
        WL_0025             TYPE ZLEST0025,
        WL_ZPFE_LOTE_HIS_DE TYPE  ZPFE_LOTE_HIS_DE,
        TL_LINES_ARQ        TYPE STANDARD TABLE OF TYL_LINES_ARQ WITH HEADER LINE,
        TL_0062             TYPE TABLE OF ZLEST0062 WITH HEADER LINE .



  DATA: BEGIN OF TL_LINES OCCURS 0,
        LINE(500),
       END OF TL_LINES .

  WL_CAMINHO_INPUT = P_CAMINHO_AUX.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = WL_CAMINHO_INPUT
      FILETYPE                = 'ASC'
*    IMPORTING
*      FILELENGTH              = V_SIZE
    CHANGING
      DATA_TAB                = TL_LINES_ARQ[]
    EXCEPTIONS
      FILE_READ_ERROR         = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      GUI_REFUSE_FILETRANSFER = 7
      OTHERS                  = 99.

  IF SY-SUBRC IS INITIAL.
    LOOP AT TL_LINES_ARQ.
      SPLIT TL_LINES_ARQ-LINE AT '|' INTO TABLE TL_LINES.
      READ TABLE TL_LINES INDEX 1.

      CASE TL_LINES-LINE.
        WHEN '1'.
          READ TABLE TL_LINES INDEX 4.
          CONDENSE TL_LINES-LINE NO-GAPS.
*          WRITE TL_LINES-LINE TO TL_ZPFE_LOTE_ITEM-DT_CONF_ADM.
          CONCATENATE TL_LINES-LINE+4(4) TL_LINES-LINE+2(2) TL_LINES-LINE(2) INTO WL_DATA.
          TL_ZPFE_LOTE_ITEM-DT_CONF_ADM = WL_DATA.

          READ TABLE TL_LINES INDEX 7.
          CONDENSE TL_LINES-LINE NO-GAPS.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = TL_LINES-LINE
            IMPORTING
              OUTPUT = TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM.

*          TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM = TL_LINES-LINE.

        WHEN '2'.
          READ TABLE TL_LINES INDEX 12.
          CONDENSE TL_LINES-LINE NO-GAPS.
          IF TL_LINES-LINE EQ 'A'.
            CLEAR: TL_LINES.
            READ TABLE TL_LINES INDEX 13.
            CONDENSE TL_LINES-LINE NO-GAPS.
            IF TL_LINES-LINE IS INITIAL.
**          Inclusao de nova linha na tabela e lote itens
              TL_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM = TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM.
              READ TABLE TL_LINES INDEX 8.
              CONDENSE TL_LINES-LINE NO-GAPS.
              TL_ZPFE_LOTE_ITEM_C-NR_CIOT = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 2.
              CONDENSE TL_LINES-LINE NO-GAPS.
              TL_ZPFE_LOTE_ITEM_C-NUCONTRATO = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 5.
              CONDENSE TL_LINES-LINE NO-GAPS.
              TL_ZPFE_LOTE_ITEM_C-CTENUM = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 3.
              CONDENSE TL_LINES-LINE NO-GAPS.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = TL_LINES-LINE
                IMPORTING
                  OUTPUT = WL_CHVID_ADM.

              SELECT SINGLE * INTO WL_ZPFE_LOTE_HIS_DE
                FROM ZPFE_LOTE_HIS_DE
               WHERE CHVID_ADM EQ WL_CHVID_ADM.

              IF SY-SUBRC IS INITIAL.
                TL_ZPFE_LOTE_ITEM_C-CHVID = WL_ZPFE_LOTE_HIS_DE-CHVID_EMP.
              ENDIF.

              READ TABLE TL_LINES INDEX 4.
              CONDENSE TL_LINES-LINE NO-GAPS.
              CONCATENATE TL_LINES-LINE+4(4) TL_LINES-LINE+2(2) TL_LINES-LINE(2) INTO  TL_ZPFE_LOTE_ITEM_C-DT_TRANSACAO.
*              TL_ZPFE_LOTE_ITEM_C-DT_TRANSACAO = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 9.
              CONDENSE TL_LINES-LINE NO-GAPS.
              TL_ZPFE_LOTE_ITEM_C-PESO_ORIGEM = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 10.
              CONDENSE TL_LINES-LINE NO-GAPS.
              TL_ZPFE_LOTE_ITEM_C-PESO_CHEGADA = TL_ZPFE_LOTE_ITEM_C-PESO_CONF_ADM = TL_LINES-LINE.

              READ TABLE TL_LINES INDEX 11.
              CONDENSE TL_LINES-LINE NO-GAPS.
              CONCATENATE TL_LINES-LINE+4(4) TL_LINES-LINE+2(2) TL_LINES-LINE(2) INTO  TL_ZPFE_LOTE_ITEM_C-DT_CHEGADA.
*              TL_ZPFE_LOTE_ITEM_C-DT_CHEGADA = TL_LINES-LINE.

              TL_ZPFE_LOTE_ITEM_C-DT_BAIXA = TL_ZPFE_LOTE_ITEM_C-DT_CONF_ADM = TL_ZPFE_LOTE_ITEM-DT_CONF_ADM.

              READ TABLE TL_LINES INDEX 7.
              CONDENSE TL_LINES-LINE NO-GAPS.
              IF NOT TL_LINES-LINE IS INITIAL.
                WL_CONT = STRLEN( TL_LINES-LINE ).
                SUBTRACT 2 FROM WL_CONT.
                CONCATENATE TL_LINES-LINE(WL_CONT) '.' TL_LINES-LINE+WL_CONT INTO TL_LINES-LINE.
              ELSE.
                TL_LINES-LINE  = '0.00'.
              ENDIF.

              TL_ZPFE_LOTE_ITEM_C-VL_CONF_ADM = TL_ZPFE_LOTE_ITEM_C-VL_AJUS_ADM = TL_LINES-LINE.

              SELECT SINGLE *
                  FROM ZLEST0025
                  INTO WL_0025
                   WHERE CHVID EQ TL_ZPFE_LOTE_ITEM_C-CHVID.
              IF WL_0025-NATUREZACHVID EQ 'S'.
                MULTIPLY TL_ZPFE_LOTE_ITEM_C-VL_AJUS_ADM BY -1.
              ENDIF.
              TL_ZPFE_LOTE_ITEM_C-CK_CONF_ADM = 'X'.

              APPEND TL_ZPFE_LOTE_ITEM_C.
            ENDIF.
          ELSE.
            READ TABLE TL_LINES INDEX 2.
            CONDENSE TL_LINES-LINE NO-GAPS.
            TL_ZPFE_LOTE_ITEM-NUCONTRATO = TL_LINES-LINE.

            READ TABLE TL_LINES INDEX 3.
            CONDENSE TL_LINES-LINE NO-GAPS.
            SHIFT TL_LINES-LINE LEFT DELETING LEADING '0'.
*          tl_zpfe_lote_item-chvid = tl_lines-line.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = TL_LINES-LINE
              IMPORTING
                OUTPUT = WL_CHVID_ADM.

            SELECT SINGLE * INTO WL_ZPFE_LOTE_HIS_DE
              FROM ZPFE_LOTE_HIS_DE
             WHERE CHVID_ADM EQ WL_CHVID_ADM.

            IF SY-SUBRC IS INITIAL.
              TL_ZPFE_LOTE_ITEM-CHVID = WL_ZPFE_LOTE_HIS_DE-CHVID_EMP.
            ENDIF.

            READ TABLE TL_LINES INDEX 7.
            CONDENSE TL_LINES-LINE NO-GAPS.
*          WRITE: TL_LINES-LINE TO TL_ZPFE_LOTE_ITEM-VL_CONF_ADM.
            CLEAR: WL_CONT.
            WL_CONT = STRLEN( TL_LINES-LINE ).
            IF WL_CONT GT 2.
              SUBTRACT 2 FROM WL_CONT.
              CONCATENATE TL_LINES-LINE(WL_CONT) '.' TL_LINES-LINE+WL_CONT(2) INTO WL_VLR.
              TL_ZPFE_LOTE_ITEM-VL_CONF_ADM = WL_VLR.
            ENDIF.

            READ TABLE TL_LINES INDEX 10.
            CONDENSE TL_LINES-LINE NO-GAPS.
            TL_ZPFE_LOTE_ITEM-PESO_CONF_ADM = TL_LINES-LINE.
*          WRITE: TL_LINES-LINE TO TL_ZPFE_LOTE_ITEM-VL_CONF_ADM.
**          CLEAR WL_CONT.
**          WL_CONT = STRLEN( TL_LINES-LINE ).
**          IF WL_CONT GT 3.
**            SUBTRACT 3 FROM WL_CONT.
*            CONCATENATE TL_LINES-LINE(WL_CONT) '.' TL_LINES-LINE+WL_CONT(3) INTO WL_VLR.
*            TL_ZPFE_LOTE_ITEM-PESO_CONF_ADM = WL_VLR.
*          ENDIF.

            APPEND TL_ZPFE_LOTE_ITEM.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    IF TL_ZPFE_LOTE_ITEM_C[] IS NOT INITIAL.
      SELECT *
        FROM ZPFE_LOTE
        INTO TABLE TL_LOTE
         FOR ALL ENTRIES IN TL_ZPFE_LOTE_ITEM_C
          WHERE NR_LOTE_ADM EQ TL_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
            AND TPLOTE      EQ 'S'.

      READ TABLE TL_LOTE INDEX 1.

      SELECT *
        FROM ZPFE_LOTE_ITEM
        INTO TABLE TL_ITENS
         FOR ALL ENTRIES IN TL_ZPFE_LOTE_ITEM_C
         WHERE NR_LOTE_ADM EQ TL_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
           AND NM_LOTE     EQ TL_LOTE-NM_LOTE.

      SELECT *
        FROM ZCTE_CIOT
        INTO TABLE TL_ZCTE_CIOT
         FOR ALL ENTRIES IN TL_ZPFE_LOTE_ITEM_C
          WHERE NUCONTRATO EQ TL_ZPFE_LOTE_ITEM_C-NUCONTRATO.


      SORT TL_ITENS BY NM_LOTE_ITEM DESCENDING.
      LOOP AT TL_ZPFE_LOTE_ITEM_C.
        CLEAR: TL_ITENS.
        READ TABLE TL_ITENS
          WITH KEY NR_LOTE_ADM = TL_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM.

        READ TABLE TL_ZCTE_CIOT
          WITH KEY NUCONTRATO = TL_ZPFE_LOTE_ITEM_C-NUCONTRATO.

        TL_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM = TL_ITENS-NM_LOTE_ITEM + 1.
        TL_ZPFE_LOTE_ITEM_C-NM_LOTE      = TL_ITENS-NM_LOTE.
        TL_ZPFE_LOTE_ITEM_C-CD_CIOT      = TL_ZCTE_CIOT-CD_CIOT.
        TL_ZPFE_LOTE_ITEM_C-DOCNUM       = TL_ZCTE_CIOT-DOCNUM.
        TL_ZPFE_LOTE_ITEM_C-TKNUM        = TL_ZCTE_CIOT-TKNUM.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = TL_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM
          IMPORTING
            OUTPUT = TL_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM.


        WL_SUCESSO = 'X'.
        PERFORM INSERT_LINE_LOG USING TL_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
                                      TL_ZPFE_LOTE_ITEM_C-NM_LOTE
                                      TL_ZPFE_LOTE_ITEM_C-CHVID
                                      TL_ZPFE_LOTE_ITEM_C-NUCONTRATO
                                      'C'
                                      TEXT-S01
                                      SPACE
                                      'S'.
        MODIFY ZPFE_LOTE_ITEM FROM TL_ZPFE_LOTE_ITEM_C.
      ENDLOOP.
      REFRESH: TL_ITENS.
    ENDIF.

    IF TL_ZPFE_LOTE_ITEM[] IS NOT INITIAL.


      SELECT *
        FROM ZPFE_LOTE_ITEM
        INTO TABLE TL_ITENS
         FOR ALL ENTRIES IN TL_ZPFE_LOTE_ITEM
         WHERE NR_LOTE_ADM EQ TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM
           AND NUCONTRATO  EQ TL_ZPFE_LOTE_ITEM-NUCONTRATO
           AND CHVID       EQ TL_ZPFE_LOTE_ITEM-CHVID.

      IF TL_ITENS[] IS NOT INITIAL.
        SELECT *
          FROM ZLEST0025
          INTO TABLE TL_ZLEST0025
           FOR ALL ENTRIES IN TL_ITENS
            WHERE CHVID EQ TL_ITENS-CHVID.
      ENDIF.

      LOOP AT TL_ZPFE_LOTE_ITEM.
        WL_TABIX = SY-TABIX.
        ADD 1 TO WL_TABIX.
        READ TABLE TL_LINES_ARQ INDEX WL_TABIX.
        READ TABLE TL_ITENS
          WITH KEY NR_LOTE_ADM = TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM
                   NUCONTRATO  = TL_ZPFE_LOTE_ITEM-NUCONTRATO
                   CHVID       = TL_ZPFE_LOTE_ITEM-CHVID.
        IF SY-SUBRC IS INITIAL.
          IF TL_ITENS-CK_CONF_ADM IS INITIAL.
            WL_TABIX = SY-TABIX.
            READ TABLE TL_ZLEST0025
              WITH KEY CHVID = TL_ITENS-CHVID.
            IF SY-SUBRC IS INITIAL.

              MOVE: TL_ZPFE_LOTE_ITEM-DT_CONF_ADM   TO TL_ITENS-DT_CONF_ADM,
                    TL_ZPFE_LOTE_ITEM-VL_CONF_ADM   TO TL_ITENS-VL_CONF_ADM,
                    TL_ZPFE_LOTE_ITEM-PESO_CONF_ADM TO TL_ITENS-PESO_CONF_ADM,
                    'X'                             TO TL_ITENS-CK_CONF_ADM.

              IF TL_ZLEST0025-NATUREZACHVID EQ 'S'.
                MULTIPLY TL_ITENS-VL_CONF_ADM  BY -1.
              ENDIF.

              MODIFY TL_ITENS INDEX WL_TABIX.
              WL_SUCESSO = 'X'.
              PERFORM INSERT_LINE_LOG USING TL_ITENS-NR_LOTE_ADM
                                            TL_ITENS-NM_LOTE
                                            TL_ITENS-CHVID
                                            TL_ITENS-NUCONTRATO
                                            'C'
                                            TEXT-S01
                                            SPACE
                                            'S'.
            ELSE.
              PERFORM INSERT_LINE_LOG USING TL_ITENS-NR_LOTE_ADM
                                            TL_ITENS-NM_LOTE
                                            TL_ITENS-CHVID
                                            TL_ITENS-NUCONTRATO
                                            'C'
                                            TEXT-E03
                                            TL_LINES_ARQ-LINE
                                            'E'.
            ENDIF.
          ELSE.
            PERFORM INSERT_LINE_LOG USING TL_ITENS-NR_LOTE_ADM
                                          TL_ITENS-NM_LOTE
                                          TL_ITENS-CHVID
                                          TL_ITENS-NUCONTRATO
                                          'C'
                                          TEXT-E02
                                          TL_LINES_ARQ-LINE
                                          'E'.

          ENDIF .
        ELSE.


          PERFORM INSERT_LINE_LOG USING TL_ZPFE_LOTE_ITEM-NR_LOTE_ADM
                                        SPACE
                                        TL_ZPFE_LOTE_ITEM-CHVID
                                        TL_ZPFE_LOTE_ITEM-NUCONTRATO
                                        'C'
                                        TEXT-E01
                                        TL_LINES_ARQ-LINE
                                        'E'.

        ENDIF .

*        CLEAR: RG_CHVID.
*        RG_CHVID-SIGN   = 'I'.
*        RG_CHVID-OPTION = 'EQ'.
*        RG_CHVID-LOW = TL_ZPFE_LOTE_ITEM-CHVID.
*        APPEND RG_CHVID.
*
*        CLEAR: RG_NUCONTRATO.
*        RG_NUCONTRATO-SIGN   = 'I'.
*        RG_NUCONTRATO-OPTION = 'EQ'.
*        RG_NUCONTRATO-LOW = TL_ZPFE_LOTE_ITEM-NUCONTRATO.
*        APPEND RG_NUCONTRATO.
*
*        CLEAR: RG_NUCONTRATO.
*        RG_NUCONTRATO-SIGN   = 'I'.
*        RG_NUCONTRATO-OPTION = 'EQ'.
*        RG_NUCONTRATO-LOW = TL_ZPFE_LOTE_ITEM-NUCONTRATO.
*        APPEND RG_NUCONTRATO.

      ENDLOOP.

      IF WL_SUCESSO IS NOT INITIAL.
        IF TL_ITENS[] IS NOT INITIAL.
          MODIFY ZPFE_LOTE_ITEM FROM TABLE TL_ITENS.
        ENDIF.
        CALL FUNCTION 'Z_BUSCA_PARAMETROS'
          EXPORTING
            I_NOME_PARAMETRO    = 'PROC_CONF_ADM'
          IMPORTING
            E_CAMINHO           = WL_CAMINHO_PROC
          EXCEPTIONS
            PARAMETRO_NOT_FOUND = 1
            OTHERS              = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        CONCATENATE WL_CAMINHO_PROC P_PATHNAME INTO WL_CAMINHO_PROC.
        CL_GUI_FRONTEND_SERVICES=>FILE_COPY( SOURCE       = WL_CAMINHO_INPUT
                                             DESTINATION  = WL_CAMINHO_PROC
                                             OVERWRITE    = ABAP_TRUE ).
        IF SY-SUBRC IS INITIAL.
          CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
            EXPORTING
              FILENAME = P_CAMINHO_AUX
            CHANGING
              RC       = WL_RC.

        ENDIF.

      ELSE.
        CALL FUNCTION 'Z_BUSCA_PARAMETROS'
          EXPORTING
            I_NOME_PARAMETRO    = 'LOG_CONF_ADM'
          IMPORTING
            E_CAMINHO           = WL_CAMINHO_LOG
          EXCEPTIONS
            PARAMETRO_NOT_FOUND = 1
            OTHERS              = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        CONCATENATE WL_CAMINHO_LOG P_PATHNAME INTO WL_CAMINHO_LOG.
        CL_GUI_FRONTEND_SERVICES=>FILE_COPY( SOURCE       = WL_CAMINHO_INPUT
                                             DESTINATION  = WL_CAMINHO_LOG
                                             OVERWRITE    = ABAP_TRUE ).
        IF SY-SUBRC IS INITIAL.
          CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
            EXPORTING
              FILENAME = P_CAMINHO_AUX
            CHANGING
              RC       = WL_RC.

        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " IMPORTA_CONFERENCIA
*&---------------------------------------------------------------------*
*&      Module  PESQ_ITENS_LOTE_CONFE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PESQ_ITENS_LOTE_CONF_ADM OUTPUT.
  IF ( VG_ALTEROU_CONF IS INITIAL ) AND ( VG_SCROLL_ROW-ROW_ID IS INITIAL ).
    PERFORM PESQUISA_ITENS_LOTE_CONF_ADM.
  ENDIF.

  PERFORM CRIA_ALV_LOTE_CONF_ADM.

  CALL METHOD COCKPIT_ALV_LOTES_CONF_ADM->REFRESH_TABLE_DISPLAY.

  CALL METHOD COCKPIT_ALV_LOTES_CONF_ADM->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_ROW_INFO = ES_ROW_INFO
      IS_COL_INFO = ES_COL_INFO
      IS_ROW_NO   = ES_ROW_NO.

ENDMODULE.                 " PESQ_ITENS_LOTE_CONFE  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INSERT_LINE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_ITENS_NR_LOTE_ADM  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0823   text
*      -->P_TEXT_S01  text
*----------------------------------------------------------------------*
FORM INSERT_LINE_LOG  USING    P_NR_LOTE_ADM
                               P_NM_LOTE
                               P_CHVID
                               P_NUCONTRATO
                               P_TIPO_ID
                               P_TEXT
                               P_LINHA_ARQ
                               P_TIPO_MSG.

  TG_0062-NR_LOTE_ADM = P_NR_LOTE_ADM.
  TG_0062-NR_LOTE     = P_NM_LOTE.
  TG_0062-CHVID       = P_CHVID.
  TG_0062-NUCONTRATO  = P_NUCONTRATO.
  TG_0062-ID_TIPO     = P_TIPO_ID.
  TG_0062-MSG_ERRO    = P_TEXT.
  TG_0062-LINHA       = P_LINHA_ARQ.

  APPEND TG_0062.

  IF P_TIPO_MSG EQ 'E'.
    MODIFY ZLEST0062 FROM TG_0062.
  ENDIF.
  CLEAR TG_0062.
ENDFORM.                    " INSERT_LINE_LOG
