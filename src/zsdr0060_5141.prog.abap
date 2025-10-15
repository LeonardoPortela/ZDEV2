*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5131
*&---------------------------------------------------------------------*

DATA: IT_SOL_POP_5141   TYPE STANDARD TABLE OF TY_SOL_5140,
      IT_ZSDT0131_5140  TYPE STANDARD TABLE OF ZSDT0131,
      VL_QTD_USADA_5141 TYPE MARA-MEINS.

DATA:  G_CUSTOM_CONTAINER_POP_5141 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5141           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5141          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5141    TYPE LVC_T_FCAT,
       IT_EXCLUDE_POP_5141         TYPE UI_FUNCTIONS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5141  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5141 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5141'.

  PERFORM PESQUISA_POP_5141.
  PERFORM MOSTRA_POP_5141.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5141  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5141 INPUT.

  DATA: VL_LINES_5141 TYPE I.

  CTL_ALV1_POP_5141->CHECK_CHANGED_DATA( ).

  CASE SY-UCOMM.
    WHEN 'SALVAR'.

      PERFORM DESBLOQUEIA_SOL_POP_5141 USING ' ' ' '.
      DELETE IT_SOL_POP_5141 WHERE CHECK NE ABAP_TRUE OR COR EQ 'C600'.

      DESCRIBE TABLE IT_SOL_POP_5141 LINES VL_LINES_5141.

      IF VL_LINES_5141 EQ 0.
        MESSAGE TEXT-101 TYPE 'S' DISPLAY LIKE 'E'.
        PERFORM DESBLOQUEIA_SOL_POP_5141 USING 'X' 'X'.
      ELSE.
        APPEND LINES OF IT_SOL_POP_5141 TO IT_SOL_5140.
        PERFORM BUSCA_CLIENTES_POP_5141.
        PERFORM ATUALIZA_HEADER_5140.
      ENDIF.

      CALL METHOD CTL_ALV1_5140->GET_FRONTEND_LAYOUT
        IMPORTING
          ES_LAYOUT = GS_LAYOUT_5140_ALV1.

      GS_LAYOUT_5140_ALV1-CWIDTH_OPT = ABAP_TRUE.

      CALL METHOD CTL_ALV1_5140->SET_FRONTEND_LAYOUT
        EXPORTING
          IS_LAYOUT = GS_LAYOUT_5140_ALV1.

      CALL METHOD CTL_ALV2_5140->GET_FRONTEND_LAYOUT
        IMPORTING
          ES_LAYOUT = GS_LAYOUT_5140_ALV2.

      GS_LAYOUT_5140_ALV2-CWIDTH_OPT = ABAP_TRUE.

      CALL METHOD CTL_ALV2_5140->SET_FRONTEND_LAYOUT
        EXPORTING
          IS_LAYOUT = GS_LAYOUT_5140_ALV2.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      PERFORM DESBLOQUEIA_SOL_POP_5141 USING 'X' ' '.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5141
*&---------------------------------------------------------------------*
FORM PESQUISA_POP_5141.

  DATA: IT_VBKD         TYPE STANDARD TABLE OF VBKD,
        IT_MAKT         TYPE STANDARD TABLE OF MAKT,
        IT_VBPA         TYPE STANDARD TABLE OF VBPA,
        IT_KNA1         TYPE STANDARD TABLE OF KNA1,
        WA_VBKD         TYPE VBKD,
        WA_MAKT         TYPE MAKT,
        WA_VBPA         TYPE VBPA,
        WA_KNA1         TYPE KNA1,
        VL_CONT         TYPE I,
        WA_SOL_POP_5141 TYPE TY_SOL_5140,
        WA_ZSDT0131     TYPE ZSDT0131,
        VL_LIFNR        TYPE ZSDT0132-LIFNR,
        VL_COD_LOC_EMB  TYPE ZSDT0131-COD_LOC_EMB,
        VL_LOCAL_EMBARQ TYPE ZSDT0131-LOCAL_EMBARQ,
        CHAVE           TYPE zde_chave_sol.

  IF P_FILIAL IS NOT INITIAL.

    SELECT VBAP~MATNR
             MARA~MEINS
             MARA~WRKST
             ZSDT0082~MANDT
             ZSDT0082~NRO_SOL
             ZSDT0082~SEQ
             ZSDT0082~VBELN
             ZSDT0082~POSNR
             ZSDT0082~SEQ_LIB
             ZSDT0082~VKORG
             ZSDT0082~SPART
             ZSDT0082~VKGRP
             ZSDT0082~VKBUR
             ZSDT0082~AUART
             VBAP~WERKS
             ZSDT0082~QTE_SOL
             ZSDT0082~DT_LIBER
             ZSDT0082~USUARIO_LIB
             ZSDT0082~QTE_LIB
             ZSDT0082~STATUS
             ZSDT0082~DT_ENTREGA
             MARA~BRGEW
             ZSDT0082~NR_ROT
        INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5141
        FROM ZSDT0082
        INNER JOIN VBAP ON ZSDT0082~VBELN = VBAP~VBELN AND
                           ZSDT0082~POSNR = VBAP~POSNR
        INNER JOIN MARA ON MARA~MATNR = VBAP~MATNR
        INNER JOIN VBAK ON VBAK~VBELN = VBAP~VBELN
          WHERE ZSDT0082~VKBUR  IN P_VKBUR
            AND ZSDT0082~VKORG  IN P_VKORG
            AND ZSDT0082~SPART  EQ P_SPART
            AND VBAK~KUNNR IN P_KUNNR
            AND ZSDT0082~SEQ    NE 1
            AND MARA~WRKST      EQ WA_HEADER_LOTE-WRKST
            AND ( ZSDT0082~STATUS EQ 2 OR
                  ZSDT0082~STATUS EQ 5 ).

  ELSEIF P_CORPLT IS NOT INITIAL OR
         P_CORPCG IS NOT INITIAL OR
         P_CORPPT IS NOT INITIAL.

    SELECT VBAP~MATNR
           MARA~MEINS
           MARA~WRKST
           ZSDT0082~MANDT
           ZSDT0082~NRO_SOL
           ZSDT0082~SEQ
           ZSDT0082~VBELN
           ZSDT0082~POSNR
           ZSDT0082~SEQ_LIB
           ZSDT0082~VKORG
           ZSDT0082~SPART
           ZSDT0082~VKGRP
           ZSDT0082~VKBUR
           ZSDT0082~AUART
           VBAP~WERKS
           ZSDT0082~QTE_SOL
           ZSDT0082~DT_LIBER
           ZSDT0082~USUARIO_LIB
           ZSDT0082~QTE_LIB
           ZSDT0082~STATUS
           ZSDT0082~DT_ENTREGA
           MARA~BRGEW
           ZSDT0082~NR_ROT
      INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5141
      FROM ZSDT0082
      INNER JOIN VBAP ON ZSDT0082~VBELN = VBAP~VBELN AND
                         ZSDT0082~POSNR = VBAP~POSNR
      INNER JOIN MARA ON MARA~MATNR = VBAP~MATNR
      INNER JOIN VBAK ON VBAK~VBELN = VBAP~VBELN
        WHERE ZSDT0082~VKBUR  IN C_VKBUR
          AND ZSDT0082~VKORG  IN C_VKORG
          AND ZSDT0082~SPART  EQ C_SPART
          AND VBAK~KUNNR IN C_KUNNR
          AND ZSDT0082~SEQ    NE 1
          AND MARA~WRKST      EQ WA_HEADER_LOTE-WRKST
          AND ( ZSDT0082~STATUS EQ 2 OR
                ZSDT0082~STATUS EQ 5 ).

  ENDIF.

  IF IT_SOL_POP_5141 IS NOT INITIAL.

    SELECT *
      FROM VBKD
      INTO TABLE IT_VBKD
      FOR ALL ENTRIES IN IT_SOL_POP_5141
      WHERE VBELN EQ IT_SOL_POP_5141-VBELN.

    SELECT *
      FROM MAKT
      INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_SOL_POP_5141
      WHERE MATNR EQ IT_SOL_POP_5141-MATNR.

    SELECT *
      FROM ZSDT0131
      INTO TABLE IT_ZSDT0131_5140
      FOR ALL ENTRIES IN IT_SOL_POP_5141
      WHERE NRO_SOL EQ IT_SOL_POP_5141-NRO_SOL
        AND SEQ     EQ IT_SOL_POP_5141-SEQ
        AND STATUS  NE 'X'.

    SELECT *
      FROM VBPA
      INTO TABLE IT_VBPA
      FOR ALL ENTRIES IN IT_SOL_POP_5141
      WHERE VBELN EQ IT_SOL_POP_5141-VBELN
        AND PARVW EQ 'AG'.

    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_VBPA
      WHERE KUNNR EQ IT_VBPA-KUNNR.

    PERFORM BUSCA_LOCAL_EMBARQUE_5141 USING VL_LIFNR
                                          VL_COD_LOC_EMB
                                          VL_LOCAL_EMBARQ.

  ENDIF.


  LOOP AT IT_SOL_POP_5141 INTO WA_SOL_POP_5141.

    VL_CONT = VL_CONT + 1.

    READ TABLE IT_SOL_5140 WITH KEY NRO_SOL = WA_SOL_POP_5141-NRO_SOL
                                    SEQ     = WA_SOL_POP_5141-SEQ TRANSPORTING NO FIELDS.
    IF SY-SUBRC IS INITIAL.
      WA_SOL_POP_5141-INCO1 = '999'.        "Para eliminar o que já está na tela 5140
    ENDIF.

    IF WA_SOL_POP_5141-INCO1 NE '999'..

      READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_SOL_POP_5141-VBELN.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5141-INCO1 = WA_VBKD-INCO1.
      ENDIF.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_SOL_POP_5141-MATNR.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5141-MAKTX = WA_MAKT-MAKTX.
      ENDIF.

      READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN = WA_SOL_POP_5141-VBELN.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5141-KUNNR = WA_VBPA-KUNNR.
        READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA-KUNNR.
        IF SY-SUBRC IS INITIAL.
          WA_SOL_POP_5141-NAME1 = WA_KNA1-NAME1.
        ENDIF.
      ENDIF.

      LOOP AT IT_ZSDT0131_5140 INTO WA_ZSDT0131
      WHERE NRO_SOL EQ WA_SOL_POP_5141-NRO_SOL.
        VL_QTD_USADA = VL_QTD_USADA + WA_ZSDT0131-QTD_VINC.
      ENDLOOP.

      WA_SOL_POP_5141-LIFNR = VL_LIFNR.
      WA_SOL_POP_5141-COD_LOC_EMB = VL_COD_LOC_EMB.
      WA_SOL_POP_5141-LOCAL_EMBARQ = VL_LOCAL_EMBARQ.

      WA_SOL_POP_5141-SALDO = WA_SOL_POP_5141-QTE_LIB - VL_QTD_USADA.
      WA_SOL_POP_5141-QTD_VINC = WA_SOL_POP_5141-SALDO.

      IF WA_SOL_POP_5141-SALDO LE 0.
        WA_SOL_POP_5141-INCO1 = '999'.
      ENDIF.

      CONCATENATE WA_SOL_POP_5141-NRO_SOL WA_SOL_POP_5141-SEQ WA_SOL_POP_5141-VBELN WA_SOL_POP_5141-POSNR INTO CHAVE.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          CHAVE          = CHAVE
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC <> 0.
        WA_SOL_POP_5141-COR = 'C600'.
      ENDIF.

      CLEAR: VL_QTD_USADA.

    ENDIF.

    MODIFY IT_SOL_POP_5141 FROM WA_SOL_POP_5141 INDEX VL_CONT.

  ENDLOOP.

  DELETE IT_SOL_POP_5141 WHERE INCO1 NE WA_HEADER_LOTE-INCO1.
  CLEAR: VL_CONT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5141
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_5141.

  IF G_CUSTOM_CONTAINER_POP_5141 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_5141
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5141'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG_POP_5141 USING:
          01 'CHECK'          ''         ' '  'X' ' '  ' '   'X'   'X'   ' '   ' '   'Nro Lote',
          01 'NRO_SOL'        'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          02 'VBELN'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'AUART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          05 'VKBUR'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          06 'KUNNR'          'VBPA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          07 'NAME1'          'KNA1'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Cliente',
          08 'VKORG'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          09 'SPART'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv.',
          10 'WERKS'          'ZSDT0082' ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          11 'MATNR'          'MARA'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          12 'MAKTX'          'MAKT'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          13 'SALDO'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          14 'MEINS'          'VBAP'     ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'U.M.'.

    GS_LAYOUT_POP_5141-SEL_MODE   = 'A'.
    GS_LAYOUT_POP_5141-STYLEFNAME = 'CELLSTYLES'.
    GS_LAYOUT_POP_5141-CWIDTH_OPT = 'X'.
    GS_LAYOUT_POP_5141-INFO_FNAME = 'COR'.

    PERFORM EXCLUIR_BOTOES CHANGING IT_EXCLUDE_POP_5141.

    CREATE OBJECT CTL_ALV1_POP_5141
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_5141.           "ALV Lote

    CALL METHOD CTL_ALV1_POP_5141->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_POP_5141
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_POP_5141
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_POP_5141
        IT_OUTTAB            = IT_SOL_POP_5141.

  ELSE.
    CALL METHOD CTL_ALV1_POP_5141->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5141_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5141_EXIT INPUT.
  PERFORM DESBLOQUEIA_SOL_POP_5141 USING 'X' ' '.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CLIENTES_POP_5141
*&---------------------------------------------------------------------*
FORM BUSCA_CLIENTES_POP_5141.

  DATA: WA_CLIENTES_POP_5141 TYPE TY_CLIENTE_5140,
        WA_ZSDT0082_POP_5141 TYPE TY_SOL_5140.

  DATA: LS_STYLE TYPE LVC_S_STYL,
        WL_NAME  TYPE THEAD-TDNAME,
        TG_TEXTO TYPE STANDARD TABLE OF TLINE.

  LOOP AT IT_SOL_POP_5141 INTO WA_ZSDT0082_POP_5141.

    WA_CLIENTES_POP_5141-NRO_SOL = WA_ZSDT0082_POP_5141-NRO_SOL.
    WA_CLIENTES_POP_5141-SEQ     = WA_ZSDT0082_POP_5141-SEQ.
    WA_CLIENTES_POP_5141-KUNNR   = WA_ZSDT0082_POP_5141-KUNNR.
    WA_CLIENTES_POP_5141-NAME1   = WA_ZSDT0082_POP_5141-NAME1.
    WA_CLIENTES_POP_5141-NR_ROT1 = WA_ZSDT0082_POP_5141-NR_ROT.
    WA_CLIENTES_POP_5141-NR_ROT2 = WA_ZSDT0082_POP_5141-NR_ROT.

    LS_STYLE-FIELDNAME = 'OBS'.
    LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    APPEND LS_STYLE TO WA_CLIENTES_POP_5141-CELLSTYLES.

    CONCATENATE WA_ZSDT0082_POP_5141-NRO_SOL '001' INTO WL_NAME.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = 'OBSE'
        LANGUAGE                = SY-LANGU
        NAME                    = WL_NAME
        OBJECT                  = 'ZTEXTO'
      TABLES
        LINES                   = TG_TEXTO
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF TG_TEXTO IS INITIAL.
      WA_CLIENTES_POP_5141-OBS = '@1F@'.
    ELSE.
      WA_CLIENTES_POP_5141-OBS = '@1E@'.
    ENDIF.

    APPEND WA_CLIENTES_POP_5141 TO IT_CLIENTE_5140.
    CLEAR: WA_CLIENTES_POP_5141.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOCAL_EMBARQUE_5141
*&---------------------------------------------------------------------*
FORM BUSCA_LOCAL_EMBARQUE_5141 USING VL_LIFNR TYPE ZSDT0132-LIFNR
                                     VL_COD_LOC_EMB  TYPE ZSDT0131-COD_LOC_EMB
                                     VL_LOCAL_EMBARQ TYPE ZSDT0131-LOCAL_EMBARQ. .

  DATA: IT_ZSDT0132_5141 TYPE STANDARD TABLE OF ZSDT0132,
        WA_ZSDT0132_5141 TYPE ZSDT0132,
        VL_LINES         TYPE I.

  SELECT *
    FROM ZSDT0132
    INTO TABLE IT_ZSDT0132_5141
    WHERE MARCA EQ WA_HEADER_LOTE-WRKST
    AND STATUS NE 'I'.

  DESCRIBE TABLE IT_ZSDT0132_5141 LINES VL_LINES.

  IF VL_LINES NE 1.

    CLEAR: VL_LIFNR, VL_COD_LOC_EMB, VL_LOCAL_EMBARQ.

  ELSE.

    READ TABLE IT_ZSDT0132_5141 INTO WA_ZSDT0132_5141 INDEX 1.

    VL_LIFNR = WA_ZSDT0132_5141-LIFNR.
    VL_COD_LOC_EMB = WA_ZSDT0132_5141-NR_ROT.
    VL_LOCAL_EMBARQ = WA_ZSDT0132_5141-ROT_DESC.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_POP_5741
*&---------------------------------------------------------------------*
FORM DESBLOQUEIA_SOL_POP_5141 USING VALUE(P_01)
                                    VALUE(P_02).

  DATA: WA_SOL_POP_5141 TYPE TY_SOL_5140,
        CHAVE_EXIT_5141 TYPE zde_chave_sol.

  LOOP AT IT_SOL_POP_5141 INTO WA_SOL_POP_5141 WHERE COR IS INITIAL
                                                 AND ( CHECK EQ P_01 OR
                                                       CHECK EQ P_02 ) .

    CONCATENATE WA_SOL_POP_5141-NRO_SOL WA_SOL_POP_5141-SEQ WA_SOL_POP_5141-VBELN WA_SOL_POP_5141-POSNR INTO CHAVE_EXIT_5141.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        CHAVE = CHAVE_EXIT_5141.

  ENDLOOP.

ENDFORM.
