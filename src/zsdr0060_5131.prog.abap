*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5131
*&---------------------------------------------------------------------*

DATA: IT_SOL_POP_5131  TYPE STANDARD TABLE OF TY_SOLICITACAO_5130,
      IT_ZSDT0131_5130 TYPE STANDARD TABLE OF ZSDT0131,
      VL_QTD_USADA     TYPE ZSDT0131-QTD_VINC.

DATA:  G_CUSTOM_CONTAINER_POP_5131 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5131           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5131          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5131    TYPE LVC_T_FCAT,
       IT_EXCLUDE_POP_5131         TYPE UI_FUNCTIONS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5131  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5131 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5131'.

  PERFORM PESQUISA_POP_5131.
  PERFORM MOSTRA_POP_5131.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5131  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5131 INPUT.

  DATA: VL_CHECK_5131 TYPE CHAR1,
        VL_LINES_5131 TYPE I.

  CTL_ALV1_POP_5131->CHECK_CHANGED_DATA( ).

  CASE SY-UCOMM.
    WHEN 'SALVAR'.

      PERFORM DESBLOQUEIA_SOL_POP_5131 USING ' ' ' '.
      DELETE IT_SOL_POP_5131 WHERE CHECK NE ABAP_TRUE OR COR EQ 'C600'.
      DESCRIBE TABLE IT_SOL_POP_5131 LINES VL_LINES_5131.

      IF VL_LINES_5131 EQ 0.
        MESSAGE TEXT-101 TYPE 'S' DISPLAY LIKE 'E'.
        PERFORM DESBLOQUEIA_SOL_POP_5131 USING 'X' 'X'.
      ELSE.
        APPEND LINES OF IT_SOL_POP_5131 TO IT_SOL_5130.
        PERFORM BUSCA_CLIENTES_POP_5131.
        PERFORM ATUALIZA_HEADER_LOTE_5130.

        CALL METHOD CTL_ALV1_5130->GET_FRONTEND_LAYOUT
          IMPORTING
            ES_LAYOUT = GS_LAYOUT_5130_ALV1.

        GS_LAYOUT_5130_ALV1-CWIDTH_OPT = ABAP_TRUE.

        CALL METHOD CTL_ALV1_5130->SET_FRONTEND_LAYOUT
          EXPORTING
            IS_LAYOUT = GS_LAYOUT_5130_ALV1.

        CALL METHOD CTL_ALV2_5130->GET_FRONTEND_LAYOUT
          IMPORTING
            ES_LAYOUT = GS_LAYOUT_5130_ALV2.

        GS_LAYOUT_5130_ALV2-CWIDTH_OPT = ABAP_TRUE.

        CALL METHOD CTL_ALV2_5130->SET_FRONTEND_LAYOUT
          EXPORTING
            IS_LAYOUT = GS_LAYOUT_5130_ALV2.

        LEAVE TO SCREEN 0.

        PERFORM DESBLOQUEIA_SOL_POP_5131 USING 'X' 'X'.
      ENDIF.

    WHEN 'CANCEL'.
      PERFORM DESBLOQUEIA_SOL_POP_5131 USING 'X' ' '.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5131
*&---------------------------------------------------------------------*
FORM PESQUISA_POP_5131.

  DATA: IT_VBKD         TYPE STANDARD TABLE OF VBKD,
        IT_MAKT         TYPE STANDARD TABLE OF MAKT,
        IT_VBPA         TYPE STANDARD TABLE OF VBPA,
        IT_KNA1         TYPE STANDARD TABLE OF KNA1,
        WA_VBKD         TYPE VBKD,
        WA_MAKT         TYPE MAKT,
        WA_VBPA         TYPE VBPA,
        WA_KNA1         TYPE KNA1,
        VL_CONT         TYPE I,
        WA_SOL_POP_5131 TYPE TY_SOLICITACAO_5130,
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
        INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5131
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
      INTO CORRESPONDING FIELDS OF TABLE IT_SOL_POP_5131
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

  IF IT_SOL_POP_5131 IS NOT INITIAL.

    SELECT *
      FROM VBKD
      INTO TABLE IT_VBKD
      FOR ALL ENTRIES IN IT_SOL_POP_5131
      WHERE VBELN EQ IT_SOL_POP_5131-VBELN.

    SELECT *
      FROM MAKT
      INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_SOL_POP_5131
      WHERE MATNR EQ IT_SOL_POP_5131-MATNR.

    SELECT *
      FROM ZSDT0131
      INTO TABLE IT_ZSDT0131_5130
      FOR ALL ENTRIES IN IT_SOL_POP_5131
      WHERE NRO_SOL EQ IT_SOL_POP_5131-NRO_SOL
        AND SEQ     EQ IT_SOL_POP_5131-SEQ
        AND STATUS  NE 'X'.

    SELECT *
      FROM VBPA
      INTO TABLE IT_VBPA
      FOR ALL ENTRIES IN IT_SOL_POP_5131
      WHERE VBELN EQ IT_SOL_POP_5131-VBELN
        AND PARVW EQ 'AG'.

    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_VBPA
      WHERE KUNNR EQ IT_VBPA-KUNNR.

    PERFORM BUSCA_LOCAL_EMBARQUE_5131 USING VL_LIFNR
                                            VL_COD_LOC_EMB
                                            VL_LOCAL_EMBARQ.

  ENDIF.

  LOOP AT IT_SOL_POP_5131 INTO WA_SOL_POP_5131.

    VL_CONT = VL_CONT + 1.

    READ TABLE IT_SOL_5130 WITH KEY NRO_SOL = WA_SOL_POP_5131-NRO_SOL
                                    SEQ     = WA_SOL_POP_5131-SEQ TRANSPORTING NO FIELDS.
    IF SY-SUBRC IS INITIAL.
      WA_SOL_POP_5131-INCO1 = '999'.        "Para eliminar o que já está na tela 5130
    ENDIF.

    IF WA_SOL_POP_5131-INCO1 NE '999'.

      READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_SOL_POP_5131-VBELN.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5131-INCO1 = WA_VBKD-INCO1.
      ENDIF.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_SOL_POP_5131-MATNR.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5131-MAKTX = WA_MAKT-MAKTX.
      ENDIF.

      READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN = WA_SOL_POP_5131-VBELN.
      IF SY-SUBRC IS INITIAL.
        WA_SOL_POP_5131-KUNNR = WA_VBPA-KUNNR.
        READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA-KUNNR.
        IF SY-SUBRC IS INITIAL.
          WA_SOL_POP_5131-NAME1 = WA_KNA1-NAME1.
        ENDIF.
      ENDIF.

      LOOP AT IT_ZSDT0131_5130 INTO WA_ZSDT0131
           WHERE NRO_SOL EQ WA_SOL_POP_5131-NRO_SOL.
        VL_QTD_USADA = VL_QTD_USADA + WA_ZSDT0131-QTD_VINC.
      ENDLOOP.

      WA_SOL_POP_5131-LIFNR = VL_LIFNR.
      WA_SOL_POP_5131-COD_LOC_EMB = VL_COD_LOC_EMB.
      WA_SOL_POP_5131-LOCAL_EMBARQ = VL_LOCAL_EMBARQ.

      WA_SOL_POP_5131-SALDO = WA_SOL_POP_5131-QTE_LIB - VL_QTD_USADA.
      WA_SOL_POP_5131-QTD_VINC = WA_SOL_POP_5131-SALDO.

      IF WA_SOL_POP_5131-SALDO LE 0.
        WA_SOL_POP_5131-INCO1 = '999'.
      ENDIF.

      CONCATENATE WA_SOL_POP_5131-NRO_SOL WA_SOL_POP_5131-SEQ WA_SOL_POP_5131-VBELN WA_SOL_POP_5131-POSNR INTO CHAVE.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          CHAVE          = CHAVE
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC <> 0.
        WA_SOL_POP_5131-COR = 'C600'.
      ENDIF.

      CLEAR: VL_QTD_USADA.

    ENDIF.

    MODIFY IT_SOL_POP_5131 FROM WA_SOL_POP_5131 INDEX VL_CONT.

  ENDLOOP.

  DELETE IT_SOL_POP_5131 WHERE INCO1 NE WA_HEADER_LOTE-INCO1.
  CLEAR: VL_CONT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_5131 .

  IF G_CUSTOM_CONTAINER_POP_5131 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_5131
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5131'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG_POP_5131 USING:
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

    GS_LAYOUT_POP_5131-SEL_MODE   = 'A'.
    GS_LAYOUT_POP_5131-STYLEFNAME = 'CELLSTYLES'.
    GS_LAYOUT_POP_5131-CWIDTH_OPT = 'X'.
    GS_LAYOUT_POP_5131-INFO_FNAME = 'COR'.

    PERFORM EXCLUIR_BOTOES CHANGING IT_EXCLUDE_POP_5131.

    CREATE OBJECT CTL_ALV1_POP_5131
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_5131.           "ALV Lote

    CALL METHOD CTL_ALV1_POP_5131->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_POP_5131
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_POP_5131
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG_POP_5131
        IT_OUTTAB            = IT_SOL_POP_5131.

  ELSE.
    CALL METHOD CTL_ALV1_POP_5131->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5131_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5131_EXIT INPUT.
  PERFORM DESBLOQUEIA_SOL_POP_5131 USING 'X' ' '.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CLIENTES_POP
*&---------------------------------------------------------------------*
FORM BUSCA_CLIENTES_POP_5131.

  DATA: WA_CLIENTE_5130 TYPE TY_CLIENTE_5130,
        WA_SOL_5130     TYPE TY_SOLICITACAO_5130.

  DATA: LS_STYLE TYPE LVC_S_STYL,
        WL_NAME  TYPE THEAD-TDNAME,
        TG_TEXTO TYPE STANDARD TABLE OF TLINE.

  LOOP AT IT_SOL_POP_5131 INTO WA_SOL_5130.

    WA_CLIENTE_5130-NRO_SOL = WA_SOL_5130-NRO_SOL.
    WA_CLIENTE_5130-SEQ     = WA_SOL_5130-SEQ.
    WA_CLIENTE_5130-KUNNR   = WA_SOL_5130-KUNNR.
    WA_CLIENTE_5130-NAME1   = WA_SOL_5130-NAME1.
    WA_CLIENTE_5130-NR_ROT1 = WA_SOL_5130-NR_ROT.
    WA_CLIENTE_5130-NR_ROT2 = WA_SOL_5130-NR_ROT.

    LS_STYLE-FIELDNAME = 'OBS'.
    LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    APPEND LS_STYLE TO WA_CLIENTE_5130-CELLSTYLES.

    CONCATENATE WA_SOL_5130-NRO_SOL '001' INTO WL_NAME.

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
      WA_CLIENTE_5130-OBS = '@1F@'.
    ELSE.
      WA_CLIENTE_5130-OBS = '@1E@'.
    ENDIF.

    APPEND WA_CLIENTE_5130 TO IT_CLIENTE_5130.
    CLEAR: WA_CLIENTE_5130.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOCAL_EMBARQUE_5131
*&---------------------------------------------------------------------*
FORM BUSCA_LOCAL_EMBARQUE_5131 USING VL_LIFNR        TYPE ZSDT0132-LIFNR
                                     VL_COD_LOC_EMB  TYPE ZSDT0131-COD_LOC_EMB
                                     VL_LOCAL_EMBARQ TYPE ZSDT0131-LOCAL_EMBARQ. .

  DATA: IT_ZSDT0132 TYPE STANDARD TABLE OF ZSDT0132,
        WA_ZSDT0132 TYPE ZSDT0132,
        VL_LINES    TYPE I.

  SELECT *
    FROM ZSDT0132
    INTO TABLE IT_ZSDT0132
    WHERE MARCA EQ WA_HEADER_LOTE-WRKST
      AND STATUS NE 'I'.

  DESCRIBE TABLE IT_ZSDT0132 LINES VL_LINES.

  IF VL_LINES NE 1.
    CLEAR: VL_LIFNR, VL_COD_LOC_EMB, VL_LOCAL_EMBARQ.
  ELSE.
    READ TABLE IT_ZSDT0132 INTO WA_ZSDT0132 INDEX 1.
    VL_LIFNR = WA_ZSDT0132-LIFNR.
    VL_COD_LOC_EMB = WA_ZSDT0132-NR_ROT.
    VL_LOCAL_EMBARQ = WA_ZSDT0132-ROT_DESC.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_POP_5131
*&---------------------------------------------------------------------*
FORM DESBLOQUEIA_SOL_POP_5131 USING VALUE(P_01)
                                    VALUE(P_02).

  DATA: WA_SOL_POP_5131 TYPE TY_SOLICITACAO_5130,
        CHAVE_EXIT_5131 TYPE zde_chave_sol.

  LOOP AT IT_SOL_POP_5131 INTO WA_SOL_POP_5131 WHERE COR IS INITIAL
                                                 AND ( CHECK EQ P_01 OR
                                                       CHECK EQ P_02 ).

    CONCATENATE WA_SOL_POP_5131-NRO_SOL WA_SOL_POP_5131-SEQ WA_SOL_POP_5131-VBELN WA_SOL_POP_5131-POSNR INTO CHAVE_EXIT_5131.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        CHAVE = CHAVE_EXIT_5131.

  ENDLOOP.

ENDFORM.
