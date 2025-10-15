FUNCTION ZSDMF001_ATUALI_OV_MAT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_SOMA) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(I_VBELN) TYPE  ZSDT0090-VBELN
*"     REFERENCE(I_POSNR) TYPE  ZSDT0041-POSNR
*"  TABLES
*"      IT_OV STRUCTURE  ZSDS015
*"      TE_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: TL_VBAK            TYPE TABLE OF VBAK WITH HEADER LINE,
        TL_VBAP            TYPE TABLE OF VBAP WITH HEADER LINE,
        TL_VBEP            TYPE TABLE OF VBEP WITH HEADER LINE,
        WL_ORDERHEADERIN   TYPE BAPISDH1,
        WL_ORDERHEADERINX  TYPE BAPISDH1X,
        WL_LOGIC_SWITCH    TYPE BAPISDLS,
        TL_BAPISDITM       TYPE TABLE OF BAPISDITM WITH HEADER LINE,
        TL_BAPISDITMX      TYPE TABLE OF BAPISDITMX WITH HEADER LINE,
        TL_RETURN          TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
        WL_RETURN          TYPE BAPIRET2,
        TL_SCHEDULE_LINES  TYPE TABLE OF BAPISCHDL WITH HEADER LINE,
        TL_SCHEDULE_LINESX TYPE TABLE OF BAPISCHDLX WITH HEADER LINE,
        TL_BAPICOND        TYPE TABLE OF BAPICOND WITH HEADER LINE,
        TL_BAPICONDX       TYPE TABLE OF BAPICONDX WITH HEADER LINE,
        TL_SAIDA_EXEC      TYPE TABLE OF TY_SAIDA_EXEC WITH HEADER LINE,
        WL_TABIX           TYPE SY-TABIX,
        V_SALDO            TYPE VBAP-KWMENG,
        WL_POSNR           TYPE POSNR_VA,
        WL_MATERIAL        TYPE BAPISDITM-MATERIAL,
        WL_VLR_COVERT      TYPE DZMENG,
        CONT               TYPE C,
        V_CHECK            TYPE C,
        NVBELN             TYPE VBELN,
        NPOSNR             TYPE POSNR,
        NMATNR             TYPE MATNR,
        IT_0041            TYPE TABLE OF ZSDT0041 WITH HEADER LINE.

  REFRESH: TL_VBAK, TL_VBAP, TL_VBEP.

  SORT IT_OV BY POSNR.

  LOOP AT IT_OV.

    READ TABLE TE_RETURN INTO WL_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT *
      FROM VBAP
      INTO TABLE TL_VBAP
      WHERE VBELN EQ IT_OV-VBELN
       AND  POSNR EQ IT_OV-POSNR.

    IF TL_VBAP[] IS NOT INITIAL.

      SELECT *
        FROM VBAK
        INTO TABLE TL_VBAK
        FOR ALL ENTRIES IN TL_VBAP
        WHERE VBELN EQ TL_VBAP-VBELN.

      IF SY-SUBRC IS INITIAL.

        SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @TL_VBAK WHERE KNUMV EQ @TL_VBAK-KNUMV AND KSCHL EQ 'PR00' INTO TABLE @DATA(IT_KONV) .

        SELECT *
          FROM VBEP
          INTO TABLE TL_VBEP
           FOR ALL ENTRIES IN TL_VBAP
            WHERE VBELN EQ TL_VBAP-VBELN
              AND POSNR EQ TL_VBAP-POSNR
              AND WMENG NE 0.

        WL_ORDERHEADERINX-UPDATEFLAG = 'U'.
        WL_LOGIC_SWITCH-COND_HANDL   = 'X'.

        LOOP AT TL_VBAP.

          REFRESH: TL_BAPISDITM, TL_BAPISDITMX, TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX, TL_BAPICOND, TL_BAPICONDX.
          CLEAR:  TL_BAPISDITM, TL_BAPISDITMX, TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX, TL_BAPICOND, TL_BAPICONDX.

          IF SY-SUBRC IS INITIAL.
            WL_TABIX = SY-TABIX.
            IF I_SOMA = 'A'.
              V_SALDO = IT_OV-ZMENG.
            ELSEIF I_SOMA = 'X'.
              V_SALDO = TL_VBAP-KWMENG + IT_OV-ZMENG.
            ELSE.
              V_SALDO = TL_VBAP-KWMENG - IT_OV-ZMENG.
            ENDIF.

            IF V_SALDO EQ 0.
              V_SALDO = 1.
              V_CHECK = ABAP_TRUE.
            ENDIF.

            DATA(SALDO) = IT_OV-ZMENG.
            IF SALDO EQ 0.
              SALDO = 1.
            ENDIF.

            CLEAR TL_VBEP.

            LOOP AT TL_VBEP ASSIGNING FIELD-SYMBOL(<VBEP>) WHERE VBELN = TL_VBAP-VBELN AND
                                                   POSNR = TL_VBAP-POSNR.

              IF SALDO IS INITIAL.
                <VBEP>-WMENG = 0.
                CONTINUE.
              ENDIF.

              IF SALDO > <VBEP>-WMENG.
                SALDO = SALDO - <VBEP>-WMENG.
              ELSE.
                <VBEP>-WMENG = SALDO.
                SALDO = 0.
              ENDIF.
            ENDLOOP.

            MOVE: 'U'               TO TL_BAPISDITMX-UPDATEFLAG,
                  TL_VBAP-POSNR     TO TL_BAPISDITMX-ITM_NUMBER,
                  'X'               TO TL_BAPISDITMX-TARGET_QTY,
                  TL_VBAP-POSNR     TO TL_BAPISDITM-ITM_NUMBER,
                  V_SALDO           TO TL_BAPISDITM-TARGET_QTY.

            APPEND TL_BAPISDITMX.
            APPEND TL_BAPISDITM.
            CLEAR: TL_BAPISDITM, TL_BAPISDITMX.

            IF V_CHECK EQ ABAP_TRUE.

              IF LINE_EXISTS( IT_KONV[ KPOSN = TL_VBAP-POSNR ] ).
                TL_BAPICOND-COND_UNIT  = SWITCH #( IT_KONV[ KPOSN = TL_VBAP-POSNR ]-KMEIN WHEN 'TO' THEN 'KG' ).
                TL_BAPICONDX-COND_UNIT = SWITCH #( IT_KONV[ KPOSN = TL_VBAP-POSNR ]-KMEIN WHEN 'TO' THEN ABAP_TRUE ).
              ENDIF.

              MOVE: TL_VBAP-POSNR TO TL_BAPICOND-ITM_NUMBER,
                    '01'          TO TL_BAPICOND-COND_COUNT,
                    'PR00'        TO TL_BAPICOND-COND_TYPE,
                    '0.1'         TO TL_BAPICOND-COND_VALUE,

                    TL_VBAP-POSNR TO TL_BAPICONDX-ITM_NUMBER,
                    '01'          TO TL_BAPICONDX-COND_COUNT,
                    'PR00'        TO TL_BAPICONDX-COND_TYPE,
                    'X'           TO TL_BAPICONDX-COND_VALUE,

                    'U'           TO TL_BAPICONDX-UPDATEFLAG.

              APPEND: TL_BAPICOND,
                      TL_BAPICONDX.

              CLEAR: TL_BAPICOND, TL_BAPICONDX.

              APPEND VALUE #(
                              ITM_NUMBER = TL_VBAP-POSNR
                              COND_COUNT = '01'
                              COND_TYPE  = 'RB00'
                              COND_VALUE = 0
                              COND_UNIT  = IT_OV-VRKME
                              CURRENCY   = TL_VBAK-WAERK
                            ) TO TL_BAPICOND.

              APPEND VALUE #(
                              ITM_NUMBER = TL_VBAP-POSNR
                              COND_COUNT = '01'
                              COND_TYPE  = 'RB00'
                              COND_VALUE = ABAP_TRUE
                              COND_UNIT  = ABAP_TRUE
                              UPDATEFLAG = 'U'
                              CURRENCY   = ABAP_TRUE
                            ) TO TL_BAPICONDX.
            ENDIF.


            LOOP AT TL_VBEP WHERE VBELN EQ TL_VBAP-VBELN
                             AND  POSNR EQ TL_VBAP-POSNR.

              MOVE: 'U'                TO TL_SCHEDULE_LINESX-UPDATEFLAG,
                    TL_VBAP-POSNR      TO TL_SCHEDULE_LINESX-ITM_NUMBER,
                    TL_VBEP-ETENR      TO TL_SCHEDULE_LINESX-SCHED_LINE,
                    'X'                TO TL_SCHEDULE_LINESX-REQ_QTY,
                    TL_VBAP-POSNR      TO TL_SCHEDULE_LINES-ITM_NUMBER,
                    TL_VBEP-ETENR      TO TL_SCHEDULE_LINES-SCHED_LINE,
*                    V_SALDO            TO TL_SCHEDULE_LINES-REQ_QTY.
                    TL_VBEP-WMENG      TO TL_SCHEDULE_LINES-REQ_QTY.

              IF V_CHECK EQ ABAP_TRUE.
                MOVE: ABAP_TRUE     TO TL_SCHEDULE_LINESX-REQ_DLV_BL,
                      '12'          TO TL_SCHEDULE_LINES-REQ_DLV_BL.
              ENDIF.

              APPEND TL_SCHEDULE_LINESX.
              APPEND TL_SCHEDULE_LINES.

              CLEAR: TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX.
            ENDLOOP.

*            LOOP AT TL_VBEP.
*              READ TABLE TL_SCHEDULE_LINES WITH KEY ITM_NUMBER = TL_VBAP-POSNR SCHED_LINE = TL_VBEP-ETENR.
*              IF TL_VBEP-WMENG EQ TL_SCHEDULE_LINES-SCHED_LINE.
*                DELETE TL_SCHEDULE_LINES  WHERE ITM_NUMBER EQ TL_VBAP-POSNR AND SCHED_LINE EQ TL_VBEP-ETENR.
*                DELETE TL_SCHEDULE_LINESX WHERE ITM_NUMBER EQ TL_VBAP-POSNR AND SCHED_LINE EQ TL_VBEP-ETENR.
*              ENDIF.
*            ENDLOOP.

            CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                SALESDOCUMENT    = TL_VBAP-VBELN
                ORDER_HEADER_IN  = WL_ORDERHEADERIN
                ORDER_HEADER_INX = WL_ORDERHEADERINX
                LOGIC_SWITCH     = WL_LOGIC_SWITCH
              TABLES
                ORDER_ITEM_IN    = TL_BAPISDITM
                ORDER_ITEM_INX   = TL_BAPISDITMX
                CONDITIONS_IN    = TL_BAPICOND
                CONDITIONS_INX   = TL_BAPICONDX
                SCHEDULE_LINES   = TL_SCHEDULE_LINES
                SCHEDULE_LINESX  = TL_SCHEDULE_LINESX
                RETURN           = TL_RETURN.

            CLEAR:WL_RETURN.
            READ TABLE TL_RETURN INTO WL_RETURN WITH KEY TYPE = 'E'.
            IF SY-SUBRC NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              READ TABLE TL_RETURN INTO WL_RETURN WITH KEY TYPE   = 'S'
                                                           NUMBER = 311.
              IF SY-SUBRC = 0.
                APPEND WL_RETURN TO TE_RETURN.
              ENDIF.
            ELSE.
              APPEND LINES OF TL_RETURN TO TE_RETURN.

              SELECT SINGLE *
                FROM VBAP
                INTO @DATA(WVBAP)
                WHERE VBELN EQ @NVBELN AND
                      POSNR EQ @NPOSNR.

              DATA(ORDER_HEADER_INX) = VALUE BAPISDH1X( UPDATEFLAG = 'U' ).
              DATA(ORDER_ITEM_IN) = VALUE WISO_T_SDITM( (
                                                          ITM_NUMBER = WVBAP-POSNR
                                                          MATERIAL   = WVBAP-MATNR
                                                      ) ).

              DATA(ORDER_ITEM_INX) = VALUE WISO_T_SDITMX( (
                                                            ITM_NUMBER = WVBAP-POSNR
                                                            MATERIAL   = WVBAP-MATNR
                                                            UPDATEFLAG = 'D'
                                                        ) ).

              CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
                EXPORTING
                  SALESDOCUMENT    = WVBAP-VBELN
                  ORDER_HEADER_INX = ORDER_HEADER_INX
                TABLES
                  ORDER_ITEM_IN    = ORDER_ITEM_IN
                  ORDER_ITEM_INX   = ORDER_ITEM_INX
                  RETURN           = TL_RETURN.

              IF NOT LINE_EXISTS( TL_RETURN[ TYPE = 'E' ] ).
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    WAIT = ABAP_TRUE.
                APPEND LINES OF TL_RETURN TO TE_RETURN.
              ELSE.
                APPEND LINES OF TL_RETURN TO TE_RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ELSE.

*******************      CRIA UM NOVO ITEM NA MESMA ORDEM    *****************************

      REFRESH: TL_BAPISDITM, TL_BAPISDITMX, TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX,
               TL_BAPICOND.

      CLEAR:   TL_BAPISDITM, TL_BAPISDITMX, TL_BAPICOND, TL_BAPICONDX.

      SELECT SINGLE *
        FROM VBAK
        INTO @DATA(_VBAK)
        WHERE VBELN EQ @IT_OV-VBELN.

      SELECT *
        UP TO 1 ROWS
        FROM VBAP
        INTO TABLE TL_VBAP
          WHERE VBELN EQ IT_OV-VBELN
          ORDER BY POSNR DESCENDING.

      READ TABLE TL_VBAP INDEX 1.

      TL_BAPISDITM-STORE_LOC     = IT_OV-LGORT.
      TL_BAPISDITM-ITM_NUMBER   = TL_VBAP-POSNR + 10.

      IF ( TL_VBAP-SPART EQ '02' OR
           TL_VBAP-SPART EQ '04' )
       AND IT_OV-VRKME EQ 'TO'.

        TL_BAPISDITM-TARGET_QTY   = WL_VLR_COVERT = ( IT_OV-ZMENG * 1000 ).
        TL_BAPISDITM-TARGET_QU    = 'KG'.
        TL_BAPISDITM-SALES_UNIT   = 'KG'.

      ELSE.
        TL_BAPISDITM-TARGET_QTY   = WL_VLR_COVERT = IT_OV-ZMENG.
        TL_BAPISDITM-TARGET_QU    = IT_OV-VRKME.
        TL_BAPISDITM-SALES_UNIT   = IT_OV-VRKME.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = IT_OV-MATNR
        IMPORTING
          OUTPUT = WL_MATERIAL.

      TL_BAPISDITM-MATERIAL     = WL_MATERIAL.
*      TL_BAPISDITM-TARGET_QTY   = IT_OV-ZMENG.
*      TL_BAPISDITM-TARGET_QU    = IT_OV-VRKME.
*      TL_BAPISDITM-SALES_UNIT   = IT_OV-VRKME.
      TL_BAPISDITM-USAGE_IND    = 'I'.
      TL_BAPISDITM-PLANT        = IT_OV-WERKS.
      TL_BAPISDITM-BATCH        = IT_OV-CHARG.
      TL_BAPISDITM-SHIP_POINT   = IT_OV-WERKS.
      TL_BAPISDITM-MATFRGTGRP   = '00000001'.
      APPEND TL_BAPISDITM.

      TL_BAPISDITMX-UPDATEFLAG   = 'I'.
      TL_BAPISDITMX-STORE_LOC    = 'X'.
      TL_BAPISDITMX-ITM_NUMBER   = TL_BAPISDITM-ITM_NUMBER.
      TL_BAPISDITMX-MATERIAL     = 'X'.
      TL_BAPISDITMX-TARGET_QTY   = 'X'.
      TL_BAPISDITMX-TARGET_QU    = 'X'.
      TL_BAPISDITMX-SALES_UNIT   = 'X'.
      TL_BAPISDITMX-USAGE_IND    = 'X'.
      TL_BAPISDITMX-PLANT        = 'X'.
      TL_BAPISDITMX-BATCH        = 'X'.
      TL_BAPISDITMX-SHIP_POINT   = 'X'.
      TL_BAPISDITMX-MATFRGTGRP   = 'X'.
      APPEND TL_BAPISDITMX.

      TL_SCHEDULE_LINES-ITM_NUMBER = TL_BAPISDITM-ITM_NUMBER.
*      TL_SCHEDULE_LINES-REQ_QTY    = IT_OV-ZMENG.
      TL_SCHEDULE_LINES-REQ_QTY    = WL_VLR_COVERT.
      TL_SCHEDULE_LINES-REQ_DLV_BL = '10'.

      APPEND TL_SCHEDULE_LINES.

      TL_SCHEDULE_LINESX-ITM_NUMBER  = TL_BAPISDITM-ITM_NUMBER.
      TL_SCHEDULE_LINESX-UPDATEFLAG  = 'I'.
      TL_SCHEDULE_LINESX-REQ_QTY     = 'X'.
      TL_SCHEDULE_LINESX-REQ_DLV_BL  = 'X'.
      APPEND TL_SCHEDULE_LINESX.

      TL_BAPICOND-ITM_NUMBER  = TL_BAPISDITM-ITM_NUMBER.
      TL_BAPICOND-CURRENCY    = TL_VBAP-WAERK.

      DATA(COEFICIENTE_O) = ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
                                                          _DIRECAO = 'O'
                                                          _VBELN   = IT_OV-VBELN
                                                          _POSNR   = IT_OV-POSNR
                                                       ).

      DATA(COEFICIENTE_D) = ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
                                                            _CLIENTE    = _VBAK-KUNNR
                                                            _FORNECEDOR = CONV #( |{ IT_OV-WERKS ALPHA = IN }| )
                                                            _MATERIAL   = IT_OV-MATNR
                                                            _TIPO_ORDEM = _VBAK-AUART
                                                            _DIRECAO    = 'D'
                                                            _WERKS      = IT_OV-WERKS  "<<RIM-SKM-IR120585-23.12.22
                                                         ).

      IF  COEFICIENTE_O NE COEFICIENTE_D.
        IF COEFICIENTE_D IS NOT INITIAL.
          TL_BAPICOND-COND_VALUE = IT_OV-NETPR * COEFICIENTE_D.
        ELSE.
          TL_BAPICOND-COND_VALUE  = IT_OV-NETPR.
        ENDIF.
      ELSE.
        TL_BAPICOND-COND_VALUE  = IT_OV-NETPR.
      ENDIF.

*      TL_BAPICOND-COND_VALUE  = IT_OV-NETPR.
*      TL_BAPICOND-COND_UNIT   = IT_OV-MEINS.
      TL_BAPICOND-COND_UNIT    =  IT_OV-VRKME.
      TL_BAPICOND-COND_TYPE   = 'PR00'.
      APPEND TL_BAPICOND.

      TL_BAPICONDX-ITM_NUMBER  = TL_BAPISDITM-ITM_NUMBER.
      TL_BAPICONDX-CURRENCY    = 'X'.
      TL_BAPICONDX-COND_VALUE  = 'X'.
      TL_BAPICONDX-COND_UNIT   = 'X'.
      TL_BAPICONDX-COND_TYPE   = 'X'.
      APPEND TL_BAPICONDX.

      IF NOT IT_OV-DIF_DESC IS INITIAL.

        IT_0041-VBELN = TL_VBAP-VBELN.
        IT_0041-POSNR = TL_BAPISDITM-ITM_NUMBER.
        IT_0041-MATNR = TL_BAPISDITM-MATERIAL.
        IT_0041-DESC_ABSOLUTO = IT_OV-DIF_DESC.

        APPEND IT_0041.

      ENDIF.


      WL_ORDERHEADERINX-UPDATEFLAG = 'U'.
*      WL_ORDERHEADERIN-SD_DOC_CAT  = 'C'.

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          SALESDOCUMENT     = TL_VBAP-VBELN
          ORDER_HEADER_IN   = WL_ORDERHEADERIN
          ORDER_HEADER_INX  = WL_ORDERHEADERINX
          BEHAVE_WHEN_ERROR = 'P'
        TABLES
          ORDER_ITEM_IN     = TL_BAPISDITM
          ORDER_ITEM_INX    = TL_BAPISDITMX
          SCHEDULE_LINES    = TL_SCHEDULE_LINES
          SCHEDULE_LINESX   = TL_SCHEDULE_LINESX
          CONDITIONS_IN     = TL_BAPICOND
          CONDITIONS_INX    = TL_BAPICONDX
          RETURN            = TL_RETURN.

      CLEAR:WL_RETURN.
      READ TABLE TL_RETURN INTO WL_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        IF NOT IT_0041 IS INITIAL.
          CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIMULADOR'
            TABLES
              TI_ITENS_OV       = IT_0041
            EXCEPTIONS
              OV_NAO_ENCONTRADA = 1
              OTHERS            = 2.
        ENDIF.

        NVBELN = TL_VBAP-VBELN.
        NPOSNR = TL_BAPISDITM-ITM_NUMBER.

        READ TABLE TL_RETURN INTO WL_RETURN WITH KEY TYPE   = 'S'
                                                     NUMBER = 311.
        IF SY-SUBRC = 0.
          APPEND WL_RETURN TO TE_RETURN.
        ENDIF.
      ELSE.
        APPEND LINES OF TL_RETURN TO TE_RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
