FUNCTION ZSDMF001_ATUALI_OV_SIM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SOMA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_AUART) TYPE  AUART OPTIONAL
*"     REFERENCE(I_ACAO) TYPE  SY-UCOMM OPTIONAL
*"     REFERENCE(I_0081) TYPE  CHAR1 OPTIONAL
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
        TL_CONDITIONS_IN   TYPE TABLE OF BAPICOND WITH HEADER LINE,
        TL_CONDITIONS_INX  TYPE TABLE OF BAPICONDX WITH HEADER LINE,
        TL_SAIDA_EXEC      TYPE TABLE OF TY_SAIDA_EXEC WITH HEADER LINE,
        WL_TABIX           TYPE SY-TABIX,
        V_SALDO            TYPE VBAP-KWMENG,
        CONT               TYPE C,
        REMESSA(1),
        V_CHECK            TYPE C.

  DATA: E_ROUTE  TYPE TROLZ-ROUTE,
        E_RETURN TYPE BAPIRET2.

  REFRESH: TL_VBAK, TL_VBAP, TL_VBEP.


  IF IT_OV[] IS NOT INITIAL.
    SELECT *
      FROM VBAP
      INTO TABLE TL_VBAP
       FOR ALL ENTRIES IN IT_OV
        WHERE VBELN EQ IT_OV-VBELN
        AND   POSNR EQ IT_OV-POSNR.
  ENDIF.

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
            AND POSNR EQ TL_VBAP-POSNR.

      LOOP AT TL_VBEP WHERE WMENG IS INITIAL.
        DELETE TL_VBEP WHERE VBELN EQ TL_VBEP-VBELN
                         AND POSNR EQ TL_VBEP-POSNR
                         AND ETENR EQ TL_VBEP-ETENR.
      ENDLOOP.

      READ TABLE TL_VBAK INDEX 1.

      WL_ORDERHEADERINX-UPDATEFLAG = 'U'.
      WL_LOGIC_SWITCH-COND_HANDL   = 'X'.

      LOOP AT TL_VBAP.

        V_CHECK = ABAP_FALSE.

        FREE: TL_BAPISDITM, TL_BAPISDITMX, TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX, TL_CONDITIONS_IN, TL_CONDITIONS_INX.
        CLEAR:  TL_BAPISDITM, TL_BAPISDITMX, TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX, TL_CONDITIONS_IN, TL_CONDITIONS_INX.

        READ TABLE IT_OV WITH KEY VBELN = TL_VBAP-VBELN
                                  POSNR = TL_VBAP-POSNR.
        IF SY-SUBRC IS INITIAL.
          WL_TABIX = SY-TABIX.
          IF I_SOMA = 'A'.
            V_SALDO = IT_OV-ZMENG.
          ELSEIF I_SOMA = 'X'.
            V_SALDO = TL_VBAP-KWMENG + IT_OV-ZMENG.
          ELSE.
            V_SALDO = TL_VBAP-KWMENG - IT_OV-ZMENG.

            IF V_SALDO EQ 0.
              V_SALDO = 1.
              V_CHECK = ABAP_TRUE.
            ENDIF.

          ENDIF.

          DATA(SALDO) = IT_OV-ZMENG.
          FIELD-SYMBOLS <VBEP> TYPE VBEP.
          CLEAR TL_VBEP.

          CASE I_ACAO.
            WHEN 'TRANSF' OR 'DESMEMBRAR' OR 'REDIST'.

              LOOP AT TL_VBEP ASSIGNING <VBEP> WHERE VBELN = TL_VBAP-VBELN AND
                                                     POSNR = TL_VBAP-POSNR.
                IF SALDO IS INITIAL.
                  CONTINUE.
                ENDIF.

                IF SALDO > <VBEP>-WMENG.
                  SALDO = SALDO - <VBEP>-WMENG.
                  <VBEP>-WMENG = 0.
                ELSE.
                  <VBEP>-WMENG = <VBEP>-WMENG - SALDO.

                  IF <VBEP>-WMENG EQ 0.
                    <VBEP>-WMENG = 1.
                  ENDIF.

                  SALDO = 0.
                ENDIF.
              ENDLOOP.

            WHEN 'ALTERAR'.

              LOOP AT TL_VBEP ASSIGNING <VBEP> WHERE VBELN = TL_VBAP-VBELN AND
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

              IF SALDO > 0.
                <VBEP>-WMENG = <VBEP>-WMENG + SALDO.
              ENDIF.
            WHEN 'R_ALTERA'.
              LOOP AT TL_VBEP ASSIGNING <VBEP> WHERE VBELN = TL_VBAP-VBELN AND
                                                     POSNR = TL_VBAP-POSNR.
                <VBEP>-WMENG = V_SALDO.
              ENDLOOP.

*              " 29.03.2023 - RAMON - 98623 -->
*            WHEN 'ESTORNAR'.
*
*              LOOP AT tl_vbep ASSIGNING <vbep> WHERE vbeln = tl_vbap-vbeln AND
*                                                     posnr = tl_vbap-posnr.
*                <vbep>-wmeng = v_saldo.
*              ENDLOOP.
*
*
*              " 29.03.2023 - RAMON - 98623 --<

          ENDCASE.

*          "FF #145609 - inicio
          IF I_0081 IS NOT INITIAL.

            DATA: LV_ITI_OV   TYPE VBAP-ROUTE,              "FF #145609
                  LV_ITI_ROTE TYPE TROLZ-ROUTE.             "FF #145609

            IMPORT _SAIDA-ITI_OV   TO LV_ITI_OV   FROM MEMORY ID 'memory_iti_ov'. "Export feito no programa ZSDR00388
            IMPORT _SAIDA-ITI_ROTE TO LV_ITI_ROTE FROM MEMORY ID 'memory_iti_rote'. "Export feito no programa ZSDR0038

            TL_BAPISDITMX-ROUTE = 'X'.
            TL_BAPISDITM-ROUTE = LV_ITI_ROTE.

*            READ TABLE tl_vbak WITH KEY vbeln = tl_vbap-vbeln INTO DATA(wa_vbak).
*            IF sy-subrc = 0.
*
*              SELECT SINGLE nr_rot FROM zsdt0132
*              WHERE kunnr = @wa_vbak-kunnr
*              INTO @DATA(lv_rot).
*
*              IF sy-subrc = 0.
*
*                CALL FUNCTION 'ZSD_BUSCA_ITINERARIO'
*                  EXPORTING
*                    i_roteiro = lv_rot
*                    i_vbeln   = wa_vbak-vbeln
*                  IMPORTING
*                    e_route   = e_route
*                    e_return  = e_return.
*
*                IF e_route IS NOT INITIAL.
*
*                  tl_bapisditmx-route = 'X'.
*                  tl_bapisditm-route = e_route.
*
*                ENDIF.
*              ENDIF.
*            ENDIF.
          ENDIF.
*          "FF #145609 - fim

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
              TL_CONDITIONS_IN-COND_UNIT = IT_KONV[ KPOSN = TL_VBAP-POSNR ]-KMEIN.

              TL_CONDITIONS_IN-COND_UNIT  = SWITCH #( TL_CONDITIONS_IN-COND_UNIT WHEN 'TO' THEN 'KG' ELSE TL_CONDITIONS_IN-COND_UNIT ).
              TL_CONDITIONS_INX-COND_UNIT = ABAP_TRUE.
            ENDIF.

            MOVE: TL_VBAP-POSNR TO TL_CONDITIONS_IN-ITM_NUMBER,
                  '01'          TO TL_CONDITIONS_IN-COND_COUNT,
                  'PR00'        TO TL_CONDITIONS_IN-COND_TYPE,
                  '0.1'           TO TL_CONDITIONS_IN-COND_VALUE,

                  TL_VBAP-POSNR TO TL_CONDITIONS_INX-ITM_NUMBER,
                  '01'          TO TL_CONDITIONS_INX-COND_COUNT,
                  'PR00'        TO TL_CONDITIONS_INX-COND_TYPE,
                  'X'           TO TL_CONDITIONS_INX-COND_VALUE,
                  'U'           TO TL_CONDITIONS_INX-UPDATEFLAG.

            APPEND: TL_CONDITIONS_IN,
                    TL_CONDITIONS_INX.

            CLEAR: TL_CONDITIONS_IN, TL_CONDITIONS_INX.

            APPEND VALUE #(
                            ITM_NUMBER = TL_VBAP-POSNR
                            COND_COUNT = '01'
                            COND_TYPE  = 'RB00'
                            COND_VALUE = 0
                            COND_UNIT  = TL_CONDITIONS_IN-COND_UNIT
                            CURRENCY   = TL_VBAK-WAERK
                          ) TO TL_CONDITIONS_IN[].

            APPEND VALUE #(
                            ITM_NUMBER = TL_VBAP-POSNR
                            COND_COUNT = '01'
                            COND_TYPE  = 'RB00'
                            COND_VALUE = ABAP_TRUE
                            COND_UNIT  = ABAP_TRUE
                            UPDATEFLAG = 'U'
                            CURRENCY   = ABAP_TRUE
                          ) TO TL_CONDITIONS_INX[].

          ENDIF.

          IF I_ACAO EQ 'ESTORNAR'.

            TL_CONDITIONS_IN[] = VALUE #( (
                                            ITM_NUMBER = TL_VBAP-POSNR
                                            COND_COUNT = '01'
                                            COND_TYPE  = 'PR00'
                                            COND_VALUE = IT_OV-NETPR
                                            COND_UNIT  = IT_OV-VRKME
                                            CURRENCY   = TL_VBAK-WAERK
                                      ) ).

            TL_CONDITIONS_INX[] = VALUE #( (
                                             ITM_NUMBER = TL_VBAP-POSNR
                                             COND_COUNT = '01'
                                             COND_TYPE  = 'PR00'
                                             COND_VALUE = ABAP_TRUE
                                             COND_UNIT  = ABAP_TRUE
                                             UPDATEFLAG = 'U'
                                             CURRENCY   = ABAP_TRUE
                                         ) ).

          ENDIF.

*          CASE I_AUART.
*            WHEN 'ZOSM' OR 'ZODF' OR 'ZOFE'.
*
*              LOOP AT TL_VBEP WHERE VBELN EQ TL_VBAP-VBELN
*                               AND  POSNR EQ TL_VBAP-POSNR
*                               AND  ETENR EQ '1'.
*
*                MOVE: 'U'                TO TL_SCHEDULE_LINESX-UPDATEFLAG,
*                      TL_VBAP-POSNR      TO TL_SCHEDULE_LINESX-ITM_NUMBER,
*                      TL_VBEP-ETENR      TO TL_SCHEDULE_LINESX-SCHED_LINE,
*                      'X'                TO TL_SCHEDULE_LINESX-REQ_QTY,
*                      TL_VBAP-POSNR      TO TL_SCHEDULE_LINES-ITM_NUMBER,
*                      TL_VBEP-ETENR      TO TL_SCHEDULE_LINES-SCHED_LINE,
*                      V_SALDO            TO TL_SCHEDULE_LINES-REQ_QTY.
*
*
*                APPEND TL_SCHEDULE_LINESX.
*                APPEND TL_SCHEDULE_LINES.
*                CLEAR: TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX.
*              ENDLOOP.
*
*            WHEN OTHERS.


          SELECT COUNT(*)
            FROM VBFA
            WHERE VBELV EQ TL_VBAP-VBELN
            AND POSNV EQ TL_VBAP-POSNR
            AND VBTYP_N = 'J'
            AND VBTYP_V = 'C'.

          IF SY-SUBRC IS INITIAL.
            REMESSA = ABAP_TRUE.

            " 27.03.2023 - RAMON - 98623 -->
            " QUANDO LE O ITEM 0010 JA FATURADO, E O PROXIMO NAO ESTAVA,
            " A VARIVEL REMESSA ESTAVA COMO 'X'.
          ELSE.
            REMESSA = ABAP_FALSE.
            " 27.03.2023 - RAMON - 98623 --<
          ENDIF.

          CLEAR CONT.
          LOOP AT TL_VBEP WHERE VBELN EQ TL_VBAP-VBELN
             AND  POSNR EQ TL_VBAP-POSNR.

            IF I_ACAO EQ 'ESTORNAR'.

              IF CONT IS INITIAL.

                IF TL_VBEP-LIFSP EQ '12'.
                  MOVE IT_OV-ZMENG      TO TL_SCHEDULE_LINES-REQ_QTY.
                ELSE.
                  ADD IT_OV-ZMENG         TO TL_VBEP-WMENG.
                  MOVE TL_VBEP-WMENG      TO TL_SCHEDULE_LINES-REQ_QTY.
                ENDIF.

                CONT = ABAP_TRUE.

              ELSE.
                MOVE TL_VBEP-WMENG      TO TL_SCHEDULE_LINES-REQ_QTY.
              ENDIF.

*              MOVE: ABAP_TRUE     TO TL_SCHEDULE_LINESX-REQ_DLV_BL.
*                    '10'          TO TL_SCHEDULE_LINES-REQ_DLV_BL.
              TL_SCHEDULE_LINESX-REQ_DLV_BL = COND #( WHEN REMESSA IS INITIAL THEN ABAP_TRUE ).
              TL_SCHEDULE_LINES-REQ_DLV_BL = COND #( WHEN REMESSA IS INITIAL THEN '10' ).

            ELSE.
              MOVE TL_VBEP-WMENG      TO TL_SCHEDULE_LINES-REQ_QTY.
            ENDIF.

            MOVE: 'U'                TO TL_SCHEDULE_LINESX-UPDATEFLAG,
                  TL_VBAP-POSNR      TO TL_SCHEDULE_LINESX-ITM_NUMBER,
                  TL_VBEP-ETENR      TO TL_SCHEDULE_LINESX-SCHED_LINE,
                  'X'                TO TL_SCHEDULE_LINESX-REQ_QTY,
                  TL_VBAP-POSNR      TO TL_SCHEDULE_LINES-ITM_NUMBER,
                  TL_VBEP-ETENR      TO TL_SCHEDULE_LINES-SCHED_LINE.

            IF V_CHECK EQ ABAP_TRUE.
*              MOVE: ABAP_TRUE     TO TL_SCHEDULE_LINESX-REQ_DLV_BL.
*                    '12'          TO TL_SCHEDULE_LINES-REQ_DLV_BL.
              TL_SCHEDULE_LINESX-REQ_DLV_BL = COND #( WHEN REMESSA IS INITIAL THEN ABAP_TRUE ).
              TL_SCHEDULE_LINES-REQ_DLV_BL = COND #( WHEN REMESSA IS INITIAL THEN '12' ).

            ENDIF.

            APPEND TL_SCHEDULE_LINESX.
            APPEND TL_SCHEDULE_LINES.

            CLEAR: TL_SCHEDULE_LINES, TL_SCHEDULE_LINESX.

          ENDLOOP.

*          ENDCASE.

*          endif.
          "*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
          CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              SALESDOCUMENT    = TL_VBAP-VBELN
              ORDER_HEADER_IN  = WL_ORDERHEADERIN
              ORDER_HEADER_INX = WL_ORDERHEADERINX
              LOGIC_SWITCH     = WL_LOGIC_SWITCH
            TABLES
              ORDER_ITEM_IN    = TL_BAPISDITM
              ORDER_ITEM_INX   = TL_BAPISDITMX
              CONDITIONS_IN    = TL_CONDITIONS_IN
              CONDITIONS_INX   = TL_CONDITIONS_INX
              SCHEDULE_LINES   = TL_SCHEDULE_LINES
              SCHEDULE_LINESX  = TL_SCHEDULE_LINESX
              RETURN           = TL_RETURN.

          CLEAR:WL_RETURN.
          READ TABLE TL_RETURN INTO WL_RETURN WITH KEY TYPE = 'E'.

          IF SY-SUBRC NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

*"// US-169490 WBARBOSA 12/08/2025 INICIO
            CALL METHOD ZCL_MANUTENCAO_INSUMOS=>SET_DESCONTO_ABS
              EXPORTING
                I_VBELN        = TL_VBAP-VBELN
                I_POSNR        = TL_VBAP-POSNR
                I_DESCONTO_ABS = 0
                I_ZERAR        = ABAP_TRUE.

            CALL METHOD ZCL_MANUTENCAO_INSUMOS=>SET_DESCONTO_ABS_OV
              EXPORTING
                I_VBELN = TL_VBAP-VBELN
                I_POSNR = TL_VBAP-POSNR.
*"// US-169490 WBARBOSA 12/08/2025 FIM

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
    ENDIF.
  ENDIF.

ENDFUNCTION.
