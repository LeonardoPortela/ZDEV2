*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0502.
*----------------------------------------------------------------------*

CLASS LCL_CALCULO_SALDO DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      CALC_SALDO IMPORTING REGISTRO TYPE ZDE_CTE_DIST_RATEI_ALV RETURNING VALUE(R_SALDO) TYPE ZDE_QT_CARGA_CTE_SINAL.
ENDCLASS.

CLASS LCL_CALCULO_SALDO IMPLEMENTATION.
  METHOD CALC_SALDO.
    R_SALDO = REGISTRO-RT_QT_ORIGEM - REGISTRO-RT_QT_UTILIZADA + REGISTRO-RT_QT_LIB_ANT - REGISTRO-RT_QT_VAGAO + REGISTRO-QT_DIFERENCA.
  ENDMETHOD.
ENDCLASS.


CLASS LCL_EVENT_RECEIVER_0502 DEFINITION.
  PUBLIC SECTION.
    DATA: ERROR_IN_DATA TYPE C.
    METHODS HANDLE_DATA_CHANGED_0502 FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING ER_DATA_CHANGED.
  PRIVATE SECTION.
    METHODS: PERFORM_SEMANTIC_CHECKS IMPORTING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
ENDCLASS.


TABLES: ZIB_CTE_DIST_LBP.

DATA: IT_RATEIO TYPE TABLE OF ZDE_CTE_DIST_RATEI_ALV WITH HEADER LINE.

DATA: CL_GRID_0502        TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER_0502      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      IT_FUNCTION_0502    TYPE UI_FUNCTIONS,
      WA_LAYOUT_0502      TYPE LVC_S_LAYO,
      IT_FIELDCAT_0502    TYPE LVC_T_FCAT,
      IT_SORT_0502        TYPE LVC_T_SORT,
      WA_VARIANT_0502     TYPE DISVARIANT,
      WA_STABLE_0502      TYPE LVC_S_STBL,
      IT_SELECTED_0502    TYPE LVC_T_ROW,
      WA_SELECTED_0502    TYPE LVC_S_ROW,
      IT_F4_0502          TYPE LVC_T_F4,
      WA_F4_0502          TYPE LVC_S_F4,
      EVENT_RECEIVER_0502 TYPE REF TO LCL_EVENT_RECEIVER_0502.

*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER_0502 IMPLEMENTATION.

  METHOD PERFORM_SEMANTIC_CHECKS.

    DATA: L_MENGE	  TYPE ZDE_QT_CARGA_CTE_SINAL,
          WA_RATEIO TYPE ZDE_CTE_DIST_RATEI_ALV.

    LOOP AT PR_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_GOOD) WHERE FIELDNAME EQ 'QT_DIFERENCA'.

      READ TABLE IT_RATEIO ASSIGNING FIELD-SYMBOL(<FS_RATEIO>) INDEX LS_GOOD-ROW_ID.

      IF LS_GOOD-VALUE IS NOT INITIAL.
        L_MENGE = CONV #( LS_GOOD-VALUE ).
        IF L_MENGE NE 0.

          WA_RATEIO = <FS_RATEIO>.
          WA_RATEIO-QT_DIFERENCA = L_MENGE.

          IF LCL_CALCULO_SALDO=>CALC_SALDO( WA_RATEIO ) < 0.

            ERROR_IN_DATA = ABAP_TRUE.
            MESSAGE S191(ZCTE_DIST) WITH <FS_RATEIO>-N55_CHAVE_ACESSO+25(09).
            SY-MSGTY = 'E'.
            CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
              EXPORTING
                I_MSGID     = SY-MSGID
                I_MSGNO     = SY-MSGNO
                I_MSGTY     = SY-MSGTY
                I_MSGV1     = SY-MSGV1
                I_MSGV2     = SY-MSGV2
                I_MSGV3     = SY-MSGV3
                I_MSGV4     = SY-MSGV4
                I_FIELDNAME = LS_GOOD-FIELDNAME
                I_ROW_ID    = LS_GOOD-ROW_ID.
          ELSE.
            <FS_RATEIO>-QT_DIFERENCA = L_MENGE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD HANDLE_DATA_CHANGED_0502.
    ERROR_IN_DATA = ABAP_FALSE.
    CALL METHOD PERFORM_SEMANTIC_CHECKS( ER_DATA_CHANGED ).
    IF ERROR_IN_DATA EQ ABAP_TRUE.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ELSE.
      PERFORM DATA_CHANGED_0502 USING ER_DATA_CHANGED.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0502 INPUT.

  DATA: QTD_DISTRIBUIDA LIKE ZDE_CTE_DIST_RATEI_ALV-RT_QT_DISTRIBUIR .

  CASE OK_CODE.

    WHEN OK_CONFIRMAR.

      CLEAR: OK_CODE.

      QTD_DISTRIBUIDA = 0.

      LOOP AT IT_RATEIO INTO DATA(WA_RATEIO_01).
        ADD WA_RATEIO_01-QT_DIFERENCA TO QTD_DISTRIBUIDA.
      ENDLOOP.

      IF QTD_DISTRIBUIDA NE WA_RATEIO_01-RT_QT_DISTRIBUIR.
        MESSAGE S192 WITH WA_RATEIO_01-RT_QT_DISTRIBUIR.
        EXIT.
      ENDIF.

      LOOP AT IT_RATEIO ASSIGNING FIELD-SYMBOL(<FS_RATEIO>).

        READ TABLE IT_ZIB_CTE_DIST_LBP
        WITH KEY CD_CHAVE_CTE     = <FS_RATEIO>-CD_CHAVE_CTE
                 N55_CHAVE_ACESSO = <FS_RATEIO>-N55_CHAVE_ACESSO
        ASSIGNING FIELD-SYMBOL(<FS_LBP>).

        IF SY-SUBRC IS INITIAL.
          <FS_LBP>-QT_DIFERENCA = <FS_RATEIO>-QT_DIFERENCA.
        ELSE.
          MOVE-CORRESPONDING <FS_RATEIO> TO ZIB_CTE_DIST_LBP.
          APPEND ZIB_CTE_DIST_LBP TO IT_ZIB_CTE_DIST_LBP.
        ENDIF.

      ENDLOOP.

      PERFORM LIMPAR_TELA_0502.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0502  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0502 OUTPUT.
  SET PF-STATUS 'PF0502'.
  SET TITLEBAR 'TL0502'.

  IF IT_RATEIO[] IS INITIAL.
    PERFORM PESQUISAR_NOTAS.
  ENDIF.

  IF CONTAINER_0502 IS INITIAL.

    CLEAR WA_LAYOUT_0502.
    WA_STABLE_0502-ROW        = ABAP_TRUE.
    WA_STABLE_0502-COL        = ABAP_TRUE.
    WA_LAYOUT_0502-SEL_MODE   = 'A'.

    CREATE OBJECT CONTAINER_0502
      EXPORTING
        CONTAINER_NAME = 'ALV_0502'.

    PERFORM FILL_IT_FIELDCATALOG_0502.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT_0502.

    CREATE OBJECT CL_GRID_0502
      EXPORTING
        I_PARENT = CONTAINER_0502.

    CL_GRID_0502->REGISTER_EDIT_EVENT( EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER ).
    CL_GRID_0502->REGISTER_EDIT_EVENT( EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).

    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO IT_FUNCTION_0502.
    APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO IT_FUNCTION_0502.

    CREATE OBJECT EVENT_RECEIVER_0502.
    SET HANDLER EVENT_RECEIVER_0502->HANDLE_DATA_CHANGED_0502 FOR CL_GRID_0502.

    CALL METHOD CL_GRID_0502->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT                    = WA_VARIANT_0502
        I_SAVE                        = 'A'
        IS_LAYOUT                     = WA_LAYOUT_0502
        IT_TOOLBAR_EXCLUDING          = IT_FUNCTION_0502
      CHANGING
        IT_OUTTAB                     = IT_RATEIO[]
        IT_FIELDCATALOG               = IT_FIELDCAT_0502
        IT_SORT                       = IT_SORT_0502
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  CALL METHOD CL_GRID_0502->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE_0502.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0502_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0502_EXIT INPUT.
  PERFORM LIMPAR_TELA_0502.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PESQUISAR_NOTAS .

  DATA(LC_CHAVE) = WA_CTE_SELECT-CD_CHAVE_CTE.

  TRY.
      EXEC SQL.
        OPEN DOCUMENTOS FOR
          SELECT TT.N55_CHAVE_ACESSO,
                 TT.QT_CARGA_CTE,
                 TT.QTD_VAGAO,
                 TT.QTD_ORIGEM,
                 TT.QTD_UTILIZADA,
                 TT.QTD_LIB_ANT,
                 CASE WHEN TT.QTD_ORIGEM > 0 THEN TT.QTD_ORIGEM - TT.QTD_UTILIZADA + TT.QTD_LIB_ANT ELSE 0 END QTD_SALDO,
                 CASE WHEN TT.QTD_ORIGEM > 0 THEN ( TT.QTD_ORIGEM - TT.QTD_UTILIZADA - TT.QTD_VAGAO + TT.QTD_LIB_ANT + TT.QTD_LIB_ATUAL ) ELSE 0 END QTD_FINAL,
                 TT.QT_CARGA_CTE,
                 TT.QT_CARGA_CTE - ( SUM( TT.QTD_VAGAO ) OVER ( PARTITION BY TT.CD_CHAVE_CTE ) ) AS QTD_DISTIBUIR,
                 TT.QTD_LIB_ATUAL
            FROM (
          SELECT O.N55_CHAVE_ACESSO,
                 O.CD_CHAVE_CTE,
                 T.QT_CARGA_CTE,
                 O.VALR_PESO_RATE * 1000 AS QTD_VAGAO,
                 IFNULL(
                 CASE WHEN IFNULL((SELECT TP_PESO_LIMITE FROM SAPHANADB.ZLEST0133 WHERE MANDT = '300' AND LIFNR = '0000140498'),'1') = '1' THEN
                        ( SELECT CH.QTD_CHEG FROM SAPHANADB.ZLEST0035 CH WHERE CH.MANDT    = '300'
                             AND CH.NR_NF    = SUBSTR(O.N55_CHAVE_ACESSO,26,09)
                             AND CH.SERIE_NF = SUBSTR(O.N55_CHAVE_ACESSO,23,03)
                             AND CH.CNPJ     = SUBSTR(O.N55_CHAVE_ACESSO,07,14))
                      ELSE ( SELECT PESOSAIDA FROM SAPHANADB.ZLEST0039 SA WHERE SA.MANDT   = NN.MANDT AND SA.DOCNUM  = NN.DOCNUM_NFE )
                 END,0) QTD_ORIGEM,

                 IFNULL( (SELECT SUM(UT.PESO_RATEADO)
                         FROM SAPHANADB.ZLEST0045        UT,
                              SAPHANADB.ZLEST0044        CB
                        WHERE UT.MANDT            =  NN.MANDT
                          AND UT.DOCNUM           =  NN.DOCNUM_NFE
                          AND UT.MANDT            =  CB.MANDT
                          AND UT.CHAVE_CTE        =  CB.CHAVE_CTE
                          AND UT.CHAVE_CTE        <> T.CD_CHAVE_CTE
                          AND TRIM(CB.NR_TRANS)   IS NOT NULL ),0) AS QTD_UTILIZADA,

                 IFNULL( (SELECT SUM(LB.QT_DIFERENCA)
                         FROM SAPHANADB.ZIB_CTE_DIST_LBP LB
                        WHERE LB.MANDT = O.MANDT
                          AND LB.N55_CHAVE_ACESSO = O.N55_CHAVE_ACESSO
                          AND LB.CD_CHAVE_CTE <> T.CD_CHAVE_CTE ) , 0 ) QTD_LIB_ANT,

                 IFNULL( (SELECT SUM(LB.QT_DIFERENCA)
                         FROM SAPHANADB.ZIB_CTE_DIST_LBP LB
                        WHERE LB.MANDT = O.MANDT
                          AND LB.N55_CHAVE_ACESSO = O.N55_CHAVE_ACESSO
                          AND LB.CD_CHAVE_CTE = T.CD_CHAVE_CTE ) , 0 ) QTD_LIB_ATUAL

            FROM SAPHANADB.ZIB_CTE_DIST_TER T,
                 SAPHANADB.ZIB_CTE_DIST_D55 O,
                 SAPHANADB.ZIB_CTE_DIST_N55 NN
           WHERE T.MANDT             = :SY-MANDT
             AND T.CD_CHAVE_CTE      = :LC_CHAVE
             AND T.MANDT             = O.MANDT
             AND T.CD_CHAVE_CTE      = O.CD_CHAVE_CTE
             AND T.MANDT             = NN.MANDT
             AND T.CD_CHAVE_CTE      = NN.CD_CHAVE_CTE
             AND O.N55_CHAVE_ACESSO  = NN.N55_CHAVE_ACESSO
             AND NOT EXISTS (SELECT * FROM SAPHANADB.ZLEST0041 CO
                              WHERE CO.MANDT            = '300'
                                AND CO.CENTRO_COMPRADOR = NN.BRANCH
                                AND CO.NR_NF            = SUBSTR(O.N55_CHAVE_ACESSO,26,09)
                                AND CO.COD_CLIENTE      = NN.PARID
                                AND CO.SERIE            = SUBSTR(O.N55_CHAVE_ACESSO,23,03)) ) TT
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
      DATA(ERROR_TEXT) = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
        :IT_RATEIO-N55_CHAVE_ACESSO,
        :IT_RATEIO-QT_CARGA_CTE,
        :IT_RATEIO-RT_QT_VAGAO,
        :IT_RATEIO-RT_QT_ORIGEM,
        :IT_RATEIO-RT_QT_UTILIZADA,
        :IT_RATEIO-RT_QT_LIB_ANT,
        :IT_RATEIO-RT_QT_SALDO,
        :IT_RATEIO-RT_QT_FINAL,
        :IT_RATEIO-RT_QT_CTE,
        :IT_RATEIO-RT_QT_DISTRIBUIR,
        :IT_RATEIO-QT_DIFERENCA
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
    IT_RATEIO-CD_CHAVE_CTE = LC_CHAVE.
    APPEND IT_RATEIO.
    CLEAR: IT_RATEIO.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_0502 .

  CLEAR: EVENT_RECEIVER_0502.

  IF CL_GRID_0502 IS NOT INITIAL.
    CL_GRID_0502->FREE( ).
  ENDIF.
  CLEAR: CL_GRID_0502.

  IF CONTAINER_0502 IS NOT INITIAL.
    CONTAINER_0502->FREE( ).
  ENDIF.
  CLEAR: CONTAINER_0502.

  CLEAR: IT_RATEIO[], IT_RATEIO, IT_FIELDCAT_0502[].
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  data_changed_0502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DATA_CHANGED_0502 USING  RR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI.
  DATA: LS_CELLS     TYPE LVC_S_MODI.
  DATA: L_MENGE	     TYPE ZDE_QT_CARGA_CTE_SINAL.

  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE IT_RATEIO ASSIGNING FIELD-SYMBOL(<FS_RATEIO>) INDEX LS_MOD_CELLS-ROW_ID.

    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'QT_DIFERENCA'.

        CALL METHOD RR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
          IMPORTING
            E_VALUE     = L_MENGE.

        IF L_MENGE NE 0.
          <FS_RATEIO>-QT_DIFERENCA = L_MENGE.
          <FS_RATEIO>-RT_QT_FINAL  = LCL_CALCULO_SALDO=>CALC_SALDO( <FS_RATEIO> ).

          WA_STABLE_0502-ROW = ABAP_TRUE.
          WA_STABLE_0502-COL = ABAP_TRUE.

          CALL METHOD CL_GRID_0502->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE      = WA_STABLE_0502
              I_SOFT_REFRESH = ABAP_TRUE.

          CALL METHOD CL_GUI_CFW=>FLUSH.

        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG_0502 .

  DATA: I_CONTADOR_2 TYPE LVC_COLPOS,
        I_CAMPO	     TYPE NAME_FELD,
        I_PROD_ITEM	 TYPE J_1BITMNUM,
        I_LOTE       TYPE CHARG_D.

  CLEAR: IT_FIELDCAT_0502[], IT_FIELDCAT_0502.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZDE_CTE_DIST_RATEI_ALV'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCAT_0502.

  I_CONTADOR_2 = 09.

  IF ( WA_CTE_0501-CK_FINALIZADO IS NOT INITIAL ) OR ( CK_VT_CRIADA IS NOT INITIAL ).
    DATA(CK_ALTERAR) = ABAP_FALSE.
  ELSE.
    CK_ALTERAR = ABAP_TRUE.
  ENDIF.

  LOOP AT IT_FIELDCAT_0502 ASSIGNING FIELD-SYMBOL(<FS_0502>).
    <FS_0502>-EDIT = ABAP_FALSE.
    CASE <FS_0502>-FIELDNAME.
      WHEN 'N55_CHAVE_ACESSO'.
        <FS_0502>-COL_POS   = 01.
        <FS_0502>-OUTPUTLEN = 44.
        <FS_0502>-SCRTEXT_L = 'Chave de Acesso NF-e'.
        <FS_0502>-SCRTEXT_M = 'NF-e'.
        <FS_0502>-SCRTEXT_S = 'NF-e'.
        <FS_0502>-REPTEXT   = 'NF-e'.
      WHEN 'RT_QT_ORIGEM'.
        <FS_0502>-COL_POS   = 02.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Origem'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Origem'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Origem'.
        <FS_0502>-REPTEXT   = 'Qtd.Origem'.
      WHEN 'RT_QT_UTILIZADA'.
        <FS_0502>-COL_POS   = 03.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Utilizada'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Utilizada'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Utilizada'.
        <FS_0502>-REPTEXT   = 'Qtd.Utilizada'.
      WHEN 'RT_QT_LIB_ANT'.
        <FS_0502>-COL_POS   = 04.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Liberada Anterior'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Lib.Anterior'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Lib.Anterior'.
        <FS_0502>-REPTEXT   = 'Qtd.Lib.Anterior'.
      WHEN 'RT_QT_SALDO'.
        <FS_0502>-COL_POS   = 05.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Saldo'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Saldo'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Saldo'.
        <FS_0502>-REPTEXT   = 'Qtd.Saldo'.
      WHEN 'RT_QT_VAGAO'.
        <FS_0502>-COL_POS   = 06.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Vagão'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Vagão'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Vagão'.
        <FS_0502>-REPTEXT   = 'Qtd.Vagão'.
      WHEN 'QT_DIFERENCA'.
        <FS_0502>-COL_POS   = 07.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Diferênça'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Diferênça'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Diferênça'.
        <FS_0502>-REPTEXT   = 'Qtd.Diferênça'.
        <FS_0502>-EDIT      = CK_ALTERAR.
      WHEN 'RT_QT_FINAL'.
        <FS_0502>-COL_POS   = 08.
        <FS_0502>-OUTPUTLEN = 12.
        <FS_0502>-SCRTEXT_L = 'Qtd.Final'.
        <FS_0502>-SCRTEXT_M = 'Qtd.Final'.
        <FS_0502>-SCRTEXT_S = 'Qtd.Final'.
        <FS_0502>-REPTEXT   = 'Qtd.Final'.
      WHEN OTHERS.
        <FS_0502>-NO_OUT    = ABAP_TRUE.
        <FS_0502>-COL_POS = I_CONTADOR_2.
        ADD 1 TO I_CONTADOR_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GS_VARIANT_0502 .

  WA_VARIANT_0502-REPORT      = SY-REPID.
  WA_VARIANT_0502-HANDLE      = '0502'.
  WA_VARIANT_0502-LOG_GROUP   = ABAP_FALSE.
  WA_VARIANT_0502-USERNAME    = ABAP_FALSE.
  WA_VARIANT_0502-VARIANT     = ABAP_FALSE.
  WA_VARIANT_0502-TEXT        = ABAP_FALSE.
  WA_VARIANT_0502-DEPENDVARS  = ABAP_FALSE.

ENDFORM.
