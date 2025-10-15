

CLASS LCL_LISTENER DEFINITION.
  PUBLIC SECTION.
    INTERFACES :
      IF_SALV_GUI_OM_EDIT_STRCT_LSTR.
ENDCLASS.

CLASS LCL_LISTENER IMPLEMENTATION.
  METHOD IF_SALV_GUI_OM_EDIT_STRCT_LSTR~ON_CHECK_CHANGED_DATA.
    O_UI_DATA_MODIFY->GET_UI_CHANGES( IMPORTING T_MODIFIED_CELLS = DATA(LT_MODIFIED) ).
  ENDMETHOD.
ENDCLASS.


CLASS LCL_REPORT DEFINITION.
  PUBLIC SECTION .
    METHODS:
      GET_DATA IMPORTING I_REFRESH TYPE CHAR1 OPTIONAL,
      GENERATE_OUTPUT,
      SET_HANDLER
        CHANGING CO_ALV    TYPE REF TO CL_SALV_TABLE
                 CO_REPORT TYPE REF TO LCL_REPORT,
      SET_REFRESH
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE,
      SET_COLUMNS_BUILD
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE,
      SET_PF_STATUS
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE,
      SET_LAYOUT
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE,
      SET_EDIT_ALV
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE,
      ON_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING
          E_OBJECT
          E_INTERACTIVE
          SENDER.

    METHODS: ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS IMPORTING E_SALV_FUNCTION SENDER,
      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE IMPORTING ROW COLUMN,
      ON_CHANGE_DATA FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING E_ONF4 E_ONF4_AFTER E_ONF4_BEFORE E_UCOMM ER_DATA_CHANGED SENDER.

ENDCLASS.
CLASS LCL_REPORT IMPLEMENTATION.
  METHOD SET_REFRESH.

    CALL METHOD LO_REPORT->GET_DATA
      EXPORTING
        I_REFRESH = ABAP_TRUE.

    CO_ALV->REFRESH( ).

  ENDMETHOD.

  METHOD ON_CHANGE_DATA.

    DATA: IT_CHANGED TYPE STANDARD TABLE OF ZFISE46 INITIAL SIZE 0.

    DATA(INSERTED_TAB) = ER_DATA_CHANGED->MT_INSERTED_ROWS.
    DATA(DELETED_TAB)  = ER_DATA_CHANGED->MT_DELETED_ROWS.

    FIELD-SYMBOLS: <ITAB>        TYPE ANY TABLE,
                   <STRUCT>      TYPE ANY,
                   <IT_MOD_ROWS> TYPE ANY TABLE,
                   <WA_MOD_ROWS> TYPE ANY.


    DATA: LT_GOOD_CELLS TYPE LVC_T_MODI,
          LS_GOOD_CELL  TYPE LVC_S_MODI.


    ASSIGN ER_DATA_CHANGED->MP_MOD_ROWS->* TO <IT_MOD_ROWS>.
    MOVE-CORRESPONDING <IT_MOD_ROWS> TO IT_CHANGED.

    LT_GOOD_CELLS = ER_DATA_CHANGED->MT_GOOD_CELLS.

  ENDMETHOD.

  METHOD SET_COLUMNS_BUILD .
*
*...Get all the Columns
    DATA: LO_COLS   TYPE REF TO CL_SALV_COLUMNS,
          LO_COLUMN TYPE REF TO CL_SALV_COLUMN.

    LO_COLS = CO_ALV->GET_COLUMNS( ).
    LO_COLS->SET_OPTIMIZE( ABAP_FALSE ).


    DATA: LO_COLS_REF  TYPE        SALV_T_COLUMN_REF,
          LO_COLS_LIST TYPE REF TO CL_SALV_COLUMN_LIST,
          LO_COL_LIST  LIKE LINE OF LO_COLS_REF.

    LO_COLS = O_ALV->GET_COLUMNS( ).
    DATA: LS_DDIC_F4_REF TYPE SALV_S_DDIC_REFERENCE.

    LO_COLS_REF    = LO_COLS->GET( ).

    TRY.

        LOOP AT LO_COLS_REF INTO LO_COL_LIST.
          LO_COLS_LIST ?= LO_COL_LIST-R_COLUMN.    "Narrow casting
          CLEAR: LS_DDIC_F4_REF.
          CASE LO_COL_LIST-COLUMNNAME.
            WHEN 'BUKRS' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'Empresa' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Empresa' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Empresa' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>LEFT ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'WERKS'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Filial' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Filial' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Filial' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>LEFT ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'BUTXT'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Nm.Empresa' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Nome Empresa' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Nome Empresa' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '20' ).
            WHEN 'NAME1'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Nm.Filial' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Nome da Filial' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Nome da Filial' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '20' ).
            WHEN 'PERIODO'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Mês\Ano' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Mês\Ano' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Mês\Ano' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
              LO_COLS_LIST->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
            WHEN 'SEQITEM' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'Seq.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Seq.' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Seq.' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>LEFT ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '04' ).
            WHEN 'STATUS_LANC'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Status' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Status' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Status' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '06' ).
              LO_COLS_LIST->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
            WHEN 'LOTE'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Lote' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Lote' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Lote' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'DT_DOC'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Dt.Doc.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Data Doc.' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Data Documento' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'DT_LANC'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Dt.Lanc.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Data Lanc.' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Data Lançamento' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'SAKNR'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Ct.Razão' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Conta Razão' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Conta Razão' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
              LS_DDIC_F4_REF = VALUE #( TABLE  = 'ZFIT0219'  FIELD = 'SAKNR').
              LO_COLS_LIST->SET_DDIC_REFERENCE( VALUE = LS_DDIC_F4_REF ).
              LO_COLS_LIST->SET_F4( IF_SALV_C_BOOL_SAP=>TRUE ).
            WHEN 'TXT50'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'DescRazão ' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Descrição Razão' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Descrição Razão' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN 'KOSTL'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Cent.Cust.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Centro Custo' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Centro de Custo' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
              LS_DDIC_F4_REF = VALUE #( TABLE  = 'ZFIT0218'  FIELD = 'KOSTL').
              LO_COLS_LIST->SET_DDIC_REFERENCE( VALUE = LS_DDIC_F4_REF ).
              LO_COLS_LIST->SET_F4( IF_SALV_C_BOOL_SAP=>TRUE ).
            WHEN 'AUFNR'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Ordem' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Ordem' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Ordem' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
            WHEN'DESC_FORNEC' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'For.Desc.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Forne Descrição' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Fornecedor (Descrição)' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '25' ).

            WHEN'DESC_DESP_REC' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'Desc.D\C' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Desc.Despesa\Receita' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Descrição Despesa\Receita' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '30' ).

            WHEN'NR_DOC' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'Nro.Doc.' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Nro.Doc.' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Nro.Doc.' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).

            WHEN'DMBTR'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Valor BRL' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Valor BRL' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Valor BRL' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).

            WHEN'ESTORNO'.
              LO_COLS_LIST->SET_SHORT_TEXT( 'Estornado' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Estornado' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Estornado' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '9' ).

            WHEN'D_C' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'D\C' ).
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'D\C' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'D\C' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              LO_COLS_LIST->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '3' ).
              LS_DDIC_F4_REF = VALUE #( TABLE  = 'ZFIT0217'  FIELD = 'D_C').
              LO_COLS_LIST->SET_DDIC_REFERENCE( VALUE = LS_DDIC_F4_REF ).
              LO_COLS_LIST->SET_F4( IF_SALV_C_BOOL_SAP=>TRUE ).

            WHEN 'DOC_CONT' .
              LO_COLS_LIST->SET_SHORT_TEXT( 'Doc.Cont.').
              LO_COLS_LIST->SET_MEDIUM_TEXT( 'Doc.Contabil' ).
              LO_COLS_LIST->SET_LONG_TEXT( 'Doc.Contabil' ).
              LO_COLS_LIST->SET_OPTIMIZED( ABAP_FALSE ).
              "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              LO_COLS_LIST->SET_OUTPUT_LENGTH( '10' ).
              LO_COLS_LIST->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
            WHEN OTHERS.
              LO_COLS_LIST->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          ENDCASE.
        ENDLOOP.
      CATCH CX_SALV_NOT_FOUND.
    ENDTRY.
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='BUKRS' POSITION = 1 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='BUTXT' POSITION = 2 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='WERKS' POSITION = 3 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='NAME1' POSITION = 4 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='PERIODO' POSITION = 5 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='STATUS' POSITION = 6 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='STATUS_LANC' POSITION = 7 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DOC_CONT' POSITION = 8 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='SEQITEM' POSITION = 9 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DT_DOC' POSITION = 10 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DT_LANC' POSITION = 11 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='SAKNR' POSITION = 12 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='TXT50' POSITION = 13 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='D_C' POSITION = 14 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='KOSTL' POSITION = 15 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='AUFNR' POSITION = 16 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DESC_FORNEC' POSITION = 17 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DESC_DESP_REC' POSITION = 18 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='NR_DOC' POSITION = 19 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='DMBTR' POSITION = 20 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='ESTORNO' POSITION = 21 ).
    LO_COLS->SET_COLUMN_POSITION( COLUMNNAME ='LOTE' POSITION = 22 ).

  ENDMETHOD.

  METHOD SET_EDIT_ALV.
    DATA:LS_API  TYPE REF TO IF_SALV_GUI_OM_EXTEND_GRID_API,
         LS_EDIT TYPE REF TO IF_SALV_GUI_OM_EDIT_RESTRICTED.

    DATA: LV_REF_TABLE  TYPE REF TO CL_ABAP_TABLEDESCR,
          LV_REF_STRUCT TYPE REF TO CL_ABAP_STRUCTDESCR.

    LS_API = O_ALV->EXTENDED_GRID_API( ).
    LS_EDIT = LS_API->EDITABLE_RESTRICTED( ).
    LV_REF_TABLE  ?= CL_ABAP_TABLEDESCR=>DESCRIBE_BY_DATA( IT_SAIDA ).
    LV_REF_STRUCT ?= LV_REF_TABLE->GET_TABLE_LINE_TYPE( ).
    DATA(LT_DETAILS)   = LV_REF_STRUCT->COMPONENTS.

    LOOP AT LT_DETAILS ASSIGNING FIELD-SYMBOL(<_DETAILS>).
      LS_EDIT->SET_ATTRIBUTES_FOR_COLUMNNAME(
        EXPORTING
          COLUMNNAME              = <_DETAILS>-NAME
          ALL_CELLS_INPUT_ENABLED = ABAP_TRUE
      ).

    ENDLOOP.

    DATA(MO_LISTENER) = NEW LCL_LISTENER( ).
    LS_EDIT->SET_LISTENER( MO_LISTENER ).
    LS_EDIT->VALIDATE_CHANGED_DATA(
).

    O_ALV->REFRESH( ).
  ENDMETHOD.

  METHOD GET_DATA.

    FREE: IT_SAIDA.
    CLEAR: WA_SAIDA.

    SELECT
      A~BUKRS,
      B~BUTXT,
      A~WERKS,
      C~NAME1,
      A~MONAT,
      A~GJAHR,
      A~SEQITEM,
      A~DT_DOC,
      A~DT_LANC,
      A~SAKNR,
      A~TXT50,
      A~D_C,
      A~KOSTL,
      A~AUFNR,
      A~DESC_FORNEC,
      A~DESC_DESP_REC,
      A~NR_DOC,
      A~DMBTR,
      A~ESTORNO,
      A~LOTE,
      A~STATUS,
      A~OBJ_KEY
    FROM
      ZFIT0217 AS A
      LEFT JOIN T001 AS B ON A~BUKRS = B~BUKRS
      LEFT JOIN T001W AS C ON A~WERKS = C~WERKS
            WHERE 1 = 1
      "AND a~bukrs IN @p_bukrs
      AND A~WERKS IN @P_WERKS
      "AND a~gjahr IN @p_gjahr
      AND A~MONAT IN @P_MONAT
      INTO TABLE @DATA(IT_SAP).

    IF IT_SAP IS INITIAL.
      IF I_REFRESH IS INITIAL.
        MESSAGE 'Não foram encontrados dados para esta selecão!' TYPE 'S'.
        EXIT.
      ENDIF.
    ELSE.
      LOOP AT IT_SAP ASSIGNING FIELD-SYMBOL(<_MOVE_LANC>).
        MOVE-CORRESPONDING <_MOVE_LANC> TO WA_SAIDA.
        WA_SAIDA-PERIODO = |{ <_MOVE_LANC>-MONAT }/{ <_MOVE_LANC>-GJAHR }|.
        CASE <_MOVE_LANC>-STATUS.
          WHEN 'A'.
            WA_SAIDA-STATUS_LANC = '@S_TL_G@'.
            IF <_MOVE_LANC>-OBJ_KEY IS NOT INITIAL.
              SELECT SINGLE BELNR FROM ZIB_CONTABIL_CHV WHERE OBJ_KEY = @<_MOVE_LANC>-OBJ_KEY INTO @DATA(L_BELNR).
              IF SY-SUBRC = 0.
                WA_SAIDA-DOC_CONT = L_BELNR.
              ELSE.
                SELECT SINGLE * FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = @<_MOVE_LANC>-OBJ_KEY INTO @DATA(L_ERR).
                IF SY-SUBRC = 0.
                  WA_SAIDA-STATUS_LANC = '@S_TL_R@'.
                ELSE.
                  WA_SAIDA-STATUS_LANC = '@AH@'.
                ENDIF.
              ENDIF.
            ENDIF.
          WHEN 'L'.
            WA_SAIDA-STATUS_LANC = '@S_TL_Y@'.
          WHEN 'R'.
            WA_SAIDA-STATUS_LANC = '@S_NONO@'.
          WHEN OTHERS.
            WA_SAIDA-STATUS_LANC = '@OUTLIG@'.
        ENDCASE.
        APPEND WA_SAIDA TO IT_SAIDA.
        CLEAR WA_SAIDA.
      ENDLOOP.

      FREE:IT_SAP.
      CLEAR: IT_SAP, IT_SAP[],WA_SAIDA.
    ENDIF.

  ENDMETHOD.


  METHOD SET_PF_STATUS.

    DATA: LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
    LO_FUNCTIONS = CO_ALV->GET_FUNCTIONS( ).
    LO_FUNCTIONS->SET_ALL( ABAP_TRUE ).
    LO_FUNCTIONS->SET_DEFAULT( ABAP_TRUE ).

    TRY.
        LO_FUNCTIONS->ADD_FUNCTION( NAME     = 'RECLASSIFICAR_LANC'
                                    ICON     = '@9A@'
                                    TEXT     = 'Reclassificar Lançamento'
                                    TOOLTIP  = 'Reclassificar Lançamento'
                                    POSITION = IF_SALV_C_FUNCTION_POSITION=>RIGHT_OF_SALV_FUNCTIONS ).

        LO_FUNCTIONS->ADD_FUNCTION( NAME     = 'ELIMINAR_LANC'
                                    ICON     = '@02@'
                                    TEXT     = 'Eliminar Lançamento'
                                    TOOLTIP  = 'Eliminar Lançamento'
                                    POSITION = IF_SALV_C_FUNCTION_POSITION=>RIGHT_OF_SALV_FUNCTIONS ).

      CATCH CX_ROOT.

    ENDTRY.


  ENDMETHOD.

  METHOD ON_USER_COMMAND.

    DATA: LO_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.
    DATA LT_ROWS TYPE SALV_T_ROW.
    DATA LT_COLUMNS TYPE SALV_T_COLUMN.
    DATA LT_CELLS TYPE SALV_T_CELL.
    DATA QTD_ROWS TYPE INT4.

    FREE: LT_ROWS.
    CLEAR: QTD_ROWS.

    LO_SELECTIONS = O_ALV->GET_SELECTIONS( ).
    LT_ROWS = LO_SELECTIONS->GET_SELECTED_ROWS( ).
    QTD_ROWS = LINES( LT_ROWS ).


    CASE E_SALV_FUNCTION.

      WHEN 'RECLASSIFICAR_LANC'.

        IF QTD_ROWS > 0.
          LOOP AT LT_ROWS ASSIGNING FIELD-SYMBOL(<_INDEX1>).
            READ TABLE IT_SAIDA ASSIGNING FIELD-SYMBOL(<_RECLA_LANC>) INDEX <_INDEX1>.
            IF <_RECLA_LANC>-STATUS = '' OR <_RECLA_LANC>-STATUS = 'L'.

              DATA:_DT  TYPE D,
                   _NDT TYPE D.
              CLEAR: _DT,_NDT.
              _DT = |{ <_RECLA_LANC>-GJAHR }{ <_RECLA_LANC>-MONAT }01|.

              CALL FUNCTION 'HR_PSD_DATES_ADD_MONTHS'
                EXPORTING
                  V_DATE   = _DT
                  V_MONTHS = 1
                IMPORTING
                  E_DATE   = _NDT.
              IF SY-SUBRC = 0.
                DATA: _ANO TYPE GJAHR,
                      _MES TYPE MONAT.

                _ANO = _NDT+0(4).
                _MES = _NDT+4(2).
                UPDATE ZFIT0217 SET LOTE = ''  GJAHR = _ANO MONAT = _MES
                WHERE BUKRS = <_RECLA_LANC>-BUKRS AND WERKS = <_RECLA_LANC>-WERKS AND MONAT = <_RECLA_LANC>-MONAT AND GJAHR = <_RECLA_LANC>-GJAHR AND SEQITEM = <_RECLA_LANC>-SEQITEM.
                COMMIT WORK.
                IF SY-SUBRC = 0.
                  MESSAGE 'Reclassificação transferida para o próximo mês!' TYPE 'I'.
                ENDIF.
              ENDIF.

            ENDIF.
          ENDLOOP.
          LO_REPORT->GET_DATA( ).
          CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.
        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

      WHEN 'ELIMINAR_LANC'.

        IF QTD_ROWS > 0.
          LOOP AT LT_ROWS ASSIGNING FIELD-SYMBOL(<_INDEX2>).
            READ TABLE IT_SAIDA ASSIGNING FIELD-SYMBOL(<_DEL_LANC>) INDEX <_INDEX2>.
            IF <_DEL_LANC>-STATUS = '' OR <_DEL_LANC>-STATUS = 'L'.
              DATA WA_ZFIT0217 TYPE ZFIT0217.
              CLEAR: WA_ZFIT0217.
              MOVE-CORRESPONDING <_DEL_LANC> TO WA_ZFIT0217.
              DELETE ZFIT0217 FROM WA_ZFIT0217.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
          LO_REPORT->GET_DATA( ).
          CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.
        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

        "CALL METHOD set_refresh CHANGING co_alv = o_alv.

      WHEN 'GRAVAR'.

        IF IT_SAIDA IS NOT INITIAL.
          LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<_READ>).
            "MODIFY zmmt0185 FROM <_read>.
          ENDLOOP.
        ELSE.
          MESSAGE 'Teste' TYPE 'I'.
        ENDIF.

      WHEN 'DELETE_ROW'.

        IF QTD_ROWS > 0.
          LOOP AT LT_ROWS ASSIGNING FIELD-SYMBOL(<_INDEX>).
            READ TABLE IT_SAIDA ASSIGNING FIELD-SYMBOL(<_DEL>) INDEX <_INDEX>.
            "DELETE zmmt0185 FROM <_del>.
          ENDLOOP.

        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

        CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.

      WHEN 'REFRESH_ROW'.
        CALL METHOD SET_REFRESH CHANGING CO_ALV = O_ALV.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD ON_TOOLBAR.

    DATA : MT_TOOLBAR TYPE STB_BUTTON.

    CLEAR MT_TOOLBAR.
    MT_TOOLBAR-BUTN_TYPE = '3'.   "separator
    APPEND MT_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING FIELD-SYMBOL(<FS_TOLLBAR>).
      "3 DESABILITA E 0 HABILITA
      IF  <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&COPY_ROW'.
        <FS_TOLLBAR>-BUTN_TYPE = '3'.
      ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&CREATE_ROW'.
        "<fs_tollbar>-butn_type = '3'.
      ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&APPEND'.
        <FS_TOLLBAR>-BUTN_TYPE = '3'.
      ENDIF.
      IF <FS_TOLLBAR>-FUNCTION EQ '&REFRESH'.
        <FS_TOLLBAR>-FUNCTION = 'REFRESH_ROW'.
      ELSEIF <FS_TOLLBAR>-FUNCTION EQ '&LOCAL&DELETE_ROW'.
        <FS_TOLLBAR>-FUNCTION = 'DELETE_ROW'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD GENERATE_OUTPUT.

    CONTAINER_MAIN = NEW CL_GUI_CUSTOM_CONTAINER(
      PARENT         = CL_GUI_CONTAINER=>DEFAULT_SCREEN
      "lifetime       =  cl_gui_container=>lifetime_dynpro
      CONTAINER_NAME = 'CONTAINER'
    ).

    DATA: LX_MSG TYPE REF TO CX_SALV_MSG.


    TRY.
        CL_SALV_TABLE=>FACTORY(
          EXPORTING
            R_CONTAINER    = CONTAINER_MAIN
            CONTAINER_NAME = 'CONTAINER'
          IMPORTING
            R_SALV_TABLE   = O_ALV
          CHANGING
            T_TABLE        = IT_SAIDA ).
      CATCH CX_SALV_MSG INTO LX_MSG.
    ENDTRY.

    CALL METHOD SET_PF_STATUS
      CHANGING
        CO_ALV = O_ALV.

    CALL METHOD SET_LAYOUT
      CHANGING
        CO_ALV = O_ALV.

    CALL METHOD SET_HANDLER
      CHANGING
        CO_ALV    = O_ALV
        CO_REPORT = LO_REPORT.

    CALL METHOD ME->SET_COLUMNS_BUILD
      CHANGING
        CO_ALV = O_ALV.

    DATA LR_SELECTIONS        TYPE REF TO CL_SALV_SELECTIONS.
* Enable cell selection mode
    LR_SELECTIONS = O_ALV->GET_SELECTIONS( ).
    LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).

    O_ALV->DISPLAY( ).

  ENDMETHOD.

  METHOD SET_HANDLER.
*
*...HotSpot
    DATA: LO_COLS_TAB TYPE REF TO CL_SALV_COLUMNS_TABLE,
          LO_COL_TAB  TYPE REF TO CL_SALV_COLUMN_TABLE,
          LO_EVENTS   TYPE REF TO CL_SALV_EVENTS_TABLE.

    LO_COLS_TAB = CO_ALV->GET_COLUMNS( ).
    LO_EVENTS = CO_ALV->GET_EVENT( ).

*   event handler
    SET HANDLER CO_REPORT->ON_LINK_CLICK FOR LO_EVENTS.
    SET HANDLER CO_REPORT->ON_USER_COMMAND FOR LO_EVENTS.
    SET HANDLER CO_REPORT->ON_TOOLBAR FOR ALL INSTANCES ACTIVATION 'X'.
    SET HANDLER CO_REPORT->ON_CHANGE_DATA FOR ALL INSTANCES ACTIVATION 'X'.

  ENDMETHOD.

  METHOD ON_LINK_CLICK.


    READ TABLE IT_SAIDA INTO DATA(LS_SAIDA) INDEX ROW.

    CHECK SY-SUBRC IS INITIAL.

    CASE COLUMN.
      WHEN 'DOC_CONT'.
        CHECK LS_SAIDA-DOC_CONT IS NOT INITIAL.

        SET PARAMETER ID 'BLN' FIELD LS_SAIDA-DOC_CONT.
        SET PARAMETER ID 'BUK' FIELD LS_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD LS_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD SET_LAYOUT.
*
    DATA: LO_LAYOUT  TYPE REF TO CL_SALV_LAYOUT,
          LF_VARIANT TYPE SLIS_VARI,
          LS_KEY     TYPE SALV_S_LAYOUT_KEY.
*   get layout object
    LO_LAYOUT = CO_ALV->GET_LAYOUT( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
    LS_KEY-REPORT = SY-REPID.
    LO_LAYOUT->SET_KEY( LS_KEY ).
*   2. Remove Save layout the restriction.
    LO_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
*   set initial Layout
    LF_VARIANT = 'DEFAULT'.
    LO_LAYOUT->SET_INITIAL_LAYOUT( LF_VARIANT ).
  ENDMETHOD.

ENDCLASS.
