**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatoio de Acompanhamento de Contratos - Algodão                         |*
**/===========================================================================\*

REPORT ZSDR0111.

TABLES: ZSDT0143.

TYPES: BEGIN OF TY_SAIDA.
         INCLUDE  TYPE ZSDT0143.
       TYPES:
                S_SOL(20)           TYPE C,
                S_CON(20)           TYPE C,
                NRO_SOL_OV(50)      TYPE C,
                DESC_EMPRESA(50)    TYPE C,
                DESC_CLIENTE(50)    TYPE C,
                DESC_VENDEDOR(50)   TYPE C,
                DESC_CORRETOR(50)   TYPE C,
                DESC_TIPO_PADRAO(5) TYPE C,
                DESC_TP_VENDA(50)   TYPE C,
                LINE_COLOR(4)       TYPE C,
              END OF TY_SAIDA,

              TY_T_SAIDA TYPE TABLE OF TY_SAIDA WITH DEFAULT KEY.

DATA GT_OUTTAB TYPE TABLE OF TY_SAIDA.
DATA _OUTTAB TYPE TY_SAIDA.
DATA DOCUMENT TYPE REF TO CL_DD_DOCUMENT.

DATA _VARIANT    TYPE DISVARIANT.
DATA VARIANTE TYPE DISVARIANT.

FIELD-SYMBOLS: <FS_DATA>  TYPE ANY TABLE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
S_EMPR FOR ZSDT0143-EMPRESA NO-EXTENSION NO INTERVALS OBLIGATORY,
S_SAFR FOR ZSDT0143-SAFRA NO-EXTENSION NO INTERVALS,
S_TP_V FOR ZSDT0143-TP_VENDA NO-EXTENSION NO INTERVALS,
S_VEND FOR ZSDT0143-VENDEDOR NO-EXTENSION NO INTERVALS,
S_CONT FOR ZSDT0143-CONTRATO NO-EXTENSION NO INTERVALS,
S_DATA FOR ZSDT0143-DATA_ATUAL,
S_STAT FOR ZSDT0143-STS_CON NO-EXTENSION NO INTERVALS DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-000.
PARAMETER: P_VARIA TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B5.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.
  VARIANTE-REPORT = SY-REPID.
  _VARIANT-REPORT = SY-REPID.

  IF ( NOT P_VARIA IS INITIAL ).
    _VARIANT-VARIANT = P_VARIA.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = VARIANTE
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE VARIANTE-VARIANT TO P_VARIA.
    MOVE VARIANTE-VARIANT TO _VARIANT-VARIANT.
  ENDIF.

CLASS CL_MAIN DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS RUN.

    METHODS SET_TITLE_AND_STATUS.

    METHODS SELECT_DATA
      EXCEPTIONS
        DATA_NOT_FOUND.

    METHODS SET_HEADER.
    METHODS CREATE_DOCKING.

    METHODS GET_FIELDCATALOG
      RETURNING VALUE(FCAT) TYPE LVC_T_FCAT.

    METHODS PROCESS_BEFORE_OUTPUT.
    METHODS SET_OUTTAB_DATA.

    METHODS HANDLE_SET_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT.

    METHODS HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.

    METHODS DISPLAY.

  PRIVATE SECTION.

*    "//tables
    DATA IT_CONTRATOS TYPE TABLE OF ZSDT0143.
    DATA IT_0227      TYPE TABLE OF ZSDT0227.
    DATA IT_0056      TYPE TABLE OF ZSDT0056.
    DATA IT_0051      TYPE TABLE OF ZSDT0051.

*    "//Objects
    DATA DOCKING        TYPE REF TO CL_GUI_DOCKING_CONTAINER.
    DATA SPLITTER       TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
    DATA CUSTOM_HEADER  TYPE REF TO CL_GUI_CONTAINER.
    DATA CUSTOM_GRID    TYPE REF TO CL_GUI_CONTAINER.
    DATA GRID           TYPE REF TO CL_GUI_ALV_GRID.
    DATA ALV_TREE       TYPE REF TO CL_GUI_ALV_TREE.
*
ENDCLASS.

DATA R_MAIN TYPE REF TO CL_MAIN.

CLASS CL_MAIN IMPLEMENTATION.

  METHOD RUN.

    CREATE OBJECT R_MAIN.

    R_MAIN->SELECT_DATA( EXCEPTIONS DATA_NOT_FOUND = 4 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    ELSE.
      CALL SCREEN 0001.
    ENDIF.
  ENDMETHOD.

  METHOD PROCESS_BEFORE_OUTPUT.
    "//set title
    ME->SET_TITLE_AND_STATUS( ).

    "//screen components
    ME->CREATE_DOCKING( ).

    "//set data
    ME->SET_HEADER( ).
    ME->SET_OUTTAB_DATA( ).

    "//display data
    ME->DISPLAY( ).
  ENDMETHOD.

  METHOD SET_TITLE_AND_STATUS.
    SET TITLEBAR 'MAIN_TITLE'.
    SET PF-STATUS 'MAIN_STATUS'.
  ENDMETHOD.

  METHOD SELECT_DATA.

    IF S_STAT-LOW EQ '*'.
      FREE S_STAT.
    ENDIF.

    SELECT *
      FROM ZSDT0143
        INTO TABLE IT_CONTRATOS
        WHERE SAFRA    IN S_SAFR
        AND EMPRESA    IN S_EMPR
        AND TP_VENDA   IN S_TP_V
        AND VENDEDOR   IN S_VEND
        AND CONTRATO   IN S_CONT
        AND DATA_ATUAL IN S_DATA
        AND STS_CON    IN S_STAT.

    IF SY-SUBRC IS INITIAL.

      IF S_STAT-LOW IS INITIAL.
        DELETE IT_CONTRATOS WHERE STS_CON NE ' '.
        DELETE IT_CONTRATOS WHERE CANCELADO EQ ABAP_TRUE.
      ENDIF.

      CHECK IT_CONTRATOS IS NOT INITIAL.

      SELECT *
        FROM ZSDT0051
          INTO TABLE IT_0051
          FOR ALL ENTRIES IN IT_CONTRATOS
          WHERE BSTKD EQ IT_CONTRATOS-CONTRATO
            AND STATUS EQ 'L'.

      SELECT *
        FROM ZSDT0227
          INTO TABLE IT_0227
          FOR ALL ENTRIES IN IT_CONTRATOS
          WHERE ID_CONTRATO EQ IT_CONTRATOS-ID_CONTRATO.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM ZSDT0056
            INTO TABLE IT_0056
            FOR ALL ENTRIES IN IT_0227
            WHERE COD_FP EQ IT_0227-COD_FP.
      ENDIF.

    ELSE.
      MESSAGE TEXT-E01 TYPE 'S' RAISING DATA_NOT_FOUND.
    ENDIF.

  ENDMETHOD.

  METHOD SET_HEADER.

    DATA TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT.
    DATA TABLE_TEXTS     TYPE SDYDO_TEXT_TABLE.
    DATA TABLE_TEXT     LIKE LINE OF TABLE_TEXTS.
    DATA COLUMN         TYPE REF TO CL_DD_AREA.

    IF ( DOCUMENT IS NOT BOUND ).
      CREATE OBJECT DOCUMENT.
    ELSE.
      DOCUMENT->INITIALIZE_DOCUMENT( ).
    ENDIF.

    "//Build title text
    DOCUMENT->ADD_TEXT( TEXT = 'Parâmetros de seleção:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    DOCUMENT->NEW_LINE(  ). DOCUMENT->UNDERLINE( ).

    DOCUMENT->ADD_TABLE(
      EXPORTING
        NO_OF_COLUMNS               = 2     " Number of Table Columns
        BORDER                      = '0'     " Width of Table Frame; '0' = No Frame
      IMPORTING
        TABLE                       = DATA(_DOCTABLE1)

    ).

    IF SY-SUBRC <> 0.

    ENDIF.

    _DOCTABLE1->ADD_COLUMN( EXPORTING WIDTH = '30%' IMPORTING COLUMN = DATA(_COLUMN_KEY) ).
    _DOCTABLE1->ADD_COLUMN( IMPORTING COLUMN = DATA(_COLUMN_VALUE) ).


    _COLUMN_KEY->ADD_TEXT( TEXT = 'Empresa:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_EMPR-LOW IS NOT INITIAL
                                       THEN CONV I( S_EMPR-LOW )"- { ME->GET_MATERIAL_DESCRIPTION( S_MATNR-LOW ) }|
                                       ELSE '*' ) }| ).

    _DOCTABLE1->NEW_ROW( ).

    _COLUMN_KEY->ADD_TEXT( TEXT = 'Safra:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_SAFR-LOW IS NOT INITIAL
                                       THEN S_SAFR-LOW" - { ME->GET_WERKS_DESCRIPTION( S_WERKS-LOW ) }|
                                       ELSE '*' ) }| ).

    _DOCTABLE1->NEW_ROW( ).

    _COLUMN_KEY->ADD_TEXT( TEXT = 'Tipo de Venda:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_TP_V-LOW IS NOT INITIAL
                                       THEN S_TP_V-LOW
                                       ELSE '*' ) }| ).

    _DOCTABLE1->NEW_ROW( ).

    _COLUMN_KEY->ADD_TEXT( TEXT = 'Vendedor:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_VEND-LOW IS NOT INITIAL
                                       THEN S_VEND-LOW
                                       ELSE '*' ) }| ).
    _DOCTABLE1->NEW_ROW( ).

    _COLUMN_KEY->ADD_TEXT( TEXT = 'Contrato:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_CONT-LOW IS NOT INITIAL
                                       THEN S_CONT-LOW
                                       ELSE '*' ) }| ).

    _DOCTABLE1->NEW_ROW( ).

    _COLUMN_KEY->ADD_TEXT( TEXT = 'Data de Criação:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _COLUMN_VALUE->ADD_TEXT( TEXT = | { COND #( WHEN S_DATA-LOW IS NOT INITIAL AND S_DATA-HIGH IS NOT INITIAL THEN |{ S_DATA-LOW } até { S_DATA-HIGH }|
                                                WHEN S_DATA-LOW IS NOT INITIAL AND S_DATA-HIGH IS INITIAL THEN |{ S_DATA-LOW }|
                                       ELSE '*' ) }| ).

    _DOCTABLE1->NEW_ROW( ).

    DOCUMENT->MERGE_DOCUMENT( ).
    DOCUMENT->DISPLAY_DOCUMENT( PARENT = CUSTOM_HEADER ).

  ENDMETHOD.
*
  METHOD CREATE_DOCKING.

    CREATE OBJECT DOCKING
      EXPORTING
        REPID     = SY-REPID
        DYNNR     = SY-DYNNR
        SIDE      = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_TOP
        EXTENSION = 5000.

    CREATE OBJECT SPLITTER
      EXPORTING
        LINK_DYNNR = SY-DYNNR
        LINK_REPID = SY-REPID
        TOP        = 50
        PARENT     = DOCKING
        ROWS       = 2
        COLUMNS    = 1.

    ME->SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 0 ).

    CUSTOM_HEADER = ME->SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
    CUSTOM_GRID   = ME->SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

  ENDMETHOD.
*
  METHOD SET_OUTTAB_DATA.
    DATA: NRO_SOL_OV TYPE STRING.
    DATA: N_SOL TYPE STRING.

    LOOP AT IT_CONTRATOS INTO DATA(_CONT).

      CLEAR NRO_SOL_OV.
      LOOP AT IT_0051 INTO DATA(WA_0051) WHERE BSTKD EQ _CONT-CONTRATO.

        N_SOL = |{ WA_0051-NRO_SOL_OV ALPHA = OUT }|.
        CONDENSE N_SOL NO-GAPS.

        IF NRO_SOL_OV IS INITIAL.
          NRO_SOL_OV = N_SOL.
        ELSE.
          NRO_SOL_OV = |{ NRO_SOL_OV }, { N_SOL }|.
        ENDIF.

      ENDLOOP.

      MOVE-CORRESPONDING _CONT TO _OUTTAB.
      _OUTTAB-NRO_SOL_OV = NRO_SOL_OV.

      SELECT SINGLE NORMT
        FROM MARA
          INTO _OUTTAB-DESC_TIPO_PADRAO
        WHERE MATNR EQ _CONT-TIPO_PADRAO.

      SELECT SINGLE NAME1
        FROM LFA1
          INTO _OUTTAB-DESC_CORRETOR
        WHERE LIFNR EQ _CONT-CORRETOR.

      SELECT SINGLE NAME1 FROM KNA1
        INTO _OUTTAB-DESC_CLIENTE
        WHERE KUNNR EQ _CONT-CLIENTE.

      SELECT SINGLE BEZEI
        FROM TVGRT
        INTO _OUTTAB-DESC_VENDEDOR
        WHERE VKGRP EQ _CONT-VENDEDOR
          AND SPRAS EQ SY-LANGU.

      SELECT SINGLE BUTXT
        FROM T001
        INTO _OUTTAB-DESC_EMPRESA
        WHERE BUKRS EQ _CONT-EMPRESA.

      CASE _CONT-STS_SOL.
        WHEN 'A'. _OUTTAB-S_SOL = 'Aguardando Sol. de Aprovação'.
        WHEN 'L'.	_OUTTAB-S_SOL = 'Liberado'.
        WHEN 'D'.	_OUTTAB-S_SOL = 'Deletado'.
        WHEN 'R'.	_OUTTAB-S_SOL = 'Reprovado'.
        WHEN 'P'.	_OUTTAB-S_SOL = 'Aguardando Aprovação'.
      ENDCASE.

*      CASE _CONT-STS_CON.
*        WHEN 'E'. _OUTTAB-S_CON = 'Encerrado'.
*        WHEN 'C'. _OUTTAB-S_CON = 'Cancelado'.
*        WHEN OTHERS. _OUTTAB-S_CON = 'Aberto'.
*      ENDCASE.

      CASE _CONT-STS_CON.
        WHEN 'E'. _OUTTAB-S_CON = 'Encerrado'.
        WHEN 'C'. _OUTTAB-S_CON = 'Cancelado'.
        WHEN OTHERS. _OUTTAB-S_CON = 'Aberto'.
      ENDCASE.

      CASE _CONT-CANCELADO.
        WHEN 'X'. _OUTTAB-S_CON = 'Cancelado'.
      ENDCASE.


      APPEND _OUTTAB TO GT_OUTTAB.
      CLEAR _OUTTAB.

    ENDLOOP.

  ENDMETHOD.

  METHOD HANDLE_SET_TOOLBAR.

*    DATA(_STANDARD_TOOLBARS) = E_OBJECT->MT_TOOLBAR.
*    CLEAR E_OBJECT->MT_TOOLBAR.

*    APPEND VALUE #( BUTN_TYPE = CNTB_BTYPE_BUTTON
*                    FUNCTION  = 'VLDAT'
*                    ICON      = ICON_PLANNING_OUT
*                    TEXT      = 'Alterar Vencimento'
*                  ) TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.

  METHOD HANDLE_USER_COMMAND.
    CALL METHOD ME->GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = DATA(SELECTED_ROW).

    CASE E_UCOMM.
      WHEN 'VLDAT'.
*        ME->VFDAT( SELECTED_ROW ).
    ENDCASE.
  ENDMETHOD.

  METHOD DISPLAY.
    DATA(_FIELDCATALOG) = ME->GET_FIELDCATALOG( ).

    DATA(_LAYOUT) = VALUE LVC_S_LAYO( CTAB_FNAME = 'COLOR' SEL_MODE   = 'A' INFO_FNAME = 'LINE_COLOR' ).

    CREATE OBJECT ME->GRID
      EXPORTING
        I_PARENT = ME->CUSTOM_GRID.

    SET HANDLER: ME->HANDLE_SET_TOOLBAR  FOR ME->GRID,
                 ME->HANDLE_USER_COMMAND FOR ME->GRID.
*    BREAK-POINT.
    CALL METHOD ME->GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = _VARIANT
        IS_LAYOUT       = _LAYOUT
        I_SAVE          = ABAP_TRUE
      CHANGING
        IT_OUTTAB       = <FS_DATA>
        IT_FIELDCATALOG = _FIELDCATALOG.

    CALL METHOD ME->GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  ENDMETHOD.

  METHOD GET_FIELDCATALOG.
*    BREAK-POINT .
    DATA: V_CAMP(7),
          V_TEXT(100),
          TABIX TYPE SY-TABIX.

    DATA: T_ALVDATA TYPE REF TO DATA.
    DATA: T_DATA TYPE REF TO DATA.
    DATA STR TYPE REF TO DATA.

    DATA T_NEW_TABLE    TYPE REF TO DATA.

    DATA: FC_227 TYPE LVC_T_FCAT.

    FIELD-SYMBOLS: <WA_DATA>  TYPE ANY,
                   <F_CAMPO>  TYPE ANY,
                   <FS_CAMPO> TYPE ANY.

    FCAT =
        VALUE #(
        ( REF_TABLE = 'TVGRT'    REF_FIELD = 'BEZEI'            FIELDNAME = 'S_SOL'            COLTEXT = 'Status da Solicitação' OUTPUTLEN = 10 )
        ( REF_TABLE = 'TVGRT'    REF_FIELD = 'BEZEI'            FIELDNAME = 'S_CON'            COLTEXT = 'Status do Contrato'    OUTPUTLEN = 10 )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'ID_CONTRATO'      FIELDNAME = 'ID_CONTRATO'      COLTEXT = 'Id Contrato'           )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'EMPRESA'          FIELDNAME = 'EMPRESA'          COLTEXT = 'Empresa'               )
        ( REF_TABLE = 'T001'     REF_FIELD = 'BUTXT'            FIELDNAME = 'DESC_EMPRESA'     COLTEXT = 'Desc Empresa'          )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'SAFRA'            FIELDNAME = 'SAFRA'            COLTEXT = 'Safra'                 )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'CONTRATO'         FIELDNAME = 'CONTRATO'         COLTEXT = 'Contrato'              )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'CONTRATO_CLIENTE' FIELDNAME = 'CONTRATO_CLIENTE' COLTEXT = 'Contrato Cliente'      )
        ( REF_TABLE = ''         REF_FIELD = ''                 FIELDNAME = 'NRO_SOL_OV'       COLTEXT = 'Solicitações'          )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'DT_VENDA'         FIELDNAME = 'DT_VENDA'         COLTEXT = 'Data Venda'            )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'CLIENTE'          FIELDNAME = 'CLIENTE'          COLTEXT = 'Cliente'               )
        ( REF_TABLE = 'KNA1'     REF_FIELD = 'NAME1'            FIELDNAME = 'DESC_CLIENTE'     COLTEXT = 'Desc Cliente'          )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'VENDEDOR'         FIELDNAME = 'VENDEDOR'         COLTEXT = 'Equipe de Vendas'      )
        ( REF_TABLE = 'TVGRT'    REF_FIELD = 'BEZEI'            FIELDNAME = 'DESC_VENDEDOR'    COLTEXT = 'Desc Vendedor'         )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'CORRETOR'         FIELDNAME = 'CORRETOR'         COLTEXT = 'Corretor'              )
        ( REF_TABLE = 'LFA1'     REF_FIELD = 'NAME1'            FIELDNAME = 'DESC_CORRETOR'    COLTEXT = 'Desc Corretor'         )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'TIPO_PADRAO'      FIELDNAME = 'TIPO_PADRAO'      COLTEXT = 'Tipo Padão'            )
        ( REF_TABLE = 'MARA'     REF_FIELD = 'NORMT'            FIELDNAME = 'DESC_TIPO_PADRAO' COLTEXT = 'Desc Tipo'             )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'PORTO'            FIELDNAME = 'PORTO'            COLTEXT = 'Porto'                 )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'DE_EMBARQUE'      FIELDNAME = 'DE_EMBARQUE'      COLTEXT = 'De'                    )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'ATE_EMBARQUE'     FIELDNAME = 'ATE_EMBARQUE'     COLTEXT = 'Até'                   )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'QUATIDADE'        FIELDNAME = 'QUATIDADE'        COLTEXT = 'Quantidade/KG'         DO_SUM = ABAP_TRUE )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'QDT_EMBARCADA'    FIELDNAME = 'QDT_EMBARCADA'    COLTEXT = 'Qtd Embarcada'         DO_SUM = ABAP_TRUE )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'COMISSAO'         FIELDNAME = 'COMISSAO'         COLTEXT = 'Comissão %'            )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'TOLERANCIA'       FIELDNAME = 'TOLERANCIA'       COLTEXT = 'Tolerância %'          )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'PRECO'            FIELDNAME = 'PRECO'            COLTEXT = 'Preço LB/USD'          )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'PRECO_TONS'       FIELDNAME = 'PRECO_TONS'       COLTEXT = 'Preço Tonelada'        )
        ( REF_TABLE = 'ZSDT0143' REF_FIELD = 'PCTGEM_ANT'       FIELDNAME = 'PCTGEM_ANT'       COLTEXT = '% Antecipação'         ) ).

    LOOP AT FCAT ASSIGNING FIELD-SYMBOL(<F_FCAT>).
      TABIX = SY-TABIX.
      <F_FCAT>-COL_POS = TABIX.
      <F_FCAT>-TABNAME = '<FS_DATA>'.
    ENDLOOP.
*    BREAK-POINT.
    LOOP AT IT_0227 INTO DATA(W_0227).

      V_CAMP = |{ W_0227-FIELD+0(3) }{ W_0227-COD_FP }|.

      TRY .
          DATA(W_0056) = IT_0056[ COD_FP = W_0227-COD_FP ].
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          CLEAR W_0056.
      ENDTRY.

      IF W_0056-BEZEI IS NOT INITIAL.
        CONCATENATE W_0056-BEZEI '-' W_0227-FIELD INTO V_TEXT.
      ELSE.
        V_TEXT = W_0056-BEZEI.
      ENDIF.

      ADD 1 TO TABIX.


      IF W_0227-C_DECIMAIS = '4'.
        APPEND VALUE #(
                REF_TABLE = 'ZFIT0036' REF_FIELD = 'TX_CAMBIO'
                COL_POS = TABIX FIELDNAME = V_CAMP COLTEXT = V_TEXT OUTPUTLEN = 10 DO_SUM = ABAP_TRUE EMPHASIZE = 'C500'
              ) TO FCAT.
      ELSE.
        APPEND VALUE #(
              REF_TABLE = 'ZSDT0053' REF_FIELD = 'VLRTOT'
              COL_POS = TABIX FIELDNAME = V_CAMP COLTEXT = V_TEXT OUTPUTLEN = 10 DO_SUM = ABAP_TRUE EMPHASIZE = 'C500'
      ) TO FCAT.
      ENDIF.

      IF W_0056-OCBOT IS NOT INITIAL.
        ADD 1 TO TABIX.
        APPEND VALUE #(
*                REF_TABLE = 'ZSDT0227' REF_FIELD = W_0227-BEZEI
                COL_POS = TABIX FIELDNAME = |CBO{ W_0227-COD_FP }| COLTEXT = 'CBOT' OUTPUTLEN = 5 EMPHASIZE = 'C500'
              ) TO FCAT.
      ENDIF.

      CASE W_0227-BEZEI.
        WHEN 'PREMIO' OR 'SPREAD'.
          ADD 1 TO TABIX.
          APPEND VALUE #(
*                          REF_TABLE = 'ZSDT0227' REF_FIELD = W_0227-BEZEI
                          COL_POS = TABIX FIELDNAME = |MON{ W_0227-COD_FP }| COLTEXT = |Mês-{ W_0227-BEZEI }| OUTPUTLEN = 5 EMPHASIZE = 'C500'
                ) TO FCAT.
      ENDCASE.

    ENDLOOP.

    SORT FCAT BY FIELDNAME.
    DELETE ADJACENT DUPLICATES FROM FCAT COMPARING FIELDNAME.

    SORT FCAT BY COL_POS.
*    BREAK-POINT.
    CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
      EXPORTING
        I_STYLE_TABLE   = ' '
        IT_FIELDCATALOG = FCAT
      IMPORTING
        EP_TABLE        = T_DATA.

    IF <FS_DATA> IS ASSIGNED.
      UNASSIGN <FS_DATA>.
      UNASSIGN <WA_DATA>.
      UNASSIGN <FS_CAMPO>.
      UNASSIGN <F_CAMPO>.
    ENDIF.

    ASSIGN T_DATA->* TO <FS_DATA>.
    CREATE DATA T_ALVDATA LIKE LINE OF <FS_DATA>.
    ASSIGN T_ALVDATA->* TO <WA_DATA>.

    REFRESH <FS_DATA>.
    DATA FIELD_(30).

*    BREAK-POINT.

    LOOP AT GT_OUTTAB ASSIGNING FIELD-SYMBOL(<_OUTTAB>).

      FREE FC_227.
      CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
        EXPORTING
          I_TP_VENDA      = <_OUTTAB>-TP_VENDA
*        IMPORTING
*         E_TABLE         = T_NEW_TABLE
        TABLES
          TE_FIELDCATALOG = FC_227.

      LOOP AT FCAT INTO DATA(_FCAT).
        ASSIGN COMPONENT _FCAT-FIELDNAME  OF STRUCTURE <_OUTTAB> TO <F_CAMPO>.
        IF SY-SUBRC IS INITIAL.
          ASSIGN COMPONENT _FCAT-FIELDNAME OF STRUCTURE <WA_DATA> TO <FS_CAMPO>.
          <FS_CAMPO> = <F_CAMPO>.
        ELSE.
          LOOP AT IT_0227 ASSIGNING FIELD-SYMBOL(<_0227>) WHERE ID_CONTRATO EQ <_OUTTAB>-ID_CONTRATO.

            LOOP AT FC_227 INTO DATA(WF_227)
              WHERE FIELDNAME EQ 'PRECO'
                 OR FIELDNAME EQ 'CBOT'
                 OR FIELDNAME EQ 'MONAT'.

              CHECK _FCAT-FIELDNAME+0(3) EQ WF_227-FIELDNAME+0(3).

              V_CAMP = |{ WF_227-FIELDNAME+0(3) }{ <_0227>-COD_FP }|.
              ASSIGN COMPONENT V_CAMP OF STRUCTURE <WA_DATA> TO <FS_CAMPO>.

              IF SY-SUBRC IS INITIAL.

                CASE WF_227-FIELDNAME.
                  WHEN 'PRECO'.
                    IF <_0227>-FORMULA2 IS NOT INITIAL.
                      <FS_CAMPO> = <_0227>-FORMULA2.
                    ENDIF.
                  WHEN 'CBOT'.
                    IF <_0227>-OCBOT IS NOT INITIAL.
                      <FS_CAMPO> = <_0227>-CBOT.
                    ENDIF.
                  WHEN 'MONAT'.
                    IF <_0227>-MONAT IS NOT INITIAL.
                      <FS_CAMPO> = <_0227>-MONAT.
                    ENDIF.
                ENDCASE.

              ENDIF.

            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      PERFORM F_CARREGA_ALV USING <FS_DATA>
                                  <WA_DATA>.
      CLEAR <WA_DATA>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  _VARIANT-REPORT = SY-REPID.
  _VARIANT-VARIANT = P_VARIA.

  CL_MAIN=>RUN( ).

*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAIN_PBO OUTPUT.
  IF R_MAIN IS INITIAL.
    CREATE OBJECT R_MAIN.
  ENDIF.

  R_MAIN->PROCESS_BEFORE_OUTPUT( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAIN_PAI INPUT.
  IF SY-UCOMM = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0105 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.

      DATA: P_RESP.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = 'Deseja Alterar o Vencimento da Seleção?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DISPLAY_CANCEL_BUTTON = ' '
        IMPORTING
          ANSWER                = P_RESP.

      IF P_RESP EQ 1.
*        IF DATA_VENC IS INITIAL.
*          MESSAGE TEXT-I03 TYPE 'S' DISPLAY LIKE 'E'.
*        ELSE.
*          IF DATA_VENC => SY-DATUM.
*            LEAVE TO SCREEN 0.
*          ELSE.
*            MESSAGE TEXT-I04 TYPE 'S' DISPLAY LIKE 'E'.
*          ENDIF.
*        ENDIF.
      ELSE.
        MESSAGE TEXT-I05 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0105 OUTPUT.
  SET TITLEBAR '0105_TITLE'.
  SET PF-STATUS '0105_STATUS'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>  text
*      -->P_<WA_DATA>  text
*----------------------------------------------------------------------*
FORM F_CARREGA_ALV USING    P_TAB TYPE TABLE
                            P_WA.
  APPEND P_WA TO P_TAB.
ENDFORM.                    " f_carrega_alv
