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
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Materiais SAAF NF                                             |*
**/===========================================================================\*

REPORT ZPMR0035.


TYPES BEGIN OF TY_004.
        INCLUDE TYPE ZPMT004.
TYPES MAKTX TYPE CHAR50.
TYPES FLAG TYPE C.
TYPES END OF TY_004.

DATA: IT_004        TYPE TABLE OF TY_004,
      IT_DEL        TYPE TABLE OF TY_004,
      _FCAT         TYPE LVC_T_FCAT,
      _ROWS         TYPE LVC_T_ROW,
      _EXCLUDE      TYPE UI_FUNCTIONS,
      _CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      _GRID         TYPE REF TO CL_GUI_ALV_GRID,
      _LAYOUT       TYPE LVC_S_LAYO,
      _SAVE         TYPE C,
      _STABLE       TYPE LVC_S_STBL VALUE 'XX',
      CONT          TYPE SY-TABIX,
      _VARIANT      TYPE DISVARIANT,
      _MENSAGEM(30),
      _MSG_RET      TYPE TABLE OF ZFIWRS0002.

CLASS ZCL_SAAFNF DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      SELECAO,
      SAVE,
      ADD_ITENS,
      DEL_ITENS,
      SEQ RETURNING VALUE(RETURN) TYPE NUMC10,
      VERIFICA_ERROS IMPORTING INPUT TYPE CHAR1,
      CHECK_MATNR IMPORTING _MATNR        TYPE MATNR
                  RETURNING VALUE(RETURN) TYPE SY-SUBRC,
      GET_DESCMATNR IMPORTING _MATNR        TYPE MATNR
                    RETURNING VALUE(RETURN) TYPE MAKTX,
      Z_DOC_CHECK_NEW IMPORTING INPUT TYPE CHAR1,
      CHECK_DUPLICIDADE IMPORTING _MATNR        TYPE MATNR
                        RETURNING VALUE(RETURN) TYPE SY-SUBRC.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

ENDCLASS.

CLASS ZCL_SAAFNF IMPLEMENTATION.
  METHOD SELECAO.

    FREE IT_004.

    SELECT A~SEQ B~MATNR B~MAKTX
      FROM ZPMT004 AS A
      INNER JOIN MAKT AS B ON A~MATNR EQ B~MATNR
      INTO CORRESPONDING FIELDS OF TABLE IT_004
      WHERE B~SPRAS  EQ SY-LANGU
        AND A~STATUS EQ ABAP_FALSE
      ORDER BY B~MATNR.

  ENDMETHOD.

  METHOD SAVE.

    DELETE IT_004 WHERE FLAG IS INITIAL.
    DELETE IT_004 WHERE MATNR IS INITIAL.

    LOOP AT IT_004 ASSIGNING FIELD-SYMBOL(<WA>).
      <WA>-SEQ = ZCL_SAAFNF=>SEQ( ).
      <WA>-USNAM = SY-UNAME.
      <WA>-DATA_ATUAL = SY-DATUM.
      <WA>-HORA_ATUAL = SY-UZEIT.
      MODIFY ZPMT004 FROM <WA>.
    ENDLOOP.

    IF  IT_DEL IS NOT INITIAL.
      LOOP AT IT_DEL INTO DATA(WA).
        WA-USNAM = SY-UNAME.
        WA-DATA_ATUAL = SY-DATUM.
        WA-HORA_ATUAL = SY-UZEIT.
        MODIFY ZPMT004 FROM WA.
      ENDLOOP.

    ENDIF.

    CLEAR: CONT.
    FREE: IT_DEL, IT_004.

  ENDMETHOD.

  METHOD ADD_ITENS.
    FREE IT_004.
    DO.
      IF CONT EQ 20.
        EXIT.
      ENDIF.
      APPEND VALUE #( FLAG = ABAP_TRUE ) TO IT_004.
      ADD 1 TO CONT.
    ENDDO.
  ENDMETHOD.

  METHOD DEL_ITENS.

    CALL METHOD _GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = _ROWS.

    LOOP AT _ROWS INTO DATA(WA).
      LOOP AT IT_004 ASSIGNING FIELD-SYMBOL(<WA>).
        CHECK WA-INDEX EQ SY-TABIX.
        <WA>-STATUS = 'I'.
        APPEND <WA> TO IT_DEL.
      ENDLOOP.
    ENDLOOP.

    DELETE IT_004 WHERE STATUS EQ 'I'.

    _GRID->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).

  ENDMETHOD.

  METHOD SEQ.
    SELECT COUNT(*)
      INTO RETURN
      FROM ZPMT004.

    ADD 1 TO RETURN.

  ENDMETHOD.

  METHOD ON_DATA_CHANGED.
  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED.
    ZCL_SAAFNF=>VERIFICA_ERROS( ABAP_FALSE ).

    LOOP AT ET_GOOD_CELLS INTO DATA(WA).
      LOOP AT IT_004 ASSIGNING FIELD-SYMBOL(<F004>).
        CHECK WA-ROW_ID EQ SY-TABIX.
        <F004>-MAKTX = GET_DESCMATNR( CONV #( <F004>-MATNR ) ).
      ENDLOOP.
    ENDLOOP.

    _GRID->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = _STABLE ).
  ENDMETHOD.

  METHOD VERIFICA_ERROS.
    FREE: _MSG_RET.

    LOOP AT IT_004 INTO DATA(WA) WHERE MATNR IS NOT INITIAL.
      IF ZCL_SAAFNF=>CHECK_MATNR( WA-MATNR ) IS NOT INITIAL.
        APPEND VALUE #( MSG = |Linha { SY-TABIX } está com material { WA-MATNR ALPHA = OUT } invalido!|
                        OBJ = '_GRID'
                        FIELD = 'MATNR'
                        TABIX = SY-TABIX ) TO _MSG_RET.
      ELSE.
        IF ZCL_SAAFNF=>CHECK_DUPLICIDADE( WA-MATNR ) IS INITIAL.
          APPEND VALUE #( MSG = |Linha { SY-TABIX } está com material { WA-MATNR ALPHA = OUT } já cadastrado!|
                          OBJ = '_GRID'
                          FIELD = 'MATNR'
                          TABIX = SY-TABIX ) TO _MSG_RET.
        ENDIF.
      ENDIF.

    ENDLOOP.

    ZCL_SAAFNF=>Z_DOC_CHECK_NEW( INPUT ).

  ENDMETHOD.

  METHOD CHECK_MATNR.
    SELECT COUNT(*) FROM MARA WHERE MATNR EQ _MATNR. RETURN = SY-SUBRC.
  ENDMETHOD.

  METHOD GET_DESCMATNR.
    SELECT SINGLE MAKTX FROM MAKT INTO RETURN WHERE MATNR EQ _MATNR.
  ENDMETHOD.

  METHOD Z_DOC_CHECK_NEW.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN    = '100'
        I_SHOW      = INPUT
        I_REPID     = SY-REPID
        I_SET_FIELD = 'X_FIELD'
        I_SET_CELL  = 'WG_CELL'
        I_SET_OBJ   = 'WG_OBJ'
      IMPORTING
        E_MESSAGEM  = _MENSAGEM
      TABLES
        IT_MSGS     = _MSG_RET.
  ENDMETHOD.

  METHOD CHECK_DUPLICIDADE.
    SELECT COUNT(*) FROM ZPMT004 WHERE MATNR EQ _MATNR AND STATUS EQ ABAP_FALSE. RETURN = SY-SUBRC.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  ZCL_SAAFNF=>SELECAO( ).
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  ZCL_SAAFNF=>Z_DOC_CHECK_NEW( ABAP_FALSE ).
  FREE _FCAT.

  _FCAT = VALUE #(
                    ( COL_POS = 1 FIELDNAME = 'MATNR' SCRTEXT_L = 'Cód. Material' NO_ZERO = ABAP_TRUE EDIT = COND #( WHEN CONT IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE ) )
                    ( COL_POS = 2 FIELDNAME = 'MAKTX' SCRTEXT_L = 'Desc. Material' OUTPUTLEN = 30 )
                 ).

  _EXCLUDE = VALUE #( ( CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL ) ).

  _LAYOUT = VALUE #(
                    ZEBRA      = ABAP_TRUE
                    NO_ROWINS  = ABAP_TRUE
                    SEL_MODE   = 'C'
   ).

  IF _CONTAINER IS INITIAL.

    CREATE OBJECT _CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CC_'.

    CREATE OBJECT _GRID
      EXPORTING
        I_PARENT = _CONTAINER.

    CALL METHOD _GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = _VARIANT
        IS_LAYOUT            = _LAYOUT
        I_SAVE               = _SAVE
        I_DEFAULT            = ABAP_TRUE
        IT_TOOLBAR_EXCLUDING = _EXCLUDE
      CHANGING
        IT_FIELDCATALOG      = _FCAT
        IT_OUTTAB            = IT_004.

    CALL METHOD _GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD _GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: ZCL_SAAFNF=>ON_DATA_CHANGED_FINISHED FOR _GRID,
                 ZCL_SAAFNF=>ON_DATA_CHANGED FOR _GRID.

  ELSE.

    CALL METHOD _GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = _FCAT.

    CALL METHOD _GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      ZCL_SAAFNF=>SAVE( ).
      ZCL_SAAFNF=>SELECAO( ).
    WHEN 'ADD'.
      ZCL_SAAFNF=>ADD_ITENS( ).
    WHEN 'DEL'.
      ZCL_SAAFNF=>DEL_ITENS( ).
    WHEN '_MENSAGEM'.
      ZCL_SAAFNF=>VERIFICA_ERROS( ABAP_TRUE ).
  ENDCASE.

ENDMODULE.
