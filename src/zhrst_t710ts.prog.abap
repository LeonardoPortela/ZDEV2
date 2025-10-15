*&---------------------------------------------------------------------*
*& Report  ZMM0015
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZHRST_T710TS.
*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS.

TYPES: BEGIN OF TYPE_MSN,
         TP_MSN   TYPE BAPI_MTYPE,
         MESSAGEM TYPE BAPI_MSG,
       END   OF TYPE_MSN.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA: MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      T_MSN   TYPE TABLE OF TYPE_MSN.

*       error session opened (' ' or 'X')
DATA:   E_GROUP_OPENED.
*       message texts
TABLES: T100, RMMG1.

DATA: BEGIN OF DYNPFIELDS OCCURS 0. "Hilfsstruktur zum auslesen des akt.
        INCLUDE STRUCTURE DYNPREAD. "Feldwertes vom Dynpro bei >F4<
DATA: END OF   DYNPFIELDS.

TYPES:
  BEGIN OF TY_ARQUIVO,
    MOLGA   TYPE T710-MOLGA,
    SLTYP   TYPE T710-SLTYP,
    SLREG   TYPE T710-SLREG,
    SLGRP   TYPE T710-SLGRP,
    SLLVL   TYPE T710-SLLVL,
    BEGDA   TYPE T710-BEGDA,
    ENDDA   TYPE T710-ENDDA,
    SLMIN   TYPE T710-SLMIN,
    SLMAX   TYPE T710-SLMAX,
    SLREF   TYPE T710-SLREF,
    MESSAGE TYPE BAPI_MSG,
  END OF TY_ARQUIVO,

  BEGIN OF TY_ERRO,
    MSG(100),
  END OF TY_ERRO.



CONSTANTS: C_X TYPE C LENGTH 1 VALUE 'X'.

DATA: BEGIN OF T_06 OCCURS 0.
        INCLUDE STRUCTURE ZMMT0007.
DATA: END OF T_06.


DATA: KZALL            LIKE SY-MARKY,
      GOB_GUI_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
      KZBEW_WERK       LIKE SY-MARKY,
      HLGORT           LIKE MARD-LGORT,
      HWERK            LIKE T001W-WERKS.

*&---------------------------------------------------------------------*
*& Importação do arquivo EXCEL
*&---------------------------------------------------------------------*
DATA: T_EXCEL  LIKE ALSMEX_TABLINE OCCURS 18 WITH HEADER LINE,
      T_EXCEL2 LIKE ALSMEX_TABLINE OCCURS 18 WITH HEADER LINE.


DATA:
  IT_AUX     TYPE TABLE OF TY_ARQUIVO,
  WA_AUX     TYPE TY_ARQUIVO,
  wa_ARQUIVO TYPE T710,
  It_ARQUIVO TYPE TABLE OF T710,
  WA_ERRO    TYPE TY_ERRO.

DATA: EL_HEADDATA             LIKE BAPIMATHEAD,
      EL_CLIENTDATA           LIKE BAPI_MARA,
      EL_CLIENTDATAX          LIKE BAPI_MARAX,
      EL_PLANTDATA            LIKE BAPI_MARC,
      EL_PLANTDATAX           LIKE BAPI_MARCX,
      EL_VALDATA              LIKE BAPI_MBEW,
      EL_VALDATAX             LIKE BAPI_MBEWX,
      EL_SALESDATA            LIKE BAPI_MVKE,
      EL_SALESDATAX           LIKE BAPI_MVKEX,
      EL_STORAGELOCATIONDATA  LIKE BAPI_MARD,
      EL_STORAGELOCATIONDATAX LIKE BAPI_MARDX,
      GS_MAT_LONGTEXT         TYPE BAPI_MLTX,
      GT_RET_MESSAGES         TYPE TABLE OF BAPI_MATRETURN2,
      E_RETURN                LIKE BAPIRET2.
DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.

.
*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

TYPE-POOLS: SLIS,
            KKBLO.

DATA: REPID           LIKE SY-REPID.
DATA: FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
DATA: LAYOUT          TYPE SLIS_LAYOUT_ALV.
DATA: PRINT           TYPE SLIS_PRINT_ALV.
DATA: SORT        TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      EVENTS      TYPE SLIS_T_EVENT,
      XS_EVENTS   TYPE SLIS_ALV_EVENT,
      GT_EXCLUDE  TYPE           SLIS_T_EXTAB,
      GT_EXCLUDE1 TYPE           UI_FUNCTIONS.
DATA: W_TIT(70),
      VMSG(70).

DATA:
  L_SEL_BUTTON                TYPE SMP_DYNTXT,
  "Objetos
  GOB_CUSTOM_CONTAINER        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  GOB_DD_DOCUMENT             TYPE REF TO CL_DD_DOCUMENT,
  GOB_SPLITTER_CONTAINER_MAIN TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  GOB_SPLITTER_CONTAINER_TOPO TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

  GOB_GUI_CONTAINER_TOPO      TYPE REF TO CL_GUI_CONTAINER,
  GOB_GUI_CONTAINER_FILTRO    TYPE REF TO CL_GUI_CONTAINER,
  GOB_GUI_CONTAINER_LOGO      TYPE REF TO CL_GUI_CONTAINER,
  GOB_GUI_CONTAINER_GRID      TYPE REF TO CL_GUI_CONTAINER,
  GOB_GUI_PICTURE             TYPE REF TO CL_GUI_PICTURE,
  GIT_FCAT                    TYPE LVC_T_FCAT,
*      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
  LINES                       TYPE SY-TABIX,
  WA_SELECTED_ROWS            TYPE LVC_S_ROW,
  IT_SELECTED_ROWS            TYPE LVC_T_ROW,
  T_FILE                      TYPE TABLE OF TY_ARQUIVO. "zpp_modelo_plan.





SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-004.


  PARAMETER: P_FILE   TYPE RLGRAP-FILENAME DEFAULT ''.


SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_FILENAME     = ' '
      DEF_PATH         = 'C:\'
      MASK             = ',*.xls*,'
      MODE             = 'O'
      TITLE            = 'Busca de Arquivo'
    IMPORTING
      FILENAME         = P_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.


START-OF-SELECTION.
  PERFORM PROCESSA_ARQUIVO.
  PERFORM ALV_ERRO.



*----------------------------------------------------------------------*
FORM MONTA_FIELDCAT USING
               X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_QFIELD
               X_HOTSPOT X_KEY X_ZERO.
*----------------------------------------------------------------------*
*

  FIELDCAT-FIELDNAME     = X_FIELD.
  FIELDCAT-TABNAME       = X_TAB.
  FIELDCAT-REF_TABNAME   = X_REF.
  FIELDCAT-DO_SUM        = X_SUM.
  FIELDCAT-JUST          = X_JUST.
  FIELDCAT-QFIELDNAME    = X_QFIELD.
  FIELDCAT-HOTSPOT       = X_HOTSPOT.
  FIELDCAT-KEY           = X_KEY.
  FIELDCAT-NO_ZERO       = X_ZERO.

  APPEND FIELDCAT.
  CLEAR FIELDCAT.
*
ENDFORM.                               " MONTA_FIELDCAT

*----------------------------------------------------------------------*
FORM MONTA_FIELDCAT_2 USING
               X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_QFIELD
               X_HOTSPOT X_KEY X_ZERO X_SELTEXT_L X_OUTPUTLEN.
*----------------------------------------------------------------------*
*

  FIELDCAT-FIELDNAME     = X_FIELD.
  FIELDCAT-TABNAME       = X_TAB.
  FIELDCAT-REF_TABNAME   = X_REF.
  FIELDCAT-DO_SUM        = X_SUM.
  FIELDCAT-JUST          = X_JUST.
  FIELDCAT-QFIELDNAME    = X_QFIELD.
  FIELDCAT-HOTSPOT       = X_HOTSPOT.
  FIELDCAT-KEY           = X_KEY.
  FIELDCAT-NO_ZERO       = X_ZERO.
  FIELDCAT-SELTEXT_L     = X_SELTEXT_L.
  FIELDCAT-OUTPUTLEN     = X_OUTPUTLEN.

  APPEND FIELDCAT.
  CLEAR FIELDCAT.
*
ENDFORM.                               " MONTA_FIELDCAT



*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
FORM SET_STATUS USING PF_TAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ALV'.

ENDFORM.                    "SET_STATUS

*

**&---------------------------------------------------------------------*
*&      Form  PROCESSA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA_ARQUIVO .
  CLEAR T_EXCEL.
  DATA: V_ENDDA     TYPE T710-ENDDA,
        V_ENDDA_TES TYPE T710-ENDDA,
        WA_T710     TYPE  T710,
        WA_T710A    TYPE  t710A.
  REFRESH: T_EXCEL, T_EXCEL2.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 15
      I_END_ROW               = 10000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  T_EXCEL2[] = T_EXCEL[].
  SORT T_EXCEL2 BY ROW COL.
  CLEAR: T_EXCEL2, IT_ARQUIVO.
  V_ENDDA = '99991231'.
  LOOP AT T_EXCEL.
    IF T_EXCEL-ROW = T_EXCEL2-ROW.
      CONTINUE.
    ENDIF.
    LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL2-COL.
        WHEN 1.
          WA_ARQUIVO-MOLGA   = T_EXCEL2-VALUE.
        WHEN 2.
          WA_ARQUIVO-SLTYP = T_EXCEL2-VALUE.
        WHEN 3.
          WA_ARQUIVO-SLREG    = T_EXCEL2-VALUE.
        WHEN 4.
          WA_ARQUIVO-SLGRP    = T_EXCEL2-VALUE.
        WHEN 5.
          WA_ARQUIVO-SLLVL    = T_EXCEL2-VALUE.
        WHEN 6.
          CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE(2)  INTO WA_ARQUIVO-BEGDA.
        WHEN 7.
          CONCATENATE T_EXCEL2-VALUE+6(4) T_EXCEL2-VALUE+3(2) T_EXCEL2-VALUE(2) INTO  WA_ARQUIVO-ENDDA.
        WHEN 8.
          PERFORM F_CHANGE_TEXT CHANGING T_EXCEL2-VALUE.
          WA_ARQUIVO-SLMIN    = T_EXCEL2-VALUE.
        WHEN 9.
          PERFORM F_CHANGE_TEXT CHANGING T_EXCEL2-VALUE.
          WA_ARQUIVO-SLMAX    = T_EXCEL2-VALUE.
        WHEN 10.
          PERFORM F_CHANGE_TEXT CHANGING T_EXCEL2-VALUE.
          WA_ARQUIVO-SLREF    = T_EXCEL2-VALUE.
      ENDCASE.

    ENDLOOP.
    APPEND WA_ARQUIVO TO IT_ARQUIVO.
    CONCATENATE 'Processando Linha ' T_EXCEL-ROW INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    SELECT SINGLE * FROM T710 INTO WA_T710
    WHERE MOLGA EQ WA_ARQUIVO-MOLGA AND
          SLTYP EQ WA_ARQUIVO-SLTYP AND
          SLREG EQ WA_ARQUIVO-SLREG AND
          SLGRP EQ WA_ARQUIVO-SLGRP AND
          SLLVL EQ WA_ARQUIVO-SLLVL AND
          ENDDA EQ V_ENDDA.


    IF WA_T710 IS NOT INITIAL.

      SELECT SINGLE * FROM T710A INTO WA_T710A
      WHERE MOLGA EQ WA_ARQUIVO-MOLGA AND
            SLTYP EQ WA_ARQUIVO-SLTYP AND
            SLREG EQ WA_ARQUIVO-SLREG AND
            SLGRP EQ WA_ARQUIVO-SLGRP AND
            ENDDA EQ V_ENDDA.

      V_ENDDA_TES = WA_ARQUIVO-BEGDA - 1.
      WA_T710-ENDDA = V_ENDDA_TES.
      WA_T710a-ENDDA = V_ENDDA_TES.

      UPDATE T710
      SET BEGDA = WA_ARQUIVO-BEGDA
          SLMIN = WA_ARQUIVO-SLMIN
          SLMAX = WA_ARQUIVO-SLMAX
          SLREF = WA_ARQUIVO-SLREF
      WHERE MOLGA EQ WA_ARQUIVO-MOLGA AND
            SLTYP EQ WA_ARQUIVO-SLTYP AND
            SLREG EQ WA_ARQUIVO-SLREG AND
            SLGRP EQ WA_ARQUIVO-SLGRP AND
            SLLVL EQ WA_ARQUIVO-SLLVL AND
            ENDDA EQ V_ENDDA.
      MODIFY T710 FROM  WA_T710.

      UPDATE T710A
      SET BEGDA = WA_ARQUIVO-BEGDA
          CTAMT = WA_ARQUIVO-SLREF
      WHERE MOLGA EQ WA_ARQUIVO-MOLGA AND
            SLTYP EQ WA_ARQUIVO-SLTYP AND
            SLREG EQ WA_ARQUIVO-SLREG AND
            SLGRP EQ WA_ARQUIVO-SLGRP AND
            ENDDA EQ V_ENDDA.


      MODIFY T710a FROM  WA_T710a.


    ELSE.
      CONCATENATE 'Não foram encontrados valores correspondentes a linha ' T_EXCEL-ROW INTO WA_AUX-MESSAGE SEPARATED BY SPACE .
      WA_AUX-SLTYP = WA_ARQUIVO-SLTYP.
      WA_AUX-SLREG = WA_ARQUIVO-SLREG.
      WA_AUX-SLGRP = WA_ARQUIVO-SLGRP.
      WA_AUX-SLLVL = WA_ARQUIVO-SLLVL.

      APPEND WA_AUX TO IT_AUX.
    ENDIF.
CLEAR: WA_ARQUIVO, WA_T710, WA_AUX.
  ENDLOOP.
ENDFORM.                    " PROCESSA_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  ALV_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_ERRO .

  IF IT_AUX IS NOT INITIAL.
*
    FREE: FIELDCAT.
*
    PERFORM MONTA_FIELDCAT USING:
        'SLTYP'    'IT_AUX'  'T710'          ' ' ' ' ' ' ' ' ' ' ' ' '',
        'SLREG'    'IT_AUX'  'T710'          ' ' ' ' ' ' ' ' ' ' ' ' '',
        'SLGRP'    'IT_AUX'  'T710'          ' ' ' ' ' ' ' ' ' ' ' ' '',
        'SLLVL'    'IT_AUX'  'T710'          ' ' ' ' ' ' ' ' ' ' ' ' '',
        'MESSAGE'  'IT_AUX'  'BSANLY_BCACT_LOG' ' ' ' ' ' ' ' ' ' ' ' ' ''.

    W_TIT ='Erros na alteração Tabela Salarial'.
*
**  LAYOUT-BOX_FIELDNAME     = 'MARK'.
    LAYOUT-BOX_TABNAME       = 'IT_AUX'.
    LAYOUT-ZEBRA = 'X'.
    PRINT-NO_PRINT_LISTINFOS = 'X'.
*
    REPID = SY-REPID.
*
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM       = REPID
        I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
        IT_FIELDCAT              = FIELDCAT[]
        IS_LAYOUT                = LAYOUT
        I_GRID_TITLE             = W_TIT
        IS_PRINT                 = PRINT
      TABLES
        T_OUTTAB                 = IT_AUX
      EXCEPTIONS
        PROGRAM_ERROR            = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    MESSAGE 'Processamento concluido sem erros!' TYPE 'S'.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

*  DATA: _PARAM  TYPE USTYP_T_PARAMETERS,
*        R_PARID TYPE RANGE OF MEMORYID,
*        ZPROC   TYPE CHAR01.
*
*  DATA FCODE TYPE TABLE OF SY-UCOMM.
*
*  FREE: R_PARID, _PARAM.
*
*  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
*    EXPORTING
*      USER_NAME           = SY-UNAME
*    TABLES
*      USER_PARAMETERS     = _PARAM
*    EXCEPTIONS
*      USER_NAME_NOT_EXIST = 1
*      OTHERS              = 2.
*
*  IF _PARAM IS NOT INITIAL.
*    LOOP AT _PARAM ASSIGNING FIELD-SYMBOL(<WS_PARAM_NEP>).
*      IF <WS_PARAM_NEP>-PARID = 'ZPARAM_CREATE_MAT'.
*        ZPROC = ABAP_TRUE.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  IF ZPROC EQ ABAP_FALSE.
*    APPEND 'PROC' TO FCODE.
*  ENDIF.

  SET PF-STATUS 'ALV' .
  SET TITLEBAR 'ALV0100' WITH 'Atualização Tabela Salário'.

 " PERFORM FM_CRIAR_OBJETOS.

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
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_criar_objetos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM FM_CRIAR_OBJETOS .
*
*  DATA: LVA_DATA(22) TYPE C,
*        W_LAYOUT     TYPE LVC_S_LAYO.
*
*  DATA: GS_VARIANT  TYPE DISVARIANT.
*  GS_VARIANT-REPORT      = SY-REPID.
*
*  PERFORM FM_CRIA_FIELDCAT.
*
*
*  CONCATENATE SY-DATUM+6(2) '.'  SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO LVA_DATA.
*
*  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
*    EXPORTING
*       I_TITULO  = 'Cadastro Material Básico'
*       I_FILTROS = VALUE ZIF_SCREEN_LINHA_FILTRO_T( ( PARAMETRO = 'Data Posição' VALOR = LVA_DATA ) )
*     CHANGING
*       ALV = GOB_GUI_ALV_GRID
*     )
*     EQ ABAP_TRUE.
*
*
**    create object event_receiver.
**    set handler event_receiver->hotspot_click  for gob_gui_alv_grid.
**    set handler event_receiver->get_ucomm  for gob_gui_alv_grid.
*
*    W_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
*    W_LAYOUT-ZEBRA      = 'X'.
*    W_LAYOUT-SEL_MODE   = 'A'.
*    W_LAYOUT-COL_OPT    = ABAP_TRUE.
*
*    DATA: WA_FCAT TYPE LVC_S_FCAT.
*
*    LOOP AT FIELDCAT ASSIGNING FIELD-SYMBOL(<FS_FIELD>).
*      WA_FCAT = CORRESPONDING #( <FS_FIELD> ).
*      WA_FCAT-COLTEXT = <FS_FIELD>-SELTEXT_L.
*      APPEND WA_FCAT TO GIT_FCAT.
*      CLEAR WA_FCAT.
*    ENDLOOP.
*
*    CALL METHOD GOB_GUI_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*      EXPORTING
*        IS_LAYOUT                     = W_LAYOUT
*        I_SAVE                        = 'A'
*        IS_VARIANT                    = GS_VARIANT
*      CHANGING
*        IT_OUTTAB                     = IT_ARQUIVO
*        IT_FIELDCATALOG               = GIT_FCAT
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*
*  ENDIF.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_cria_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM FM_CRIA_FIELDCAT .
*
*  REFRESH: FIELDCAT.
*
*  PERFORM MONTA_FIELDCAT_2 USING:
*
*'STATUS'   'T_MRP2'   ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Status' '10',
*'MATERIAL'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Material' '20',
*'MBRSH'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Setor industrial' '20',
*'MTART'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Tipo de material' '20',
*'MATKL'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de mercadorias' '20',
*'SPART'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Setor de atividade' '20',
*'GEWEI'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade de peso' '20',
*'MEINS'   'T_MRP2'   'MAKT ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade de medida básica' '20',
*'MAKTX'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Texto breve de material' '30',
*'WERKS'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Centro' '20',
*'MTVFP'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de verificação' '20',
*'STEUC'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Código de controle p/imposto' '20',
*'KAUTB'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Dados de centro para material' '20',
*'BKLAS'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Classe de avaliação' '20',
*'MLAST'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Apropriação custos do ledger' '20',
*'PEINH'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade preço' '20',
*'MTUSE'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Utilização de material' '20',
*'MTORG'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Origem de material' '20',
*'VKORG'   'T_MRP2'   'TVKO ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Organização de vendas' '20',
*'VTWEG'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Canal de distribuição' '20',
*'SKTOF'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Dados de venda para material' '20',
*'MTPOS'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de categorias de item' '20',
*'MESSAGE'   'T_MRP2'   ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Mensagem Erro' '40'.
*
*
*ENDFORM.

**********************************************************************
* tratamento valores
**********************************************************************
FORM F_CHANGE_TEXT CHANGING P_OUT_TEXT.

  DATA: L_INDEX     TYPE I,
        L_LENGTH    TYPE I,
        L_CHARACTER TYPE C,
        L_ALLOWED   TYPE STRING.

  TRANSLATE P_OUT_TEXT TO LOWER CASE.

  L_ALLOWED = '1234567890,'.
  L_LENGTH  = STRLEN( P_OUT_TEXT ).
  L_INDEX   = 0.

  WHILE L_LENGTH GT L_INDEX.
    L_CHARACTER = P_OUT_TEXT+L_INDEX(1).
    SEARCH L_ALLOWED FOR L_CHARACTER.
    IF SY-SUBRC NE 0.
      REPLACE L_CHARACTER WITH SPACE INTO P_OUT_TEXT.
    ENDIF.
    L_INDEX = L_INDEX + 1.
  ENDWHILE.

  REPLACE ALL OCCURRENCES OF REGEX ',' IN P_OUT_TEXT WITH '.'.
  CONDENSE P_OUT_TEXT NO-GAPS.

ENDFORM.
