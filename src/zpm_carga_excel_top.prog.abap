*&---------------------------------------------------------------------*
*&  Include           ZPM_CARGA_EXCEL_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA                                                                 *
*----------------------------------------------------------------------*

DATA:
  gr_alv      TYPE REF TO     cl_salv_table,
  gr_alv_erro TYPE REF TO     cl_salv_table.

DATA: gva_ucomm TYPE sy-ucomm.
DATA: git_ucomm TYPE TABLE OF sy-ucomm.

TABLES: mseg, zppt0035, t001, icon, sscrfields.

DATA: tg_excel   TYPE TABLE OF zalsmex_tabline.

DATA:l_leave      TYPE syst_ucomm,
     l_sel_button TYPE smp_dyntxt,
     l_opcao      TYPE char1,
     l_nfps       TYPE znfnum,
     l_data_char  TYPE char10,
     l_tabix      TYPE sy-tabix,
     l_icon_name  TYPE icon-name.

TYPES: BEGIN OF ty_0017,
         farol      TYPE icon_d,
         mandt      TYPE mandt,
         class_oper TYPE zpmr0001-class_oper,
         herst      TYPE zpmr0001-herst,
         typbz      TYPE zpmr0001-typbz,
         consumo    TYPE zpmr0001-consumo,
         variacao   TYPE zpmr0001-variacao,
         rbnr       TYPE zpmr0001-rbnr,
         tq_comb    TYPE zpmr0001-tq_comb,
         tolerancia TYPE zpmr0001-tolerancia,
         dt_criacao TYPE zpmr0001-dt_criacao,
       END OF ty_0017.

TYPES: BEGIN OF ty_0074.
TYPES: farol TYPE icon_d.
       INCLUDE STRUCTURE zpmt0074.
TYPES: END OF ty_0074.

TYPES: BEGIN OF ty_0075.
TYPES: farol TYPE icon_d.
       INCLUDE STRUCTURE zpmt0075.
TYPES: END OF ty_0075.



TABLES: zpmr0001, zpmt0074, zpmt0075.
DATA:
  gt_data_0017 TYPE TABLE OF ty_0017,
  gt_data_0074 TYPE TABLE OF ty_0074,
  gt_data_0075 TYPE TABLE OF ty_0075.

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*
CONSTANTS:
  gc_light_inactive TYPE            icon_d    VALUE '@EB@',
  gc_light_red      TYPE            icon_d    VALUE '@0A@',
  gc_light_yellow   TYPE            icon_d    VALUE '@09@',
  gc_light_green    TYPE            icon_d    VALUE '@08@'.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_0017 RADIOBUTTON GROUP rad1 DEFAULT 'X',
              p_0093 RADIOBUTTON GROUP rad1,
              p_0094 RADIOBUTTON GROUP rad1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:     p_file         LIKE rlgrap-filename MODIF ID t1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'


AT SELECTION-SCREEN OUTPUT.
  IF p_0017 = 'X'.
    DATA(l_opcao) = '0017'.
  ELSEIF p_0093 = 'X'.
    l_opcao = '0093'.
  ELSEIF p_0094 = 'X'.
    l_opcao = '0094'.
  ENDIF.

  "Desabilitando botões
*  gva_ucomm = 'ONLI'. "Executar
*  APPEND gva_ucomm TO git_ucomm.
*  CLEAR gva_ucomm.
  gva_ucomm = 'SPOS'. "Salvar
  APPEND gva_ucomm TO git_ucomm.
  CLEAR gva_ucomm.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
*     P_PROGRAM = ' '
    TABLES
      p_exclude = git_ucomm.


**********************************************************************
*SELECTION-SCREEN p_file
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM f_busca_arquivo USING p_file.
*  PERFORM f_le_arquivo.

  CHECK p_file IS NOT INITIAL.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ONLI'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.
