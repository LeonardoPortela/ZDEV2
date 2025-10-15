*&---------------------------------------------------------------------*
*&  Include           ZMMR0044_TOP
*&---------------------------------------------------------------------*


TABLES: sscrfields.

TYPES: BEGIN OF ty_dados,
         linha TYPE string,
       END OF ty_dados.

*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
DATA: git_zprovcoupa03 TYPE TABLE OF zprovcoupa03,
      git_dados        TYPE TABLE OF alsmex_tabline,
      git_dados_aux    TYPE TABLE OF alsmex_tabline.


*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
DATA: gwa_dados     LIKE LINE OF git_dados,
      gwa_dados_aux LIKE LINE OF git_dados_aux.


DATA: exc_ref  TYPE REF TO cx_root,
      exc_text TYPE string.

DATA: ok_code            LIKE sy-ucomm,
      save_ok            LIKE sy-ucomm,
      g_container        TYPE scrfname VALUE 'C_ALV',
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      t_fieldcat         TYPE lvc_t_fcat,
      t_exclude          TYPE ui_functions,
      sg_layout          TYPE lvc_s_layo.

CONSTANTS: c_car       TYPE c VALUE 'CAR'          LENGTH 4.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01. "#EC SHAREOK

PARAMETERS: p_arq  TYPE file_table-filename OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b02.
SELECTION-SCREEN COMMENT /1(75) comm0.
SELECTION-SCREEN COMMENT /1(75) com00.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(75) comm1.
SELECTION-SCREEN COMMENT /1(75) comm2.
SELECTION-SCREEN COMMENT /1(75) comm3.
SELECTION-SCREEN COMMENT /1(75) comm4.
SELECTION-SCREEN COMMENT /1(75) comm5.
SELECTION-SCREEN COMMENT /1(75) comm6.
SELECTION-SCREEN COMMENT /1(75) comm7.
SELECTION-SCREEN COMMENT /1(75) comm8.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
* Layout dos campos para carga de infotipos
  PERFORM f_layout.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.
  PERFORM f_caminho CHANGING p_arq.
