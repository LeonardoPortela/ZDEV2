*&---------------------------------------------------------------------*
*&  Include          ZSDR0031_FARDO_ALV_CLASS_0300
*&---------------------------------------------------------------------*
*&TITULO  : Include para REL. VENDA ALGODÃO (ZSDR0031)
*&AUTOR   : Sara Trincha Oikawa
*&DATA.   : 12.05.2020
*&OBJETIVO: CS2018001961
*           O Objetivo deste novo relátório é exibir e comparar a qtde
*           de fardos e o peso que foram formados lote com o que foi
*           exportado. E nos casos onde há diferença exibir de forma
*           clara qual a instrução foi "formado o lote" e posteriormente
*           quando exportado, em qual instrução foi realizada a exportação.
*
*----------------------------------------------------------------------*
* INCLUDE ZSDR0031_FARDO_ALV_CLASS_0300                                *
*----------------------------------------------------------------------*
* This include has all the data declaration defined for ALV
************************************************************************

INCLUDE <ICON>.
* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
DATA : O_ALVGRID1        TYPE REF TO CL_GUI_ALV_GRID,
       O_ALVGRID2        TYPE REF TO CL_GUI_ALV_GRID,
       CONTAINER1        TYPE SCRFNAME VALUE 'GRID_0310',
       CONTAINER2        TYPE SCRFNAME VALUE 'GRID_0320',
       CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

*---------------------------------------------------------------------*
* Work Area
*---------------------------------------------------------------------*
       W_LAYOUT          TYPE LVC_S_LAYO,
       W_VARIANT         TYPE DISVARIANT.
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
* Constants
*---------------------------------------------------------------------*
CONSTANTS : C_LAY(1) TYPE C VALUE 'A' . " All Layouts
CONSTANTS: BEGIN OF C_MAIN_TAB,
             TAB1 LIKE SY-UCOMM VALUE '0300_TAB1', "
             TAB2 LIKE SY-UCOMM VALUE '0300_TAB2', "
           END OF C_MAIN_TAB.
