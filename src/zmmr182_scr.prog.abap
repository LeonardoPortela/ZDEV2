*&---------------------------------------------------------------------*
*&  Include           ZMMR182_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bukrs FOR ekko-bukrs OBLIGATORY,
                s_werks FOR ekpo-werks OBLIGATORY,
                s_budat FOR ekbe-budat OBLIGATORY,
                s_ebeln FOR ekko-ebeln.

PARAMETERS: p_dif  TYPE flag.
*PARAMETERS: p_grao TYPE flag.

SELECTION-SCREEN END OF BLOCK b1.
