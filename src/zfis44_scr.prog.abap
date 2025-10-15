*&---------------------------------------------------------------------*
*& Include          ZFIS44_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_conf   TYPE c RADIOBUTTON GROUP rg1,
            p_desc   TYPE c RADIOBUTTON GROUP rg1,
            p_mail   TYPE c RADIOBUTTON GROUP rg1,
            p_mail_2 TYPE c RADIOBUTTON GROUP rg1.

SELECTION-SCREEN END OF BLOCK b1.
