*&---------------------------------------------------------------------*
*& Include          ZSDR0181_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN:  BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

  PARAMETERS: p_cfop_e  TYPE c RADIOBUTTON GROUP g1,
              p_cfop_f  TYPE c RADIOBUTTON GROUP g1,
              p_cfop_t  TYPE c RADIOBUTTON GROUP g1,
              p_cfop_l  TYPE c RADIOBUTTON GROUP g1,
              p_cfop_r  TYPE c RADIOBUTTON GROUP g1,
              p_matkl   TYPE c RADIOBUTTON GROUP g1,
              p_fkart   TYPE c RADIOBUTTON GROUP g1,
              p_fkarti  TYPE c RADIOBUTTON GROUP g1,
              p_final   TYPE c RADIOBUTTON GROUP g1,
              p_fdev_e  TYPE c RADIOBUTTON GROUP g1,
              p_user_f  TYPE c RADIOBUTTON GROUP g1,
              p_ativa   TYPE c RADIOBUTTON GROUP g1,
              p_ufxml   TYPE c RADIOBUTTON GROUP g1,
              p_aprov   TYPE c RADIOBUTTON GROUP g1. "US #180203 - MMSILVA - 26.05.2025
SELECTION-SCREEN end of block b1.
