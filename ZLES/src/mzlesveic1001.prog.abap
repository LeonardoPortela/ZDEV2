*&---------------------------------------------------------------------*
*&  Include           MZLESVEIC1001
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN NESTING LEVEL 4.
SELECTION-SCREEN BEGIN OF BLOCK zb01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: b_placa  FOR  zlest0002-pc_veiculo,
                b_propr  FOR  zlest0002-proprietario,
                b_cidad  FOR  zlest0002-cd_cidade,
                b_estad  FOR  zlest0002-cd_uf.
SELECTION-SCREEN END OF BLOCK zb01.
SELECTION-SCREEN END OF SCREEN 1001.
