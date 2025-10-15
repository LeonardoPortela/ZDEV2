*&---------------------------------------------------------------------*
*& Include          ZFIS43_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_uf     FOR adrc-region,
                  s_bukrs  FOR j_1bbranch-bukrs OBLIGATORY,
                  s_branch FOR j_1bbranch-branch.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: rb_conf TYPE c RADIOBUTTON GROUP rg_1 DEFAULT 'X',
              rb_desc TYPE c RADIOBUTTON GROUP rg_1.

SELECTION-SCREEN END OF BLOCK b2.
