*&---------------------------------------------------------------------*
*&  Include           ZFIY0001_SCR
*&---------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-000.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS: so_bukrs FOR st_data-bukrs .

  PARAMETER:    p_check2  AS CHECKBOX USER-COMMAND ent,"AS CHECKBOX,
                p_path    LIKE rlgrap-filename,
                p_pathi   LIKE rlgrap-filename.

  SELECTION-SCREEN SKIP 1.
  PARAMETER: p_cap  TYPE c RADIOBUTTON GROUP r1,
             p_prov TYPE c RADIOBUTTON GROUP r1.

  SELECTION-SCREEN END OF BLOCK bl1.

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-001.
  PARAMETERS:
                  p_check  AS CHECKBOX USER-COMMAND ent,"AS CHECKBOX,
                  p_path2  TYPE string .  "Path en front-end.

  SELECTION-SCREEN END OF BLOCK bl2.
