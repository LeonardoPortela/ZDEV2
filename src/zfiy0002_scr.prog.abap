*&---------------------------------------------------------------------*
*&  Include           ZFIY0002_SCR
*&---------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-000.

  PARAMETERS:  r1 RADIOBUTTON GROUP rad1 USER-COMMAND ent DEFAULT 'X'.
  PARAMETERS:  r2 RADIOBUTTON GROUP rad1." .
  PARAMETER:   p_path LIKE rlgrap-filename." OBLIGATORY.

  PARAMETERS: r_cap  RADIOBUTTON GROUP rad2,
              r_prov RADIOBUTTON GROUP rad2.

  SELECTION-SCREEN END OF BLOCK bl1.
