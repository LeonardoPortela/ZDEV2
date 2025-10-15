*&---------------------------------------------------------------------*
*&  Include           ZFIY0019_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK bloque0 WITH FRAME TITLE text-000.

SELECT-OPTIONS: so_bukrs FOR ST_DATA-BUKRS.

SELECT-OPTIONS: so_witht FOR ST_LFBW-WITHT.

SELECTION-SCREEN SKIP.

PARAMETERS:   p_check  AS CHECKBOX USER-COMMAND ent .
PARAMETER:    p_path   LIKE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK bloque0.
