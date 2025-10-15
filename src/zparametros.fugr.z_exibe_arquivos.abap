FUNCTION Z_EXIBE_ARQUIVOS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CAMINHO) TYPE  STRING
*"  TABLES
*"      TE_FILENAME STRUCTURE  SDOKPATH
*"  EXCEPTIONS
*"      DIRETORIO_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: TL_FILE_TABLE TYPE TABLE OF  SDOKPATH WITH HEADER LINE,
        TL_DIR_TABLE  TYPE TABLE OF  SDOKPATH WITH HEADER LINE,
        V_REPORT     LIKE SY-REPID,
        WL_CAMINHO(255).

  REFRESH: TL_FILE_TABLE, TL_DIR_TABLE, ESTRUTURA, EVENTS, TG_FILENAME,
           TG_SAIDA.

  WL_CAMINHO = I_CAMINHO.
  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      DIRECTORY        = WL_CAMINHO
*   FILTER           = '*.*'
* IMPORTING
*   FILE_COUNT       =
*   DIR_COUNT        =
    TABLES
      FILE_TABLE       = TL_FILE_TABLE
      DIR_TABLE        = TL_DIR_TABLE
* EXCEPTIONS
*   CNTL_ERROR       = 1
*   OTHERS           = 2
            .
  IF SY-SUBRC EQ 0.
    LOOP AT TL_FILE_TABLE.
      TG_SAIDA-FILENAME = TL_FILE_TABLE-PATHNAME.

      APPEND TG_SAIDA.
      CLEAR: TG_SAIDA.
    ENDLOOP.

    IF TG_SAIDA[] IS NOT INITIAL.
      PERFORM MONTAR_LAYOUT.
      PERFORM DEFINIR_EVENTOS.

      LAYOUT-BOX_FIELDNAME = 'MARK'.
      LAYOUT-BOX_TABNAME  = 'TL_SAIDA'.
      V_REPORT = SY-REPID.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
               EXPORTING
                    I_CALLBACK_PROGRAM      = V_REPORT
                    I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
                    IT_FIELDCAT             = ESTRUTURA[]
*                    IT_EVENTS               = EVENTS[]
                    IS_LAYOUT               = LAYOUT
*            IT_SORT                 = T_SORT[]
                    I_SAVE                  = 'A'
                    I_SCREEN_START_COLUMN   = 3
                    I_SCREEN_START_LINE     = 3
                    I_SCREEN_END_COLUMN     = 100
                     I_SCREEN_END_LINE      = 13
               TABLES
                    T_OUTTAB                = TG_SAIDA.

      TE_FILENAME[] = TG_FILENAME[].
    ENDIF.


  ELSE.
    RAISE DIRETORIO_NOT_FOUND.
  ENDIF.




ENDFUNCTION.
