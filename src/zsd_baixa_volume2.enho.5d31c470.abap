"Name: \PR:SAPLCATSXT_UTIL\TY:LCL_SIMPLE_TEXT_EDITOR\ME:START\SE:END\EI
ENHANCEMENT 0 ZSD_BAIXA_VOLUME2.
*
*-CS2020000343 - 23.04.2021 - JT - inicio
  IF g_no_toolbar = abap_true.
    CALL METHOD editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.
  ENDIF.
*-CS2020000343 - 23.04.2021 - JT - fim
*
ENDENHANCEMENT.
