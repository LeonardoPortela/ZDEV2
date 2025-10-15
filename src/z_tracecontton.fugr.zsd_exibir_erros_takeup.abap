FUNCTION zsd_exibir_erros_takeup.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_DADOS STRUCTURE  ZSDT0295
*"----------------------------------------------------------------------

  t_saida[] = t_dados[].

  LOOP AT t_saida  INTO w_saida.
    w_saida-data_takeup = w_saida-data_takeup+6(2) && '.' &&
                          w_saida-data_takeup+4(2) && '.' &&
                          w_saida-data_takeup(4).
    MODIFY t_saida FROM w_saida INDEX sy-tabix.
  ENDLOOP.

  l_mesg1 = 'Foram encontrados Erros durante a validação do Arquivo.'.
  l_mesg2 = 'Verifique a seguir.'.

  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
    EXPORTING
      titel        = 'Aviso'
      textline1    = l_mesg1
      textline2    = l_mesg2
*     textline3    = l_mesg3
      start_column = 25
      start_row    = 6.

*-----------------------------
* exibe log
*-----------------------------
  CALL SCREEN 200 STARTING AT 40  5
                    ENDING AT 152 16.

ENDFUNCTION.
