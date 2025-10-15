FUNCTION zsd_checa_carga_hvi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(E_OK) TYPE  CHAR1
*"  TABLES
*"      T_DADOS STRUCTURE  ZMMI0001
*"----------------------------------------------------------------------

  e_ok = abap_true.

  CHECK t_dados[] IS NOT INITIAL.

  FREE: t_zmmi0001.

  SELECT *
    FROM zppt0033
    INTO TABLE t_zppt0033
     FOR ALL ENTRIES IN t_dados
   WHERE lifnr   = t_dados-lifnr
     AND cod_gs1 = t_dados-cod_gs1
     AND safra   = t_dados-safra.

  CHECK t_zppt0033[] IS NOT INITIAL.

  SORT t_zppt0033 BY lifnr cod_gs1 safra.

  LOOP AT t_dados INTO w_dados.
    READ TABLE t_zppt0033 INTO w_zppt0033 WITH KEY lifnr = w_dados-lifnr
                                                   cod_gs1 = w_dados-cod_gs1
                                                   safra = w_dados-safra
                                          BINARY SEARCH.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING w_dados TO w_zmmi0001.
    w_zmmi0001-charg         = w_zppt0033-digito && w_dados-fardos.
    APPEND w_zmmi0001       TO t_zmmi0001.
  ENDLOOP.

  SELECT *
    FROM zmmt0027
    INTO TABLE t_saida
     FOR ALL ENTRIES IN t_zmmi0001
   WHERE charg = t_zmmi0001-charg
     AND werks = t_zmmi0001-werks
     AND matkl = t_zmmi0001-matkl.

  CHECK t_saida[] IS NOT INITIAL.

  l_mesg1 = 'Serão listados a seguir os Fardos que já foram importados'.
  l_mesg2 = 'neste arquivo. Confirme a substituição das informações.'.
  l_mesg3 = 'Os novos fardos serão incluídos normalmente.'.

  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
    EXPORTING
      titel        = 'Aviso'
      textline1    = l_mesg1
      textline2    = l_mesg2
      textline3    = l_mesg3
      start_column = 25
      start_row    = 6.

*-----------------------------
* exibe log
*-----------------------------
  CALL SCREEN 100 STARTING AT 40  5
                    ENDING AT 152 16.

  e_ok = l_confirma.

ENDFUNCTION.
