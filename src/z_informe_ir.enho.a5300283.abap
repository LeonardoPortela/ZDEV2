"Name: \PR:HBRCCED0\FO:MOVE_DCC\SE:END\EI
ENHANCEMENT 0 Z_INFORME_IR.
*  lt_compl_info[] =  ls_dcc-compl_info[].
*
*  DESCRIBE TABLE lt_compl_info LINES lv_lines.
*
*  DO lv_lines TIMES.
*    lv_lines2 = lv_lines2 + 1.
*    READ TABLE lt_compl_info INTO ls_compl_info INDEX lv_lines2.
*    IF sy-subrc IS INITIAL.
*      IF ls_compl_info-line CO space.
*        IF lv_del1 = space.
*          lv_del1 = `X`.
*          CONTINUE.
*        ELSE.
*          IF lv_del2 = space.
*            lv_del2 = `X`.
*            CONTINUE.
*          ELSE.
*            IF lv_del1 EQ `X` AND
*               lv_del2 EQ `X`.
*              DELETE lt_compl_info  INDEX lv_lines2.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        clear: lv_del1,
*               lv_del2.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*  ENDDO.

  cs_data-compl_info[]     = ls_dcc-compl_info[].
  cs_data-pensioner_data[] = ls_dcc-pensioner_data[].

* Inicio - alteração - RMNI -  CSTASK0012612 - 27.01.2023
  IF pn-begda(4) = '2022'.

*** 27.02.2023  - BRADESCO SAUDE
    CLEAR: lv_position.
    LOOP AT cs_data-compl_info INTO DATA(lwa-compl_info_bra).
      DATA(lv_tabix_b) = sy-tabix.
      FIND FIRST OCCURRENCE OF 'BRADESCO SAUDE SA CNPJ: ( 92693118000160 ) ANS: 005711' IN lwa-compl_info_bra-line MATCH COUNT lv_position.
      IF lv_position IS NOT INITIAL.
        wa_compl_info = lv_brasaude.
        MODIFY cs_data-compl_info FROM wa_compl_info INDEX lv_tabix_b TRANSPORTING line.
        CLEAR:  wa_compl_info.
        EXIT.
      ENDIF.
    ENDLOOP.
*** 27.02.2023 - BRADESCO SAUDE FIM


** 27.02.2023 - Inicio
*    DO 9 TIMES.
*      DELETE cs_data-compl_info INDEX 9.
*    ENDDO.

    DATA(lv_table) = lines( cs_data-compl_info ).
    CLEAR: lv_position.
    LOOP AT cs_data-compl_info INTO DATA(lwa-compl_info).
      DATA(lv_tabix_x) = sy-tabix.
      FIND FIRST OCCURRENCE OF 'Direitos da Criança' IN lwa-compl_info-line MATCH COUNT lv_position.
      IF lv_position IS NOT INITIAL.
        lv_table = ( lv_table - lv_tabix_x ).
        DATA(lv_count) =  lv_tabix_x  + 1.
        DO lv_table TIMES.
          DELETE cs_data-compl_info INDEX lv_count.
        ENDDO.
      ENDIF.
    ENDLOOP.
** 27.02.2023 - Fim

    wa_compl_info = lv_fmunic.
    APPEND wa_compl_info TO cs_data-compl_info.
    CLEAR wa_compl_info.

    APPEND wa_compl_info TO cs_data-compl_info.

    wa_compl_info = c_linha4.
    APPEND wa_compl_info TO cs_data-compl_info.
    CLEAR wa_compl_info.

    wa_compl_info = c_cabec.
    APPEND wa_compl_info TO cs_data-compl_info.
    CLEAR wa_compl_info.

    APPEND wa_compl_info TO cs_data-compl_info.

    wa_compl_info = lv_brad.
    APPEND wa_compl_info TO cs_data-compl_info.
    CLEAR wa_compl_info.

    wa_compl_info = lv_icat.
    APPEND wa_compl_info TO cs_data-compl_info.
    CLEAR wa_compl_info.

    APPEND wa_compl_info TO cs_data-compl_info.
  ENDIF.
* Final  - alteração - RMNI -  CSTASK0012612 - 27.01.2023

ENDENHANCEMENT.
