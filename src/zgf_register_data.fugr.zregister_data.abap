FUNCTION zregister_data.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DB_TAB) TYPE  TABNAME
*"     REFERENCE(I_STCNAM) TYPE  TABNAME
*"     REFERENCE(I_TITLE) TYPE  CUA_TIT_TX
*"     REFERENCE(I_START_LIN) TYPE  NUMC3 OPTIONAL
*"     REFERENCE(I_START_COL) TYPE  NUMC3 OPTIONAL
*"     REFERENCE(I_END_LIN) TYPE  NUMC3 OPTIONAL
*"     REFERENCE(I_END_COL) TYPE  NUMC3 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  p_db_tab    = i_db_tab.
  p_stcnam    = i_stcnam.
  p_title     = i_title.
  p_start_lin = i_start_lin.
  p_start_col = i_start_col.
  p_end_lin   = i_end_lin.
  p_end_col   = i_end_col.

  FREE: obj_container,
        obj_alv,
        obj_toolbar.

*-----------------------
*-Monta Estrutura Saida
*-----------------------
  UNASSIGN: <fs_it_saida>,
            <fs_wa_saida>,
            <fs_wa_saida_tmp>,
            <fs_table>,
            <fs_wa_table>,
            <fs_wa_registro_manter>,
            <fs_wa_registro_manter_tmp>.

  CREATE DATA d_it_saida TYPE TABLE OF (p_stcnam).
  ASSIGN d_it_saida->* TO <fs_it_saida>.

  CREATE DATA d_wa_saida LIKE LINE OF <fs_it_saida>.
  ASSIGN d_wa_saida->* TO <fs_wa_saida>.

  CREATE DATA d_wa_saida_tmp LIKE LINE OF <fs_it_saida>.
  ASSIGN d_wa_saida_tmp->* TO <fs_wa_saida_tmp>.
  "Fim Monta Estrutura Saida

  CREATE DATA d_table TYPE STANDARD TABLE OF (p_db_tab).
  ASSIGN d_table->* TO <fs_table>.

  CREATE DATA d_table_wa LIKE LINE OF <fs_table>.
  ASSIGN d_table_wa->* TO <fs_wa_table>.

  CREATE DATA d_wa_registro_manter LIKE LINE OF <fs_table>.
  ASSIGN d_wa_registro_manter->* TO <fs_wa_registro_manter>.

  CREATE DATA d_wa_registro_manter_tmp LIKE LINE OF <fs_table>.
  ASSIGN d_wa_registro_manter_tmp->* TO <fs_wa_registro_manter_tmp>.

*-----------------------
*-Processamento
*-----------------------
  PERFORM: f_selecionar_dados,
           f_processa_dados.

  IF p_start_lin IS INITIAL OR
     p_start_col IS INITIAL OR
     p_end_lin   IS INITIAL OR
     p_end_col   IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    CALL SCREEN 0100 STARTING AT p_start_col p_start_lin
                       ENDING AT p_end_col   p_end_lin.
  ENDIF.

ENDFUNCTION.
