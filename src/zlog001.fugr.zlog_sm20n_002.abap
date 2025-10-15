FUNCTION zlog_sm20n_002.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USNAM) TYPE  USNAM
*"     VALUE(I_ERDAT) TYPE  ERDAT
*"  EXPORTING
*"     VALUE(R_LOGS) TYPE  RSAU_T_RESULT
*"----------------------------------------------------------------------

  DATA: lt_seltab TYPE TABLE OF rsparams.

  DATA: lva_data_ini TYPE erdat,
        lva_hora_ini TYPE erzet,
        lva_data_fim TYPE erdat,
        lva_hora_fim TYPE erzet,
        lva_user     TYPE usnam.

  DATA: lwa_dados_sm20n TYPE rsau_s_result.

  CLEAR: r_logs[].

  lva_data_ini   = i_erdat.
  lva_hora_ini   = '000000'.
  lva_data_fim   = i_erdat.
  lva_hora_fim   = '235959'.
  lva_user       = i_usnam.

  APPEND  VALUE #(  selname = 'STRTDATE' kind = 'P' sign    = 'I' option  = 'EQ' low    = lva_data_ini ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'STRTTIME' kind = 'P' sign    = 'I' option  = 'EQ' low    = lva_hora_ini ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'ENDDATE'  kind = 'P' sign    = 'I' option  = 'EQ' low    = lva_data_fim ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'ENDTIME'  kind = 'P' sign    = 'I' option  = 'EQ' low    = lva_hora_fim ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'USER'     kind = 'S' sign    = 'I' option  = 'EQ' low    = i_usnam      ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'LOGON'    kind = 'P' sign    = 'I' option  = 'EQ' low    = space        ) TO lt_seltab.

  "APPEND  VALUE #(  selname = 'S_VARI'   kind = 'P' sign    = 'I' option  = 'EQ' low    = '/ANALISE2'  ) TO lt_seltab.

  PERFORM f_prepare_run_time_info.

  SUBMIT rsau_read_log
           WITH SELECTION-TABLE lt_seltab

           "USING SELECTION-SET p_varia
           EXPORTING LIST TO MEMORY AND RETURN.

  PERFORM f_get_runtime_info.

  CHECK <t_data> IS ASSIGNED.

  LOOP AT <t_data> ASSIGNING <w_data>.
    CLEAR: lwa_dados_sm20n.
    MOVE-CORRESPONDING <w_data> TO lwa_dados_sm20n.
    APPEND lwa_dados_sm20n TO r_logs.
  ENDLOOP.


ENDFUNCTION.
