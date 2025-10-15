function zfu_anular_atrib_exec_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_MSG TYPE  BAPIRET2_TAB
*"      T_DADOS TYPE  ZPMT0083_T
*"----------------------------------------------------------------------
  data: it_zpmt0083 type table of zpmt0083.


  check t_dados[] is not initial.

  free: it_zpmt0083.

  select * from zpmt0083 into table it_zpmt0083
         for all entries in t_dados
         where aufnr eq t_dados-aufnr.

  if it_zpmt0083 is not initial.
    delete zpmt0083 from table it_zpmt0083.
    if sy-subrc eq 0.
      commit work.
    endif.
  endif.



endfunction.
