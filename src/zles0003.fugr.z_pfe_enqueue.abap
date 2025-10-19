function z_pfe_enqueue.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(_SCOPE) DEFAULT '2'
*"     REFERENCE(_WAIT) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT SPACE
*"  EXCEPTIONS
*"      FOREIGN_LOCK
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  data: __seqta_tab type seqta occurs 01 with header line,
        __scope type ddenqscope,
        __wait type ddenqwait.

  __wait = _wait.
  __scope = _scope.

  data: begin of %a_zpfe_lote,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt         type sy-mandt,
              nm_lote       type zpfe_numero_lote,
        end of %a_zpfe_lote.
* Inicialização argumento bloqueio:
  call 'C_ENQ_WILDCARD' id 'HEX0' field %a_zpfe_lote.

  move: nm_lote  to %a_zpfe_lote-nm_lote.

  if not mandt is initial.
    move mandt to: %a_zpfe_lote-mandt.
  endif.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZPFE_LOTE'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_zpfe_lote.
  append __seqta_tab.

* Fixar bloqueio:
  perform send_enqueue(saplsena) tables __seqta_tab using '1' __scope __wait ' ' 'EZPFE_LOTE' _collect.

endfunction.
