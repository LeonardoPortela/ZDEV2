function zenqueue_proc_dre.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
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

  data: begin of %a_processa_dre,
              mandt type sy-mandt,
        end of %a_processa_dre.

* Inicialização argumento bloqueio:
  call 'C_ENQ_WILDCARD' id 'HEX0' field %a_processa_dre.

  move mandt to: %a_processa_dre-mandt.

* Preencher tab.bloqueio:
  __seqta_tab-gname = 'ZPROCESSA_DRE'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_processa_dre.
  append __seqta_tab.

* Fixar bloqueio:
  perform send_enqueue(saplsena) tables __seqta_tab using '1' __scope __wait ' ' 'EZPROCESSA_DRE' _collect.

endfunction.
