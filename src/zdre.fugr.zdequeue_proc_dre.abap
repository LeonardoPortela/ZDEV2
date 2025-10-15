function zdequeue_proc_dre.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------

  data: __seqta_tab type seqta occurs 01 with header line,
        __scope     type ddenqscope,
        __synchron  type ddenqsync.

  __synchron = _synchron.
  __scope = _scope.

  data: begin of %a_processa_dre,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt	type sy-mandt,
        end of %a_processa_dre.
* Inicialização argumento bloqueio:
  call 'C_ENQ_WILDCARD' id 'HEX0' field %a_processa_dre.

* Preencher tab.bloqueio:
  move mandt to: %a_processa_dre-mandt.

  __seqta_tab-gname = 'ZPROCESSA_DRE'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_processa_dre.
  append __seqta_tab.

* Lock assigned:
  perform send_enqueue(saplsena) tables __seqta_tab
    using '2' __scope ' ' __synchron 'EZPROCESSA_DRE' _collect.

endfunction.
