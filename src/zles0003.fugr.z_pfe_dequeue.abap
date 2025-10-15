function z_pfe_dequeue.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------

  data: __seqta_tab type seqta occurs 01 with header line,
        __scope type ddenqscope,
        __synchron type ddenqsync.
  __synchron = _synchron.
  __scope = _scope.

  data: begin of %a_zpfe_lote,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt	        type sy-mandt,
              nm_lote       type zpfe_numero_lote,
        end of %a_zpfe_lote.
* Inicialização argumento bloqueio:
  call 'C_ENQ_WILDCARD' id 'HEX0' field %a_zpfe_lote.

  if not mandt is initial.
    move mandt to:
         %a_zpfe_lote-mandt.
  endif.

  move: nm_lote to %a_zpfe_lote-nm_lote.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZPFE_LOTE'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_zpfe_lote.
  append __seqta_tab.

* Lock assigned:
  perform send_enqueue(saplsena)
          tables __seqta_tab
          using '2' __scope ' ' __synchron 'EZPFE_LOTE' _collect.

endfunction.
