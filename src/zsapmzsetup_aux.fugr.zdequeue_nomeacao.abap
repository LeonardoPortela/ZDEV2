function zdequeue_nomeacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(ID_NOMEACAO_TRAN) TYPE  ZID_NOM
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------

  data: __seqta_tab type seqta occurs 01 with header line,
        __scope     type ddenqscope,
        __synchron  type ddenqsync.

  __synchron = _synchron.
  __scope = _scope.

  data: begin of %a_znom_transporte,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt	           type sy-mandt,
              id_nomeacao_tran type zid_nom,
        end of %a_znom_transporte.

* Inicialização argumento bloqueio:
  call 'C_ENQ_WILDCARD' id 'HEX0' field %a_znom_transporte.

  if not mandt is initial.
    move mandt to:
         %a_znom_transporte-mandt.
  endif.

  move: id_nomeacao_tran to %a_znom_transporte-id_nomeacao_tran.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZNOM_TRANSPORTE'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_znom_transporte.
  append __seqta_tab.

* Lock assigned:
  perform send_enqueue(saplsena)
          tables __seqta_tab
          using '2' __scope ' ' __synchron 'ZNOM_TRANSPORTE' _collect.

endfunction.
