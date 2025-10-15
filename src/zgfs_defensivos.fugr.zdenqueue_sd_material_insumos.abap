FUNCTION zdenqueue_sd_material_insumos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(CHAVE) TYPE  MATNR
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope     TYPE ddenqscope,
        __synchron  TYPE ddenqsync.

  __synchron = _synchron.
  __scope = _scope.

  DATA: BEGIN OF %a_it_zsdt0303,
          mandt	TYPE sy-mandt,
          matnr TYPE zsdt0303-matnr,
        END OF %a_it_zsdt0303.

  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_it_zsdt0303.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO: %a_it_zsdt0303-mandt.
  ENDIF.

  MOVE: chave TO %a_it_zsdt0303-matnr.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'IT_ZSDT0303'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg  = %a_it_zsdt0303.
  APPEND __seqta_tab.

* Lock assigned:
  PERFORM send_enqueue(saplsena) TABLES __seqta_tab USING '2' __scope ' ' __synchron 'EZ_ZSDT0303' _collect.

ENDFUNCTION.
