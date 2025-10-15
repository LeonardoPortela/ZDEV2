FUNCTION zenqueue_sd_material_insumos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(CHAVE) TYPE  MATNR
*"     REFERENCE(_SCOPE) DEFAULT '2'
*"     REFERENCE(_WAIT) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT SPACE
*"  EXCEPTIONS
*"      FOREIGN_LOCK
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope     TYPE ddenqscope,
        __wait      TYPE ddenqwait.

  __wait = _wait.
  __scope = _scope.

  DATA: BEGIN OF %a_it_zsdt0303,
          mandt TYPE sy-mandt,
          matnr TYPE  zsdt0303-matnr,
        END OF %a_it_zsdt0303.

  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_it_zsdt0303.

  MOVE: chave TO %a_it_zsdt0303-matnr.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO: %a_it_zsdt0303-mandt.
  ENDIF.

* Preencher tab.bloqueio:
  __seqta_tab-gname = 'IT_ZSDT0303'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_it_zsdt0303.
  APPEND __seqta_tab.

* Fixar bloqueio:
  PERFORM send_enqueue(saplsena)
   TABLES __seqta_tab
    USING '1' __scope __wait ' ' 'EZ_ZSDT0303' _collect.

ENDFUNCTION.
