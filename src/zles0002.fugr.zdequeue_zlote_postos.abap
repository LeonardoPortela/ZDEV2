FUNCTION zdequeue_zlote_postos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(TRANSPORTADOR) TYPE  TDLNR
*"     REFERENCE(POSTO) TYPE  LIFNR
*"     REFERENCE(LOTE) TYPE  CHAR10
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __synchron TYPE ddenqsync.
  __synchron = _synchron.
  __scope = _scope.

  DATA: BEGIN OF %a_zlest0015,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt	        TYPE sy-mandt,
              transportador	TYPE tdlnr,
              posto	        TYPE lifnr,
              lote          TYPE char10,
        END OF %a_zlest0015.
* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zlest0015.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO:
         %a_zlest0015-mandt.
  ENDIF.

  MOVE: transportador TO %a_zlest0015-transportador,
        posto         TO %a_zlest0015-posto,
        lote          TO %a_zlest0015-lote.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZLEST0015'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_zlest0015.
  APPEND __seqta_tab.

* Lock assigned:
  PERFORM send_enqueue(saplsena)
          TABLES __seqta_tab
          USING '2' __scope ' ' __synchron 'EZLEST0015' _collect.

ENDFUNCTION.
