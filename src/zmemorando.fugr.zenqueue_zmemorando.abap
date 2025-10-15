FUNCTION zenqueue_zmemorando.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE) TYPE  ENQMODE DEFAULT 'X'
*"     REFERENCE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(NR_MEMORANDO) TYPE  Z_MEMORANDO
*"     REFERENCE(_SCOPE) DEFAULT '2'
*"     REFERENCE(_WAIT) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT SPACE
*"  EXCEPTIONS
*"      FOREIGN_LOCK
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __wait TYPE ddenqwait.

  __wait = _wait.
  __scope = _scope.

  DATA: BEGIN OF %a_zdoc_memorando,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt         TYPE sy-mandt,
              nr_memorando  TYPE z_memorando,
        END OF %a_zdoc_memorando.
* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zdoc_memorando.

  MOVE: nr_memorando TO %a_zdoc_memorando-nr_memorando.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO: %a_zdoc_memorando-mandt.
  ENDIF.

* Preencher tab.bloqueio:
  __seqta_tab-gname = 'ZDOC_MEMORANDO'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_zdoc_memorando.
  APPEND __seqta_tab.

* Fixar bloqueio:
  PERFORM send_enqueue(saplsena) TABLES __seqta_tab USING '1' __scope __wait ' ' 'EZDOC_MEMORANDO' _collect.

ENDFUNCTION.
