FUNCTION zenqueue_romaneios_centro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_MODE) TYPE  ENQMODE DEFAULT 'E'
*"     REFERENCE(V_MANDT) TYPE  ZSDT0001-MANDT DEFAULT SY-MANDT
*"     REFERENCE(V_WERKS) TYPE  ZSDT0001-BRANCH
*"     VALUE(V_MATNR) TYPE  ZSDT0001-MATNR OPTIONAL
*"     REFERENCE(V_SCOPE) DEFAULT '2'
*"     REFERENCE(V_WAIT) DEFAULT SPACE
*"     REFERENCE(V_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"  EXCEPTIONS
*"      FOREIGN_LOCK
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __wait TYPE ddenqwait.
  __wait = v_wait.
  __scope = v_scope.


  DATA: BEGIN OF %a_zsdt0001,
*       Argumento bloqueio p/tab.zsdt0001
              mandt  TYPE zsdt0001-mandt,
              branch TYPE zsdt0001-branch,
              matnr  TYPE zsdt0001-matnr,
        END OF %a_zsdt0001.

* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zsdt0001.

  IF NOT v_mandt IS INITIAL.
    MOVE v_mandt TO:
         %a_zsdt0001-mandt.
  ENDIF.

  MOVE v_werks TO %a_zsdt0001-branch.

  IF NOT v_matnr IS INITIAL.
    MOVE v_matnr TO %a_zsdt0001-matnr.
  ENDIF.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZSDT0001'.
  __seqta_tab-gmode = v_mode.
  __seqta_tab-garg = %a_zsdt0001.
  APPEND __seqta_tab.


* Lock assigned:
  PERFORM send_enqueue(saplsena)
          TABLES __seqta_tab
          USING '1' __scope __wait ' ' 'EZSDT0001' v_collect.


ENDFUNCTION.
