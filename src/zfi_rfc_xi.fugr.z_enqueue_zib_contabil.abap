FUNCTION z_enqueue_zib_contabil.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MODE) TYPE  ENQMODE DEFAULT 'E'
*"     VALUE(MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     VALUE(RG_ATUALIZADO) TYPE  ZIB_CONTABIL-RG_ATUALIZADO OPTIONAL
*"     VALUE(_SCOPE) DEFAULT '2'
*"     VALUE(_WAIT) DEFAULT SPACE
*"     VALUE(_COLLECT) TYPE  DDENQCOLL OPTIONAL
*"     REFERENCE(OPCODE) TYPE  SEQAREA-OPCODE DEFAULT '1'
*"----------------------------------------------------------------------
* Módulo função gerado p/obj.bloq.EFZIB_CONTABIL

  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __wait TYPE ddenqwait.
  __wait = _wait.
  __scope = _scope.

  DATA: BEGIN OF %a_zib_contabil,
*       Argumento bloqueio p/tab.BKPF
              mandt         TYPE zib_contabil-mandt,
              rg_atualizado TYPE zib_contabil-rg_atualizado,
        END OF %a_zib_contabil.

* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zib_contabil.

* Atribuição de parâmetros de bloqueio a campos de bloqueio:

  IF NOT mandt IS INITIAL.
    MOVE mandt TO:
         %a_zib_contabil-mandt.
  ENDIF.

  IF NOT rg_atualizado IS INITIAL .
    MOVE rg_atualizado TO:
         %a_zib_contabil-rg_atualizado.
  ENDIF.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZIB_CONTABIL'.
  __seqta_tab-gmode = mode.
  __seqta_tab-garg = %a_zib_contabil.
  APPEND __seqta_tab.

* Fixar bloqueio:
  PERFORM send_enqueue(saplsena)
          TABLES __seqta_tab
          USING opcode __scope __wait ' ' 'EFZIB_CONTABIL' _collect.

ENDFUNCTION.
