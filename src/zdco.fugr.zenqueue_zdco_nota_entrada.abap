FUNCTION zenqueue_zdco_nota_entrada.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MODE_ZDCO_VINC) TYPE  ENQMODE DEFAULT 'E'
*"     VALUE(MANDT) TYPE  ZDCO_NF_ENTRADA-MANDT DEFAULT SY-MANDT
*"     VALUE(NU_DCO) TYPE  ZDCO_NF_ENTRADA-NU_DCO OPTIONAL
*"     VALUE(_SCOPE) DEFAULT '2'
*"     VALUE(_WAIT) DEFAULT SPACE
*"     VALUE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"  EXCEPTIONS
*"      FOREIGN_LOCK
*"      SYSTEM_FAILURE
*"----------------------------------------------------------------------


  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __wait TYPE ddenqwait.
  __wait = _wait.
  __scope = _scope.

  DATA: BEGIN OF %a_zdco_nf_entrada,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt  TYPE zdco_nf_entrada-mandt,
              nu_dco TYPE zdco_nf_entrada-nu_dco,
        END OF %a_zdco_nf_entrada.
* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zdco_nf_entrada.

  MOVE nu_dco TO %a_zdco_nf_entrada-nu_dco.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO:
         %a_zdco_nf_entrada-mandt.
  ENDIF.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZDCO_NF_ENTRADA'.
  __seqta_tab-gmode = mode_zdco_vinc.
  __seqta_tab-garg = %a_zdco_nf_entrada.
  APPEND __seqta_tab.

* Fixar bloqueio:
  PERFORM send_enqueue(saplsena)
          TABLES __seqta_tab
          USING '1' __scope __wait ' ' 'EZDCO_NF_ENTRADA' _collect.

ENDFUNCTION.
