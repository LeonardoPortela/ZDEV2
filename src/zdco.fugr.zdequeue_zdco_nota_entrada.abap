FUNCTION zdequeue_zdco_nota_entrada.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MODE_ZDCO_VINC) TYPE  ENQMODE DEFAULT 'E'
*"     REFERENCE(MANDT) TYPE  ZDCO_NF_ENTRADA-MANDT DEFAULT SY-MANDT
*"     REFERENCE(NU_DCO) TYPE  ZDCO_NF_ENTRADA-NU_DCO
*"     REFERENCE(_SCOPE) DEFAULT '3'
*"     REFERENCE(_SYNCHRON) DEFAULT SPACE
*"     REFERENCE(_COLLECT) TYPE  DDENQCOLL DEFAULT ' '
*"----------------------------------------------------------------------


  DATA: __seqta_tab TYPE seqta OCCURS 01 WITH HEADER LINE,
        __scope TYPE ddenqscope,
        __synchron TYPE ddenqsync.
  __synchron = _synchron.
  __scope = _scope.

  DATA: BEGIN OF %a_zdco_nf_entrada,
*       Argumento bloqueio p/tab.ZDCO_NF_ENTRADA
              mandt  TYPE zdco_nf_entrada-mandt,
              nu_dco TYPE zdco_nf_entrada-nu_dco,
        END OF %a_zdco_nf_entrada.
* Inicialização argumento bloqueio:
  CALL 'C_ENQ_WILDCARD' ID 'HEX0' FIELD %a_zdco_nf_entrada.

  IF NOT mandt IS INITIAL.
    MOVE mandt TO:
         %a_zdco_nf_entrada-mandt.
  ENDIF.

  MOVE nu_dco TO %a_zdco_nf_entrada-nu_dco.

* Preencher tab.bloqueio:

  __seqta_tab-gname = 'ZDCO_NF_ENTRADA'.
  __seqta_tab-gmode = mode_zdco_vinc.
  __seqta_tab-garg = %a_zdco_nf_entrada.
  APPEND __seqta_tab.

* Lock assigned:
  PERFORM send_enqueue(saplsena)
          TABLES __seqta_tab
          USING '2' __scope ' ' __synchron 'EZDCO_NF_ENTRADA' _collect.

ENDFUNCTION.
