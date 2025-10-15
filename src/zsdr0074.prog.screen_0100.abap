PROCESS BEFORE OUTPUT.
  MODULE: pbo_0100,
          create_fcat,
          popula_obj.

PROCESS AFTER INPUT.

  CHAIN.
    FIELD it_new-instrucao.
    MODULE get_0045 ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-zmeng.
    FIELD it_new-volum.
    MODULE check_zmeng ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-instrucao.
*    FIELD IT_NEW-ZMENG.
    FIELD it_new-zieme.
    FIELD it_new-waerk.
    FIELD it_new-matnr.
    FIELD it_new-volum.
    FIELD it_new-voleh.
    FIELD it_new-libra_to.
    FIELD it_new-werks.
    FIELD it_new-dmbtr.
    FIELD it_new-pmein.
    FIELD it_new-usd_to.
    FIELD it_new-ponto_c.
    FIELD it_new-vlrtot.
    FIELD it_new-vlr_tot_frm_usd.
    FIELD it_new-terminal.
    FIELD it_new-charg.
    FIELD it_new-classificacao.
    FIELD it_new-lentrega.
    FIELD it_new-dco.
    FIELD it_new-vbeln.
    FIELD it_new-kunnr.
    FIELD it_new-lgort.
    FIELD it_new-inco1.
    FIELD it_new-inco2.
    FIELD it_new-aviso.
    FIELD it_new-auart.
    MODULE check_obrigatorio ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE pai_0100.

  "  PROCESS ON VALUE-REQUEST.
  " FIELD IT_NEW-INSTRUCAO MODULE F4_INSTRUCAO.
