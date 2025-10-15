PROCESS BEFORE OUTPUT.
  MODULE modify_screen_0103.

PROCESS AFTER INPUT.

  CHAIN.
    FIELD wg_header-tpsim.
    MODULE clear ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD wg_header-kunnr.
    FIELD wg_header-vkbur MODULE refresh_vkbur ON CHAIN-REQUEST.
    FIELD wg_header-waerk.
    FIELD wg_header-cultura.
    FIELD wg_header-tpcult.
    FIELD wg_header-tpsim.
    MODULE atualiza_dados ON CHAIN-REQUEST.
  ENDCHAIN.
  CHAIN.
    FIELD wg_header-prec_ant_cult.
    FIELD wg_header-vlr_adto.
    FIELD wg_header-adto_ha.      " Sara Oikawa - 38859 - Agosto/2020
    FIELD wg_header-antec.
    FIELD wg_header-waerk.
    FIELD wg_header-safra.
    FIELD wg_header-vkbur.
    FIELD wg_header-cultura.
    FIELD wg_header-juros_ano.
    FIELD wg_header-dtpgtcult.
    FIELD wg_header-dtinijuros.
    MODULE refresh_calcu ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD wg_header-tpsim.
    FIELD wg_header-vkbur.
* Início - Sara Oikawa - 38859 - Agosto/2020
*    FIELD WG_HEADER-MEIO_PAGO.
    FIELD wg_meio_pago.
* Fim - Sara Oikawa - 38859 - Agosto/2020
    MODULE busca_antecipacao ON CHAIN-REQUEST.
  ENDCHAIN.

PROCESS ON VALUE-REQUEST.
  FIELD wg_header-doc_simulacao MODULE search_doc.
  FIELD wg_header-vendedor MODULE search_vendor.
  FIELD wg_header-safra MODULE search_safra.
  FIELD wg_header-hbkid MODULE search_banco.
